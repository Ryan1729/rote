#![deny(unused)]
use rasterizer::{Cache, CachedBy};
use std::{
    borrow::Cow,
    fmt,
    hash::{Hash, Hasher},
    mem,
};

mod owned_section;
mod section;

pub use rasterizer::{
    Font, GlyphId, Point, Glyph,
    Rect, Scale,
    point,
    new_cache,
    new_glyph,
    get_line_height,
    get_advance_width,
    add_position,
    dimensions,
    intersects,
};

use rasterizer::{
    queue_glyph,
    rect_for,
};

pub use crate::{
    owned_section::*,
    section::*,
};

/// Id for a font
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct FontId(pub(crate) usize);

pub type CalculatedGlyph<'font> = (Glyph<'font>, Color);

/// Logic to calculate glyph positioning using [`Font`](struct.Font.html),
/// [`SectionGeometry`](struct.SectionGeometry.html) and
/// [`SectionText`](struct.SectionText.html).
pub trait GlyphPositioner: Hash {
    /// Calculate a sequence of positioned glyphs to render. Implementations should
    /// return the same result when called with the same arguments to allow layout caching.
    fn calculate_glyphs<'font>(
        &self,
        font: &Font<'font>,
        scale: Scale,
        geometry: &SectionGeometry,
        sections: &[SectionText<'_>],
    ) -> Vec<CalculatedGlyph<'font>>;
}


/// A hash of `Section` data
type SectionHash = u64;

/// Object allowing glyph drawing, containing cache state. Manages glyph positioning cacheing,
/// glyph draw caching & efficient GPU texture cache updating.
///
/// Build using a [`GlyphBrushBuilder`](struct.GlyphBrushBuilder.html).
///
/// Also see [`GlyphCruncher`](trait.GlyphCruncher.html) trait which providers extra functionality,
/// such as [`glyph_bounds`](trait.GlyphCruncher.html#method.glyph_bounds).
///
/// # Caching behaviour
///
/// Calls to [`GlyphBrush::queue`](#method.queue),
/// [`GlyphBrush::pixel_bounds`](#method.pixel_bounds), [`GlyphBrush::glyphs`](#method.glyphs)
/// calculate the positioned glyphs for a section.
/// This is cached so future calls to any of the methods for the same section are much
/// cheaper. In the case of [`GlyphBrush::queue`](#method.queue) the calculations will also be
/// used for actual drawing.
///
/// The cache for a section will be **cleared** after a
/// [`GlyphBrush::process_queued`](#method.process_queued) call when that section has not been used
/// since the previous call.
///
/// # Texture caching behaviour
/// Note the gpu/draw cache may contain multiple versions of the same glyph at different
/// subpixel positions.
/// This is required for high quality text as a glyph's positioning is always exactly aligned
/// to it's draw positioning.
///
/// This behaviour can be adjusted with
/// [`GlyphBrushBuilder::gpu_cache_position_tolerance`]
/// (struct.GlyphBrushBuilder.html#method.gpu_cache_position_tolerance).
pub struct GlyphBrush<'font, V> {
    fonts: Vec<Font<'font>>,
    texture_cache: Cache<'font>,
    text_hash: u64,

    // cache of section-layout hash -> computed glyphs, this avoid repeated glyph computation
    // for identical layout/sections common to repeated frame rendering
    calculate_glyph_cache: fast_hash::Map<SectionHash, Glyphed<'font, V>>,

    last_frame_seq_id_sections: Vec<SectionHashDetail>,
    frame_seq_id_sections: Vec<SectionHashDetail>,

    // buffer of section-layout hashes (that must exist in the calculate_glyph_cache)
    // to be used on the next `process_queued` call
    section_buffer: Vec<SectionHash>,

    // Set of section hashes to keep in the glyph cache this frame even if they haven't been drawn
    keep_in_cache: fast_hash::Set<SectionHash>,

    last_pre_positioned: Vec<Glyphed<'font, V>>,
    pre_positioned: Vec<Glyphed<'font, V>>,
}

impl <'font, V> GlyphBrush<'font, V>
where
    V: Clone + 'static,
{
    pub fn using_font(font_0: Font<'font>) -> Self {
        Self::using_fonts(vec![font_0])
    }

    fn using_fonts<Fonts: Into<Vec<Font<'font>>>>(fonts: Fonts) -> Self {
        GlyphBrush {
            fonts: fonts.into(),
            texture_cache: new_cache::<'font>(),
            text_hash: <_>::default(),
            section_buffer: <_>::default(),
            calculate_glyph_cache: <_>::default(),

            last_frame_seq_id_sections: <_>::default(),
            frame_seq_id_sections: <_>::default(),

            keep_in_cache: <_>::default(),

            last_pre_positioned: <_>::default(),
            pre_positioned: <_>::default(),
        }
    }

    /// Mark previous texture positions as no longer valid 
    /// (vertices require re-generation)
    fn invalidate_texture_positions(&mut self) {
        for glyphed in self.calculate_glyph_cache.values_mut() {
            glyphed.vertices.clear();
        }
    }
}

pub type CalculatedGlyphIter<'a, 'font> = std::iter::Map<
    std::slice::Iter<'a, CalculatedGlyph<'font>>,
    fn(
        &'a CalculatedGlyph<'font>,
    ) -> &'a rasterizer::Glyph<'font>,
>;

#[derive(Clone, Default, Debug)]
pub struct RectSpec {
    pub pixel_coords: PixelCoords,
    pub bounds: Bounds,
    pub color: Color,
    pub z: f32,
}

impl Hash for RectSpec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pixel_coords.min.x.to_bits().hash(state);
        self.pixel_coords.min.y.to_bits().hash(state);
        self.pixel_coords.max.x.to_bits().hash(state);
        self.pixel_coords.max.y.to_bits().hash(state);
        self.bounds.min.x.to_bits().hash(state);
        self.bounds.min.y.to_bits().hash(state);
        self.bounds.max.x.to_bits().hash(state);
        self.bounds.max.y.to_bits().hash(state);
        self.color[0].to_bits().hash(state);
        self.color[0].to_bits().hash(state);
        self.color[1].to_bits().hash(state);
        self.color[2].to_bits().hash(state);
        self.z.to_bits().hash(state);
    }
}

pub struct AdditionalRects<V: Clone + 'static> {
    pub set_alpha: fn(&mut V, alpha: f32),
    pub rect_specs: Vec<RectSpec>,
}

#[derive(Debug)]
enum GlyphChange {
    /// Only the geometry has changed, contains the old geometry
    Geometry(SectionGeometry),
    /// Only the colors have changed (including alpha)
    Color,
    /// Only the alpha has changed
    Alpha,
    Unknown,
}

fn recalculate_glyphs<'font, G>(
    positioner: &G,
    previous: Cow<Vec<CalculatedGlyph<'font>>>,
    change: GlyphChange,
    font: &Font<'font>,
    scale: Scale,
    geometry: &SectionGeometry,
    sections: &[SectionText],
) -> Vec<CalculatedGlyph<'font>>
where
        G: GlyphPositioner,
{
    match change {
        GlyphChange::Geometry(old) if old.bounds == geometry.bounds => {
            // position change
            let adjustment = point(
                geometry.screen_position.0 - old.screen_position.0,
                geometry.screen_position.1 - old.screen_position.1,
            );

            let mut glyphs = previous.into_owned();
            for (glyph, ..) in &mut glyphs {
                let pos = glyph.position();
                glyph.set_position(point(
                    pos.x + adjustment.x,
                    pos.y + adjustment.y,
                ));
            }

            glyphs
        }
        GlyphChange::Color if !sections.is_empty() && !previous.is_empty() => {
            let new_color = sections[0].color;
            // even if the colour changed only slightly, we still want to do a fresh calculation
            #[allow(clippy::float_cmp)]
            if sections.iter().all(|s| s.color == new_color) {
                // if only the color changed, but the new section only use a single color
                // we can simply set all the olds to the new color
                let mut glyphs = previous.into_owned();
                for (_, color, ..) in &mut glyphs {
                    *color = new_color;
                }
                glyphs
            } else {
                positioner.calculate_glyphs(font, scale, geometry, sections)
            }
        }
        GlyphChange::Alpha if !sections.is_empty() && !previous.is_empty() => {
            let new_alpha = sections[0].color[3];
            // even if the alpha changed only slightly, we still want to do a fresh calculation
            #[allow(clippy::float_cmp)]
            if sections.iter().all(|s| s.color[3] == new_alpha) {
                // if only the alpha changed, but the new section only uses a single alpha
                // we can simply set all the olds to the new alpha
                let mut glyphs = previous.into_owned();
                for (_, color, ..) in &mut glyphs {
                    color[3] = new_alpha;
                }
                glyphs
            } else {
                positioner.calculate_glyphs(font, scale, geometry, sections)
            }
        }
        _ => positioner.calculate_glyphs(font, scale, geometry, sections),
    }
}

impl<'font, V> GlyphBrush<'font, V>
where
    V: Clone + 'static,
{
    /// Queues a section/layout to be processed by the next call of
    /// [`process_queued`](struct.GlyphBrush.html#method.process_queued). Can be called multiple
    /// times to queue multiple sections for drawing.
    ///
    /// Used to provide custom `GlyphPositioner` logic, if using built-in
    /// [`Layout`](enum.Layout.html) simply use [`queue`](struct.GlyphBrush.html#method.queue)
    ///
    /// Benefits from caching, see [caching behaviour](#caching-behaviour).
    pub fn queue_layout<'a, S, G>(&mut self, section: S, layout: &G)
    where
        G: GlyphPositioner,
        S: Into<Cow<'a, VariedSection<'a>>>,
    {
        // further borrows are required after the contains_key check
        #![allow(clippy::map_entry)]

        let section = section.into();
        let section_ref: &VariedSection<'a> = &section;
        debug_assert!(self.fonts.len() > section.font_id.0, "Invalid font id");
        
        let section_hash = SectionHashDetail::new(section_ref, layout);
        // section id used to find a similar calculated layout from last frame
        let frame_seq_id = self.frame_seq_id_sections.len();
        self.frame_seq_id_sections.push(section_hash);

        let scale = section.scale;
        let font_id = section.font_id;
        
        if !self.calculate_glyph_cache.contains_key(&section_hash.full) {
            let geometry = SectionGeometry::from(section_ref);

            let glyphs = self
                .last_frame_seq_id_sections
                .get(frame_seq_id)
                .cloned()
                .and_then(|hash| {
                    let font = &self.fonts[font_id.0];
                    let change = hash.diff(section_hash);
                    if let GlyphChange::Unknown = change {
                        return None;
                    }

                    let old_glyphs = if self.keep_in_cache.contains(&hash.full) {
                        let cached = self.calculate_glyph_cache.get(&hash.full)?;
                        Cow::Borrowed(&cached.glyphs)
                    } else {
                        let old = self.calculate_glyph_cache.remove(&hash.full)?;
                        Cow::Owned(old.glyphs)
                    };

                    Some(recalculate_glyphs(
                        layout,
                        old_glyphs,
                        change,
                        font,
                        scale,
                        &geometry,
                        &section.text,
                    ))
                })
                .unwrap_or_else(|| {
                    let font = &self.fonts[font_id.0];
                    layout.calculate_glyphs(
                        font,
                        scale,
                        &geometry,
                        &section.text
                    )
                });

            
            self.calculate_glyph_cache.insert(
                section_hash.full,
                Glyphed {
                    glyphs,
                    z: section.z,
                    font_id,
                    vertices: Vec::new(),
                },
            );
        }

        self.section_buffer.push(section_hash.full);
        self.keep_in_cache.insert(section_hash.full);
    }

    /// Processes all queued sections, calling texture update logic when necessary &
    /// returning a `BrushAction`.
    /// See [`queue`](struct.GlyphBrush.html#method.queue).
    ///
    /// Two closures are required:
    /// * `update_texture` is called when new glyph texture data has been drawn for update in the
    ///   actual texture.
    ///   The arguments are the rect position of the data in the texture & the byte data itself
    ///   which is a single `u8` alpha value per pixel.
    /// * `to_vertex` maps a single glyph's `GlyphVertex` data into a generic vertex type. The
    ///   mapped vertices are returned in an `Ok(BrushAction::Draw(vertices))` result.
    ///   It's recommended to use a single vertex per glyph quad for best performance.
    ///
    /// Trims the cache, see [caching behaviour](#caching-behaviour).
    ///
    #[perf_viz::record]
    pub fn process_queued<F1, F2>(
        &mut self,
        update_texture: F1,
        to_vertex: F2,
        additional_rects: Option<AdditionalRects<V>>,
    ) -> Result<BrushAction<V>, BrushError>
    where
        F1: FnMut(TextureRect, &[u8]),
        F2: Fn(GlyphVertex) -> V + Copy,
    {
        let text_hash = {
            perf_viz::record_guard!("text_hash");
            let mut hasher = fast_hash::Hasher::default();
            self.section_buffer.hash(&mut hasher);
            if let Some(a_r) = additional_rects.as_ref() {
                a_r.rect_specs.hash(&mut hasher);
            }

            hasher.finish()
        };

        let result = if self.text_hash != text_hash
            || self.last_pre_positioned != self.pre_positioned
        {
            perf_viz::record_guard!("prepare BrushAction::Draw");
            let mut some_text = false;
            // Everything in the section_buffer should also be here. The extras should also
            // be retained in the texture cache avoiding cache thrashing if they are rendered
            // in a 2-draw per frame style.
            for section_hash in &self.keep_in_cache {
                for positioned in self
                    .calculate_glyph_cache
                    .get(section_hash)
                    .iter()
                {
                    let font_id = positioned.font_id;
                    for &(ref glyph, _) in positioned.glyphs.iter() {
                        queue_glyph(
                            &mut self.texture_cache,
                            font_id.0,
                            glyph.clone(),
                        );
                        some_text = true;
                    }
                }
            }

            for positioned in self
                .pre_positioned
                .iter()
            {
                let font_id = positioned.font_id;
                for &(ref glyph, _) in positioned.glyphs.iter() {
                    queue_glyph(&mut self.texture_cache, font_id.0, glyph.clone());
                    some_text = true;
                }
            }

            if some_text {
                match rasterizer::cache_queued(
                    &mut self.texture_cache,
                    &self.fonts,
                    update_texture,
                ) {
                    Ok(CachedBy::Adding) => {}
                    Ok(CachedBy::Reordering) => {
                        self.invalidate_texture_positions();
                    }
                    Err(_) => {
                        let (width, height) = dimensions(&self.texture_cache);
                        return Err(BrushError::TextureTooSmall {
                            suggested: (width * 2, height * 2),
                        });
                    }
                }
            }

            self.text_hash = text_hash;

            BrushAction::Draw({
                perf_viz::record_guard!("verts BrushAction::Draw");
                let mut verts = Vec::new();

                for hash in &self.section_buffer {
                    perf_viz::start_record!("hash in &self.section_buffer");
                    let glyphed = self.calculate_glyph_cache.get_mut(hash).unwrap();
                    perf_viz::start_record!("section_buffer ensure_vertices");
                    glyphed.ensure_vertices(&self.texture_cache, to_vertex);
                    perf_viz::end_record!("section_buffer ensure_vertices");
                    verts.extend(glyphed.vertices.iter().cloned());
                    perf_viz::end_record!("hash in &self.section_buffer");
                }

                for glyphed in &mut self.pre_positioned {
                    perf_viz::start_record!("glyphed in &mut self.pre_positioned");
                    // pre-positioned glyph vertices can't be cached so
                    // generate & move straight into draw vec
                    glyphed.ensure_vertices(&self.texture_cache, to_vertex);
                    verts.append(&mut glyphed.vertices);
                    perf_viz::end_record!("glyphed in &mut self.pre_positioned");
                }

                // This stuff was hacked in here to fix a perf issue arising from
                // the previous method of drawing rects through glyph_brush's API
                // that used multiple instances of "█"
                // (What would make these feel less hacked in?)
                if let Some(AdditionalRects {
                     set_alpha,
                     rect_specs,
                 }) = additional_rects
                {
                    perf_viz::record_guard!("rect_specs loop");
                    for range in rect_specs {
                        let RectSpec {
                            pixel_coords,
                            bounds,
                            color,
                            z,
                        } = range;

                        let mut v = to_vertex(GlyphVertex {
                            pixel_coords,
                            bounds,
                            color,
                            z,
                            ..Default::default()
                        });

                        set_alpha(&mut v, color[3]);

                        verts.push(v);
                    }
                }

                verts
            })
        } else {
            BrushAction::ReDraw
        };

        perf_viz::record_guard!("cleanup_frame");
        self.cleanup_frame();
        Ok(result)
    }

    /// Rebuilds the logical texture cache with new dimensions. Should be avoided if possible.
    ///
    /// # Example
    ///
    pub fn resize_texture(&mut self, new_width: u32, new_height: u32) {
        rasterizer::resize_texture(
            &mut self.texture_cache,
            new_width,
            new_height,
        );

        self.text_hash = <_>::default();

        self.invalidate_texture_positions();
    }

    /// Returns the logical texture cache pixel dimensions `(width, height)`.
    pub fn texture_dimensions(&self) -> (u32, u32) {
        dimensions(&self.texture_cache)
    }

    fn cleanup_frame(&mut self) {
        // clear section_buffer & trim calculate_glyph_cache to active sections
        {
            let keep_in_cache = &self.keep_in_cache;
            self.calculate_glyph_cache
                .retain(|key, _| keep_in_cache.contains(key));
        }

        self.keep_in_cache.clear();

        self.section_buffer.clear();

        mem::swap(
            &mut self.last_frame_seq_id_sections,
            &mut self.frame_seq_id_sections,
        );
        self.frame_seq_id_sections.clear();

        mem::swap(&mut self.last_pre_positioned, &mut self.pre_positioned);
        self.pre_positioned.clear();
    }
}

pub type TextureCoords = rasterizer::TextureCoords;
pub type PixelCoords = rasterizer::PixelCoords;
pub type TextureRect = rasterizer::TextureRect;

/// A Bounds struct specifies floating point coordinates on the screen, inside
/// of which the text should be rendered, and outside of which the text
/// should not be.
pub type Bounds = Rect;

/// Data used to generate vertex information for a single glyph
#[derive(Debug, Default)]
pub struct GlyphVertex {
    pub tex_coords: TextureCoords,
    pub pixel_coords: PixelCoords,
    pub bounds: Bounds,
    pub color: Color,
    pub z: f32,
}

/// Actions that should be taken after processing queue data
#[derive(Debug)]
pub enum BrushAction<V> {
    /// Draw new/changed vertix data.
    Draw(Vec<V>),
    /// Re-draw last frame's vertices unmodified.
    ReDraw,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BrushError {
    /// Texture is too small to cache queued glyphs
    ///
    /// A larger suggested size is included.
    TextureTooSmall { suggested: (u32, u32) },
}
impl fmt::Display for BrushError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TextureTooSmall { .. } => write!(f, "TextureTooSmall"),
        }
    }
}
impl std::error::Error for BrushError {
    fn description(&self) -> &str {
        match self {
            BrushError::TextureTooSmall { .. } => "Texture is too small to cache queued glyphs",
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct SectionHashDetail {
    /// hash of text (- color - alpha - geo - z)
    text: SectionHash,
    // hash of text + color (- alpha - geo - z)
    text_color: SectionHash,
    /// hash of text + color + alpha (- geo - z)
    text_alpha_color: SectionHash,
    /// hash of text  + color + alpha + geo + z
    full: SectionHash,

    /// copy of geometry for later comparison
    geometry: SectionGeometry,
}

impl SectionHashDetail {
    #[inline]
    fn new<G>(section: &VariedSection<'_>, layout: &G) -> Self
    where
        G: GlyphPositioner,
    {
        let VariedSection {
            screen_position: (screen_x, screen_y),
            bounds: (bound_w, bound_h),
            z,
            text: section_texts,
            ..
        } = section;

        let mut hasher = fast_hash::Hasher::default();

        layout.hash(&mut hasher);

        for t in section_texts {
            let SectionText {
                text,
                ..
            } = *t;

            text.hash(&mut hasher);
        }

        let text = hasher.finish();

        for t in section_texts {
            let color = t.color;

            [
                color[0].to_bits(),
                color[1].to_bits(),
                color[2].to_bits()
            ].hash(&mut hasher);
        }
        let text_color = hasher.finish();

        for t in section_texts {
            t.color[3].to_bits().hash(&mut hasher);
        }
        let text_alpha_color = hasher.finish();

        [
            screen_x.to_bits(),
            screen_y.to_bits(),
            bound_w.to_bits(),
            bound_h.to_bits(),
        ].hash(&mut hasher);
        
        z.to_bits().hash(&mut hasher);
        let full = hasher.finish();

        Self {
            text,
            text_color,
            text_alpha_color,
            full,
            geometry: SectionGeometry::from(section),
        }
    }

    /// They're different, but how?
    fn diff(self, other: SectionHashDetail) -> GlyphChange {
        if self.text_alpha_color == other.text_alpha_color {
            return GlyphChange::Geometry(self.geometry);
        } else if self.geometry == other.geometry {
            if self.text_color == other.text_color {
                return GlyphChange::Alpha;
            } else if self.text == other.text {
                // color and alpha may have changed
                return GlyphChange::Color;
            }
        }
        GlyphChange::Unknown
    }
}

struct Glyphed<'font, V> {
    glyphs: Vec<CalculatedGlyph<'font>>,
    font_id: FontId,
    z: f32,
    vertices: Vec<V>,
}

impl<V> PartialEq for Glyphed<'_, V> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // We ignore the vertices on purpose since those are just a cache.
        self.z == other.z
        && self.font_id == other.font_id
            && self.glyphs.len() == other.glyphs.len()
            && self.glyphs.iter().zip(other.glyphs.iter()).all(|(l, r)| {
                    l.1 == r.1
                    && l.0.id() == r.0.id()
                    && l.0.position() == r.0.position()
                    && l.0.scale() == r.0.scale()
            })
    }
}

impl<'font, V> Glyphed<'font, V> {
    /// Calculate vertices if not already done
    fn ensure_vertices<F>(&mut self, texture_cache: &Cache<'font>, to_vertex: F)
    where
        F: Fn(GlyphVertex) -> V,
    {
        if !self.vertices.is_empty() {
            return;
        }

        let Self {
            z,
            font_id,
            ref glyphs,
            ..
        } = self;

        self.vertices.reserve(glyphs.len());
        perf_viz::record_guard!("ensure_vertices extend");
        self.vertices
            .extend(glyphs.iter().filter_map(|(glyph, color)| {
                match rect_for(texture_cache, font_id.0, glyph) {
                    Err(err) => {
                        eprintln!("Cache miss?: {:?}, {:?}: {}", font_id, glyph, err);
                        None
                    }
                    Ok(None) => {
                        None
                    },
                    Ok(Some(rasterizer::Coords{ texture, pixel })) => {
                        use std::f32::INFINITY;
                        const INFINITY_RECT: Rect = Rect {
                            min: Point {
                                x: -INFINITY,
                                y: -INFINITY,
                            },
                            max: Point {
                                x: INFINITY,
                                y: INFINITY,
                            },
                        };

                        Some(to_vertex(GlyphVertex {
                            tex_coords: texture,
                            pixel_coords: pixel,
                            bounds: INFINITY_RECT,
                            color: *color,
                            z: *z,
                        }))
                    }
                }
            }));
    }
}