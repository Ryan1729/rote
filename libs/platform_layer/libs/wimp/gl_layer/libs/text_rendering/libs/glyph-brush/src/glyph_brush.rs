mod builder;

pub use self::builder::*;

use super::*;
use full_rusttype::gpu_cache::{Cache, CachedBy};
use log::error;
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    borrow::Cow,
    fmt,
    hash::{BuildHasher, Hash, Hasher},
    i32, mem,
};

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
pub struct GlyphBrush<'font, V, H = DefaultSectionHasher> {
    fonts: Vec<Font<'font>>,
    texture_cache: Cache<'font>,
    last_draw: LastDrawInfo,

    // cache of section-layout hash -> computed glyphs, this avoid repeated glyph computation
    // for identical layout/sections common to repeated frame rendering
    calculate_glyph_cache: FxHashMap<SectionHash, Glyphed<'font, V>>,

    last_frame_seq_id_sections: Vec<SectionHashDetail>,
    frame_seq_id_sections: Vec<SectionHashDetail>,

    // buffer of section-layout hashs (that must exist in the calculate_glyph_cache)
    // to be used on the next `process_queued` call
    section_buffer: Vec<SectionHash>,

    // Set of section hashs to keep in the glyph cache this frame even if they haven't been drawn
    keep_in_cache: FxHashSet<SectionHash>,

    // config
    cache_glyph_positioning: bool,
    cache_glyph_drawing: bool,

    section_hasher: H,

    last_pre_positioned: Vec<Glyphed<'font, V>>,
    pre_positioned: Vec<Glyphed<'font, V>>,
}

impl<V, H> fmt::Debug for GlyphBrush<'_, V, H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GlyphBrush")
    }
}

impl<'font, V, H> GlyphCruncher<'font> for GlyphBrush<'font, V, H>
where
    V: Clone + 'static,
    H: BuildHasher,
{
    fn pixel_bounds_custom_layout<'a, S, L>(
        &mut self,
        section: S,
        custom_layout: &L,
    ) -> Option<Rect<i32>>
    where
        L: GlyphPositioner + Hash,
        S: Into<Cow<'a, VariedSection<'a>>>,
    {
        let section_hash = self.cache_glyphs(&section.into(), custom_layout);
        self.keep_in_cache.insert(section_hash);
        self.calculate_glyph_cache[&section_hash]
            .positioned
            .pixel_bounds()
    }

    fn glyphs_custom_layout<'a, 'b, S, L>(
        &'b mut self,
        section: S,
        custom_layout: &L,
    ) -> PositionedGlyphIter<'b, 'font>
    where
        L: GlyphPositioner + Hash,
        S: Into<Cow<'a, VariedSection<'a>>>,
    {
        let section_hash = self.cache_glyphs(&section.into(), custom_layout);
        self.keep_in_cache.insert(section_hash);
        self.calculate_glyph_cache[&section_hash]
            .positioned
            .glyphs()
    }

    fn fonts(&self) -> &[Font<'font>] {
        &self.fonts
    }
}

#[derive(Clone, Default, Debug)]
pub struct RectSpec {
    pub pixel_coords: PixelCoords,
    pub bounds: Bounds,
    pub color: Color,
    pub z: f32,
}

impl Hash for RectSpec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pixel_coords.hash(state);
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

impl<'font, V, H> GlyphBrush<'font, V, H>
where
    V: Clone + 'static,
    H: BuildHasher,
{
    /// Queues a section/layout to be processed by the next call of
    /// [`process_queued`](struct.GlyphBrush.html#method.process_queued). Can be called multiple
    /// times to queue multiple sections for drawing.
    ///
    /// Used to provide custom `GlyphPositioner` logic, if using built-in
    /// [`Layout`](enum.Layout.html) simply use [`queue`](struct.GlyphBrush.html#method.queue)
    ///
    /// Benefits from caching, see [caching behaviour](#caching-behaviour).
    pub fn queue_custom_layout<'a, S, G>(&mut self, section: S, custom_layout: &G)
    where
        G: GlyphPositioner,
        S: Into<Cow<'a, VariedSection<'a>>>,
    {
        let section = section.into();
        if cfg!(debug_assertions) {
            for text in &section.text {
                assert!(self.fonts.len() > text.font_id.0, "Invalid font id");
            }
        }
        let section_hash = self.cache_glyphs(&section, custom_layout);
        self.section_buffer.push(section_hash);
        self.keep_in_cache.insert(section_hash);
    }

    /// Queues a section/layout to be processed by the next call of
    /// [`process_queued`](struct.GlyphBrush.html#method.process_queued). Can be called multiple
    /// times to queue multiple sections for drawing.
    ///
    /// Benefits from caching, see [caching behaviour](#caching-behaviour).
    ///
    pub fn queue<'a, S>(&mut self, section: S)
    where
        S: Into<Cow<'a, VariedSection<'a>>>,
    {
        let section = section.into();
        let layout = section.layout;
        self.queue_custom_layout(section, &layout)
    }

    /// Queues pre-positioned glyphs to be processed by the next call of
    /// [`process_queued`](struct.GlyphBrush.html#method.process_queued). Can be called multiple
    /// times.
    pub fn queue_pre_positioned(
        &mut self,
        glyphs: Vec<(PositionedGlyph<'font>, Color, FontId)>,
        bounds: Rect<f32>,
        z: f32,
    ) {
        self.pre_positioned
            .push(Glyphed::new(GlyphedSection { glyphs, bounds, z }));
    }

    /// Returns the calculate_glyph_cache key for this sections glyphs
    #[allow(clippy::map_entry)] // further borrows are required after the contains_key check
    fn cache_glyphs<L>(&mut self, section: &VariedSection<'_>, layout: &L) -> SectionHash
    where
        L: GlyphPositioner,
    {
        let section_hash = SectionHashDetail::new(&self.section_hasher, section, layout);
        // section id used to find a similar calculated layout from last frame
        let frame_seq_id = self.frame_seq_id_sections.len();
        self.frame_seq_id_sections.push(section_hash);

        if self.cache_glyph_positioning {
            if !self.calculate_glyph_cache.contains_key(&section_hash.full) {
                let geometry = SectionGeometry::from(section);

                let recalculated_glyphs = self
                    .last_frame_seq_id_sections
                    .get(frame_seq_id)
                    .cloned()
                    .and_then(|hash| {
                        let change = hash.diff(section_hash);
                        if let GlyphChange::Unknown = change {
                            return None;
                        }

                        let old_glyphs = if self.keep_in_cache.contains(&hash.full) {
                            let cached = self.calculate_glyph_cache.get(&hash.full)?;
                            Cow::Borrowed(&cached.positioned.glyphs)
                        } else {
                            let old = self.calculate_glyph_cache.remove(&hash.full)?;
                            Cow::Owned(old.positioned.glyphs)
                        };

                        Some(layout.recalculate_glyphs(
                            old_glyphs,
                            change,
                            &self.fonts,
                            &geometry,
                            &section.text,
                        ))
                    });

                self.calculate_glyph_cache.insert(
                    section_hash.full,
                    Glyphed::new(GlyphedSection {
                        bounds: layout.bounds_rect(&geometry),
                        glyphs: recalculated_glyphs.unwrap_or_else(|| {
                            layout.calculate_glyphs(&self.fonts, &geometry, &section.text)
                        }),
                        z: section.z,
                    }),
                );
            }
        } else {
            let geometry = SectionGeometry::from(section);
            self.calculate_glyph_cache.insert(
                section_hash.full,
                Glyphed::new(GlyphedSection {
                    bounds: layout.bounds_rect(&geometry),
                    glyphs: layout.calculate_glyphs(&self.fonts, &geometry, &section.text),
                    z: section.z,
                }),
            );
        }
        section_hash.full
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
        F1: FnMut(Rect<u32>, &[u8]),
        F2: Fn(GlyphVertex) -> V + Copy,
    {
        let draw_info = LastDrawInfo {
            text_state: {
                perf_viz::record_guard!("text_state");
                let mut s = self.section_hasher.build_hasher();
                let s_ref = &mut s;
                self.section_buffer.hash(s_ref);
                if let Some(a_r) = additional_rects.as_ref() {
                    a_r.rect_specs.hash(s_ref);
                }

                s.finish()
            },
        };

        let result = if !self.cache_glyph_drawing
            || self.last_draw != draw_info
            || self.last_pre_positioned != self.pre_positioned
        {
            perf_viz::record_guard!("prepare BrushAction::Draw");
            let mut some_text = false;
            // Everything in the section_buffer should also be here. The extras should also
            // be retained in the texture cache avoiding cache thrashing if they are rendered
            // in a 2-draw per frame style.
            for section_hash in &self.keep_in_cache {
                for &(ref glyph, _, font_id) in self
                    .calculate_glyph_cache
                    .get(section_hash)
                    .iter()
                    .flat_map(|gs| &gs.positioned.glyphs)
                {
                    self.texture_cache.queue_glyph(font_id.0, glyph.clone());
                    some_text = true;
                }
            }

            for &(ref glyph, _, font_id) in self
                .pre_positioned
                .iter()
                .flat_map(|p| &p.positioned.glyphs)
            {
                self.texture_cache.queue_glyph(font_id.0, glyph.clone());
                some_text = true;
            }

            if some_text {
                match self.texture_cache.cache_queued(update_texture) {
                    Ok(CachedBy::Adding) => {}
                    Ok(CachedBy::Reordering) => {
                        for glyphed in self.calculate_glyph_cache.values_mut() {
                            glyphed.invalidate_texture_positions();
                        }
                    }
                    Err(_) => {
                        let (width, height) = self.texture_cache.dimensions();
                        return Err(BrushError::TextureTooSmall {
                            suggested: (width * 2, height * 2),
                        });
                    }
                }
            }

            self.last_draw = draw_info;

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
        self.texture_cache
            .to_builder()
            .dimensions(new_width, new_height)
            .rebuild(&mut self.texture_cache);

        self.last_draw = LastDrawInfo::default();

        // invalidate any previous cache position data
        for glyphed in self.calculate_glyph_cache.values_mut() {
            glyphed.invalidate_texture_positions();
        }
    }

    /// Returns the logical texture cache pixel dimensions `(width, height)`.
    pub fn texture_dimensions(&self) -> (u32, u32) {
        self.texture_cache.dimensions()
    }

    fn cleanup_frame(&mut self) {
        if self.cache_glyph_positioning {
            // clear section_buffer & trim calculate_glyph_cache to active sections
            let active = mem::take(&mut self.keep_in_cache);
            self.calculate_glyph_cache
                .retain(|key, _| active.contains(key));
            mem::replace(&mut self.keep_in_cache, active);

            self.keep_in_cache.clear();

            self.section_buffer.clear();
        } else {
            self.section_buffer.clear();
            self.calculate_glyph_cache.clear();
            self.keep_in_cache.clear();
        }

        mem::swap(
            &mut self.last_frame_seq_id_sections,
            &mut self.frame_seq_id_sections,
        );
        self.frame_seq_id_sections.clear();

        mem::swap(&mut self.last_pre_positioned, &mut self.pre_positioned);
        self.pre_positioned.clear();
    }

    /// Adds an additional font to the one(s) initially added on build.
    ///
    /// Returns a new [`FontId`](struct.FontId.html) to reference this font.
    pub fn add_font_bytes<'a: 'font, B: Into<SharedBytes<'a>>>(&mut self, font_data: B) -> FontId {
        self.add_font(Font::from_bytes(font_data.into()).unwrap())
    }

    /// Adds an additional font to the one(s) initially added on build.
    ///
    /// Returns a new [`FontId`](struct.FontId.html) to reference this font.
    pub fn add_font<'a: 'font>(&mut self, font_data: Font<'a>) -> FontId {
        self.fonts.push(font_data);
        FontId(self.fonts.len() - 1)
    }

    /// Retains the section in the cache as if it had been used in the last draw-frame.
    ///
    /// Should not generally be necessary, see [caching behaviour](#caching-behaviour).
    pub fn keep_cached_custom_layout<'a, S, G>(&mut self, section: S, custom_layout: &G)
    where
        S: Into<Cow<'a, VariedSection<'a>>>,
        G: GlyphPositioner,
    {
        if !self.cache_glyph_positioning {
            return;
        }
        let section = section.into();
        if cfg!(debug_assertions) {
            for text in &section.text {
                assert!(self.fonts.len() > text.font_id.0, "Invalid font id");
            }
        }

        let section_hash = SectionHashDetail::new(&self.section_hasher, &section, custom_layout);
        self.keep_in_cache.insert(section_hash.full);
    }

    /// Retains the section in the cache as if it had been used in the last draw-frame.
    ///
    /// Should not generally be necessary, see [caching behaviour](#caching-behaviour).
    pub fn keep_cached<'a, S>(&mut self, section: S)
    where
        S: Into<Cow<'a, VariedSection<'a>>>,
    {
        let section = section.into();
        let layout = section.layout;
        self.keep_cached_custom_layout(section, &layout);
    }
}

impl<'font, V, H: BuildHasher + Clone> GlyphBrush<'font, V, H> {
    /// Return a [`GlyphBrushBuilder`](struct.GlyphBrushBuilder.html) prefilled with the
    /// properties of this `GlyphBrush`.
    pub fn to_builder(&self) -> GlyphBrushBuilder<'font, H> {
        let mut builder = GlyphBrushBuilder::using_fonts(self.fonts.clone())
            .cache_glyph_positioning(self.cache_glyph_positioning)
            .cache_glyph_drawing(self.cache_glyph_drawing)
            .section_hasher(self.section_hasher.clone());
        builder.gpu_cache_builder = self.texture_cache.to_builder();
        builder
    }
}

#[derive(Debug, Default, PartialEq)]
struct LastDrawInfo {
    text_state: u64,
}

pub type TexCoords = Rect<f32>;
pub type PixelCoords = Rect<i32>;
pub type Bounds = Rect<f32>;

/// Data used to generate vertex information for a single glyph
#[derive(Debug, Default)]
pub struct GlyphVertex {
    pub tex_coords: TexCoords,
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
    test_alpha_color: SectionHash,
    /// hash of text  + color + alpha + geo + z
    full: SectionHash,

    /// copy of geometry for later comparison
    geometry: SectionGeometry,
}

impl SectionHashDetail {
    #[inline]
    fn new<H, L>(build_hasher: &H, section: &VariedSection<'_>, layout: &L) -> Self
    where
        H: BuildHasher,
        L: GlyphPositioner,
    {
        let parts = section.to_hashable_parts();

        let mut s = build_hasher.build_hasher();
        layout.hash(&mut s);
        parts.hash_text_no_color(&mut s);
        let text_hash = s.finish();

        parts.hash_color(&mut s);
        let text_color_hash = s.finish();

        parts.hash_alpha(&mut s);
        let test_alpha_color_hash = s.finish();

        parts.hash_geometry(&mut s);
        parts.hash_z(&mut s);
        let full_hash = s.finish();

        Self {
            text: text_hash,
            text_color: text_color_hash,
            test_alpha_color: test_alpha_color_hash,
            full: full_hash,
            geometry: SectionGeometry::from(section),
        }
    }

    /// They're different, but how?
    fn diff(self, other: SectionHashDetail) -> GlyphChange {
        if self.test_alpha_color == other.test_alpha_color {
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

/// Container for positioned glyphs which can generate and cache vertices
struct Glyphed<'font, V> {
    positioned: GlyphedSection<'font>,
    vertices: Vec<V>,
}

impl<V> PartialEq for Glyphed<'_, V> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.positioned == other.positioned
    }
}

impl<'font, V> Glyphed<'font, V> {
    #[inline]
    fn new(gs: GlyphedSection<'font>) -> Self {
        Self {
            positioned: gs,
            vertices: Vec::new(),
        }
    }

    /// Mark previous texture positions as no longer valid (vertices require re-generation)
    fn invalidate_texture_positions(&mut self) {
        self.vertices.clear();
    }

    /// Calculate vertices if not already done
    fn ensure_vertices<F>(&mut self, texture_cache: &Cache<'font>, to_vertex: F)
    where
        F: Fn(GlyphVertex) -> V,
    {
        if !self.vertices.is_empty() {
            return;
        }

        let GlyphedSection {
            bounds,
            z,
            ref glyphs,
        } = self.positioned;

        self.vertices.reserve(glyphs.len());
        perf_viz::record_guard!("ensure_vertices extend");
        self.vertices
            .extend(glyphs.iter().filter_map(|(glyph, color, font_id)| {
                match texture_cache.rect_for(font_id.0, glyph) {
                    Err(err) => {
                        error!("Cache miss?: {:?}, {:?}: {}", font_id, glyph, err);
                        None
                    }
                    Ok(None) => {
                        None
                    },
                    Ok(Some((tex_coords, pixel_coords))) => {
                        let is_outside = pixel_coords.min.x as f32 > bounds.max.x
                            || pixel_coords.min.y as f32 > bounds.max.y
                            || bounds.min.x > pixel_coords.max.x as f32
                            || bounds.min.y > pixel_coords.max.y as f32;
                        if is_outside
                        {
                            // glyph is totally outside the bounds
                            None
                        } else {
                            Some(to_vertex(GlyphVertex {
                                tex_coords,
                                pixel_coords,
                                bounds,
                                color: *color,
                                z,
                            }))
                        }
                    }
                }
            }));
    }
}