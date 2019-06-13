// This file has been modified for use in the `rote ` project.
mod builder;

pub use self::builder::*;

use super::*;
use full_rusttype::gpu_cache::{Cache, CachedBy};
use log::error;
use macros::d;
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
pub struct GlyphBrush<'font, V: Clone + 'static, H = DefaultSectionHasher> {
    fonts: Vec<Font<'font>>,
    texture_cache: Cache<'font>,
    last_draw: LastDrawInfo,

    // cache of section-layout hash -> computed glyphs, this avoid repeated glyph computation
    // for identical layout/sections common to repeated frame rendering
    calculate_glyph_cache: hashbrown::HashMap<SectionHash, Glyphed<'font, V>>,

    last_frame_seq_id_sections: Vec<SectionHashDetail>,
    frame_seq_id_sections: Vec<SectionHashDetail>,

    // buffer of section-layout hashs (that must exist in the calculate_glyph_cache)
    // to be used on the next `process_queued` call
    section_buffer: Vec<SectionHash>,

    // Set of section hashs to keep in the glyph cache this frame even if they haven't been drawn
    keep_in_cache: hashbrown::HashSet<SectionHash>,

    // config
    cache_glyph_positioning: bool,
    cache_glyph_drawing: bool,

    section_hasher: H,

    last_pre_positioned: Vec<Glyphed<'font, V>>,
    pre_positioned: Vec<Glyphed<'font, V>>,
}

impl<V: Clone + 'static, H> fmt::Debug for GlyphBrush<'_, V, H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GlyphBrush")
    }
}

impl<'font, V: Clone + 'static, H: BuildHasher> GlyphCruncher<'font> for GlyphBrush<'font, V, H> {
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
pub struct HighlightRange {
    pub pixel_coords: PixelCoords,
    pub bounds: Bounds,
    pub color: Color,
    pub z: f32,
}

pub struct AdditionalRects<V: Clone + 'static> {
    pub transform_status_line: fn(&mut V),
    pub extract_tex_coords: fn(&V) -> TexCoords,
    pub status_line_position: (f32, f32),
    pub status_scale: Scale,
    pub highlight_ranges: Vec<HighlightRange>,
}

impl<'font, V: Clone + 'static, H: BuildHasher> GlyphBrush<'font, V, H> {
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
        perf_viz::record_guard!("queue_custom_layout");
        let section = section.into();
        if cfg!(debug_assertions) {
            for text in &section.text {
                assert!(self.fonts.len() > text.font_id.0, "Invalid font id");
            }
        }
        perf_viz::start_record!("cache_glyphs");
        let section_hash = self.cache_glyphs(&section, custom_layout);
        perf_viz::end_record!("cache_glyphs");
        perf_viz::start_record!("add to cache");
        self.section_buffer.push(section_hash);
        self.keep_in_cache.insert(section_hash);
        perf_viz::end_record!("add to cache");
    }

    /// Queues a section/layout to be processed by the next call of
    /// [`process_queued`](struct.GlyphBrush.html#method.process_queued). Can be called multiple
    /// times to queue multiple sections for drawing.
    ///
    /// Benefits from caching, see [caching behaviour](#caching-behaviour).
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
    fn cache_glyphs<L>(&mut self, section: &VariedSection<'_>, layout: &L) -> SectionHash
    where
        L: GlyphPositioner,
    {
        let section_hash = SectionHashDetail::new(&self.section_hasher, section, layout);
        // section id used to find a similar calculated layout from last frame
        let frame_seq_id = self.frame_seq_id_sections.len();
        self.frame_seq_id_sections.push(section_hash);

        if self.cache_glyph_positioning {
            perf_viz::record_guard!("if self.cache_glyph_positioning");
            if !self.calculate_glyph_cache.contains_key(&section_hash.full) {
                let geometry = SectionGeometry::from(section);

                let recalculated_glyphs = self
                    .last_frame_seq_id_sections
                    .get(frame_seq_id)
                    .cloned()
                    .and_then(|hash| {
                        let change = match section_hash.diff(hash) {
                            SectionHashDiff::GeometryChange => GlyphChange::Geometry(hash.geometry),
                            SectionHashDiff::ColorChange => GlyphChange::Color,
                            SectionHashDiff::AlphaChange => GlyphChange::Alpha,
                            SectionHashDiff::Different => return None,
                        };

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
            perf_viz::record_guard!("if not self.cache_glyph_positioning");
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
    #[perf_viz::record]
    pub fn process_queued<F1, F2>(
        &mut self,
        (screen_w, screen_h): (u32, u32),
        update_texture: F1,
        to_vertex: F2,
        additional_rects: Option<AdditionalRects<V>>,
    ) -> Result<BrushAction<V>, BrushError>
    where
        F1: FnMut(Rect<u32>, &[u8]),
        F2: Fn(GlyphVertex) -> V + Copy,
    {
        let current_text_state = {
            let mut s = self.section_hasher.build_hasher();
            self.section_buffer.hash(&mut s);
            screen_w.hash(&mut s);
            screen_h.hash(&mut s);
            s.finish()
        };

        let result = if !self.cache_glyph_drawing
            || self.last_draw.text_state != current_text_state
            || self.last_pre_positioned != self.pre_positioned
        {
            let mut some_text = false;
            // Everything in the section_buffer should also be here. The extras should also
            // be retained in the texture cache avoiding cache thrashing if they are rendered
            // in a 2-draw per frame style.

            // This additional rects stuff was hacked in here to fix a perf issue arising from
            // the previous method that used multiple instances of the character
            let rect_hash = additional_rects.map(
                |AdditionalRects {
                     transform_status_line,
                     extract_tex_coords,
                     status_line_position,
                     status_scale,
                     highlight_ranges,
                 }| {
                    let section = Section {
                        // The status line will be a rectangle with the height of this glyph
                        //stretched horzontally across the screen.
                        text: "█",
                        scale: status_scale,
                        screen_position: status_line_position,
                        bounds: (std::f32::INFINITY, std::f32::INFINITY),
                        color: [7.0 / 256.0, 7.0 / 256.0, 7.0 / 256.0, 1.0],
                        layout: Layout::default_single_line(),
                        z: 0.1875,
                        ..Section::default()
                    }
                    .into();

                    let section_hash = self.cache_glyphs(&section, &section.layout);
                    self.section_buffer.push(section_hash);
                    self.keep_in_cache.insert(section_hash);

                    (
                        transform_status_line,
                        extract_tex_coords,
                        section_hash,
                        highlight_ranges,
                    )
                },
            );

            perf_viz::start_record!("keep_in_cache");
            for section_hash in &self.keep_in_cache {
                for &(ref glyph, _, font_id) in self
                    .calculate_glyph_cache
                    .get(section_hash)
                    .iter()
                    .flat_map(|gs| &gs.positioned.glyphs)
                {
                    perf_viz::start_record!("calculate_glyph_cache loop");
                    self.texture_cache.queue_glyph(font_id.0, glyph.clone());
                    some_text = true;
                    perf_viz::end_record!("calculate_glyph_cache loop");
                }
            }
            perf_viz::end_record!("keep_in_cache");

            perf_viz::start_record!("&(ref glyph, _, font_id)");
            for &(ref glyph, _, font_id) in self
                .pre_positioned
                .iter()
                .flat_map(|p| &p.positioned.glyphs)
            {
                perf_viz::start_record!("pre_positioned loop");
                self.texture_cache.queue_glyph(font_id.0, glyph.clone());
                some_text = true;
                perf_viz::end_record!("pre_positioned loop");
            }
            perf_viz::end_record!("&(ref glyph, _, font_id)");

            if some_text {
                perf_viz::record_guard!("if some_text");
                match self.texture_cache.cache_queued(update_texture) {
                    Ok(CachedBy::Adding) => {}
                    Ok(CachedBy::Reordering) => {
                        perf_viz::record_guard!("CachedBy::Reordering calculate_glyph_cache");
                        for glyphed in self.calculate_glyph_cache.values_mut() {
                            perf_viz::start_record!("CachedBy::Reordering calculate_glyph_cache");
                            glyphed.invalidate_texture_positions();
                            perf_viz::end_record!("CachedBy::Reordering calculate_glyph_cache");
                        }
                    }
                    Err(_) => {
                        perf_viz::record_guard!("Create TextureTooSmall");
                        let (width, height) = self.texture_cache.dimensions();
                        return Err(BrushError::TextureTooSmall {
                            suggested: (width * 2, height * 2),
                        });
                    }
                }
            }

            self.last_draw.text_state = current_text_state;

            BrushAction::Draw({
                let screen_dims = (screen_w as f32, screen_h as f32);

                let mut verts = Vec::new();

                for hash in &self.section_buffer {
                    perf_viz::start_record!("BrushAction::Draw section_buffer");
                    let glyphed = self.calculate_glyph_cache.get_mut(hash).unwrap();
                    glyphed.ensure_vertices(&self.texture_cache, screen_dims, to_vertex);
                    verts.extend(glyphed.vertices.iter().cloned());
                    perf_viz::end_record!("BrushAction::Draw section_buffer");
                }

                for glyphed in &mut self.pre_positioned {
                    perf_viz::start_record!("BrushAction::Draw pre_positioned");
                    // pre-positioned glyph vertices can't be cached so
                    // generate & move straight into draw vec
                    glyphed.ensure_vertices(&self.texture_cache, screen_dims, to_vertex);
                    verts.append(&mut glyphed.vertices);
                    perf_viz::end_record!("BrushAction::Draw pre_positioned");
                }

                // This status line stuff was hacked in here to fix a perf issue arising from
                // the previous method that used multiple instances of the character
                if let Some((
                    transform_status_line,
                    extract_tex_coords,
                    rect_hash,
                    highlight_ranges,
                )) = rect_hash
                {
                    let glyphed = self.calculate_glyph_cache.get_mut(&rect_hash).unwrap();
                    glyphed.ensure_vertices(&self.texture_cache, screen_dims, to_vertex);
                    if let Some(mut vertex) = glyphed.vertices.pop() {
                        let tex_coords = {
                            let mut tex_coords = extract_tex_coords(&vertex);

                            // Hacky way to prevent sampling outside of the texture.
                            let x_apron = (tex_coords.max.x - tex_coords.min.x) * 0.25;
                            let y_apron = (tex_coords.max.y - tex_coords.min.y) * 0.25;
                            tex_coords.min.x += x_apron;
                            tex_coords.min.y += y_apron;
                            tex_coords.max.x -= x_apron;
                            tex_coords.max.y -= y_apron;

                            tex_coords
                        };

                        transform_status_line(&mut vertex);

                        verts.push(vertex);

                        let highlight_base = GlyphVertex {
                            tex_coords,
                            screen_dimensions: screen_dims,
                            ..d!()
                        };
                        for range in highlight_ranges {
                            let HighlightRange {
                                pixel_coords,
                                bounds,
                                color,
                                z,
                            } = range;
                            verts.push(to_vertex(GlyphVertex {
                                tex_coords,
                                pixel_coords,
                                bounds,
                                color,
                                z,
                                ..highlight_base
                            }));
                        }
                    }
                }

                verts
            })
        } else {
            BrushAction::ReDraw
        };

        self.cleanup_frame();
        Ok(result)
    }

    /// Rebuilds the logical texture cache with new dimensions. Should be avoided if possible.
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

    #[perf_viz::record]
    fn cleanup_frame(&mut self) {
        if self.cache_glyph_positioning {
            // clear section_buffer & trim calculate_glyph_cache to active sections
            let active = mem::replace(&mut self.keep_in_cache, <_>::default());
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
    /// Returns a new [`FontId`](struct.FontId.html) to reference this font
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

#[derive(Debug, Default)]
struct LastDrawInfo {
    text_state: u64,
}

pub type TexCoords = Rect<f32>;
pub type PixelCoords = Rect<i32>;
pub type Bounds = Rect<f32>;

// glyph: &PositionedGlyph,
// color: Color,
// font_id: FontId,
// cache: &Cache,
// bounds: Rect<f32>,
// z: f32,
// (screen_width, screen_height): (f32, f32),

/// Data used to generate vertex information for a single glyph
#[derive(Debug, Default)]
pub struct GlyphVertex {
    pub tex_coords: TexCoords,
    pub pixel_coords: PixelCoords,
    pub bounds: Bounds,
    pub screen_dimensions: (f32, f32),
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
        write!(f, "{}", std::error::Error::description(self))
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
    /// hash of text
    text_no_color_alpha: SectionHash,
    // hash of text & alpha
    text_no_color: SectionHash,
    /// hash of text & colors including alpha
    text: SectionHash,
    /// hash of everything
    full: SectionHash,

    /// copy of geometry for later comparison
    geometry: SectionGeometry,
}

#[derive(Debug)]
enum SectionHashDiff {
    GeometryChange,
    ColorChange,
    AlphaChange,
    Different,
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
        let text_no_color_alpha_hash = s.finish();

        parts.hash_alpha(&mut s);
        let text_no_color_hash = s.finish();

        parts.hash_color(&mut s);
        let text_hash = s.finish();

        parts.hash_geometry(&mut s);
        parts.hash_z(&mut s);
        let full_hash = s.finish();

        Self {
            text_no_color_alpha: text_no_color_alpha_hash,
            text_no_color: text_no_color_hash,
            text: text_hash,
            // text_geometry: text_geo_hash,
            full: full_hash,
            geometry: SectionGeometry::from(section),
        }
    }

    /// They're different, but how?
    fn diff(self, other: SectionHashDetail) -> SectionHashDiff {
        if self.text == other.text {
            return SectionHashDiff::GeometryChange;
        } else if self.geometry == other.geometry {
            if self.text_no_color == other.text_no_color {
                return SectionHashDiff::ColorChange;
            } else if self.text_no_color_alpha == other.text_no_color_alpha {
                return SectionHashDiff::AlphaChange;
            }
        }
        SectionHashDiff::Different
    }
}

/// Container for positioned glyphs which can generate and cache vertices
struct Glyphed<'font, V: Clone + 'static> {
    positioned: GlyphedSection<'font>,
    vertices: Vec<V>,
}

impl<V: Clone + 'static> PartialEq for Glyphed<'_, V> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.positioned == other.positioned
    }
}

impl<'font, V: Clone + 'static> Glyphed<'font, V> {
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
    fn ensure_vertices<F>(
        &mut self,
        texture_cache: &Cache<'font>,
        screen_dimensions: (f32, f32),
        to_vertex: F,
    ) where
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
        self.vertices
            .extend(glyphs.iter().filter_map(|(glyph, color, font_id)| {
                match texture_cache.rect_for(font_id.0, glyph) {
                    Err(err) => {
                        error!("Cache miss?: {:?}, {:?}: {}", font_id, glyph, err);
                        None
                    }
                    Ok(None) => None,
                    Ok(Some((tex_coords, pixel_coords))) => {
                        if pixel_coords.min.x as f32 > bounds.max.x
                            || pixel_coords.min.y as f32 > bounds.max.y
                            || bounds.min.x > pixel_coords.max.x as f32
                            || bounds.min.y > pixel_coords.max.y as f32
                        {
                            // glyph is totally outside the bounds
                            None
                        } else {
                            Some(to_vertex(GlyphVertex {
                                tex_coords,
                                pixel_coords,
                                bounds,
                                screen_dimensions,
                                color: *color,
                                z,
                            }))
                        }
                    }
                }
            }));
    }
}
