// This file has been modified for use in the `rote ` project.

//! This module provides capabilities for managing a cache of rendered glyphs in
//! GPU memory, with the goal of minimisng the size and frequency of glyph
//! uploads to GPU memory from the CPU.
//!
//! This module is optional, and not compiled by default. To use it enable the
//! `gpu_cache` feature in your Cargo.toml.
//!
//! Typical applications that render directly with hardware graphics APIs (e.g.
//! games) need text rendering. There is not yet a performant solution for high
//! quality text rendering directly on the GPU that isn't experimental research
//! work. Quality is often critical for legibility, so many applications use
//! text or individual characters that have been rendered on the CPU. This is
//! done either ahead-of-time, giving a fixed set of fonts, characters, and
//! sizes that can be used at runtime, or dynamically as text is required. This
//! latter scenario is more flexible and the focus of this module.
//!
//! To minimise the CPU load and texture upload bandwidth saturation, recently
//! used glyphs should be cached on the GPU for use by future frames. This
//! module provides a mechanism for maintaining such a cache in the form of a
//! single packed 2D GPU texture. When a rendered glyph is requested, it is
//! either retrieved from its location in the texture if it is present or room
//! is made in the cache (if necessary), the CPU renders the glyph then it is
//! uploaded into a gap in the texture to be available for GPU rendering. This
//! cache uses a Least Recently Used (LRU) cache eviction scheme - glyphs in the
//! cache that have not been used recently are as a rule of thumb not likely to
//! be used again soon, so they are the best candidates for eviction to make
//! room for required glyphs.
//!
//! The API for the cache does not assume a particular graphics API. The
//! intended usage is to queue up glyphs that need to be present for the current
//! frame using `Cache::queue_glyph`, update the cache to ensure that the queued
//! glyphs are present using `Cache::cache_queued` (providing a function for
//! uploading pixel data), then when it's time to render call `Cache::rect_for`
//! to get the UV coordinates in the cache texture for each glyph. For a
//! concrete use case see the `gpu_cache` example.
//!
//! Cache dimensions are immutable. If you need to change the dimensions of the
//! cache texture (e.g. due to high cache pressure), rebuild a new `Cache`.
//! Either from scratch or with `CacheBuilder::rebuild`.
use crate::{point, vector, GlyphId, Point, PositionedGlyph, Rect, Vector};
use linked_hash_map::LinkedHashMap;
use rustc_hash::{FxHashMap, FxHasher};
use std::collections::{HashMap, HashSet};
use std::error;
use std::fmt;
use std::hash::BuildHasherDefault;

type FxBuildHasher = BuildHasherDefault<FxHasher>;

/// Texture coordinates (floating point) of the quad for a glyph in the cache,
/// as well as the pixel-space (integer) coordinates that this region should be
/// drawn at.
pub type TextureCoords = (Rect<f32>, Rect<i32>);
type FontId = usize;

/// Indicates where a glyph texture is stored in the cache
/// (row position, glyph index in row)
type TextureRowGlyphIndex = (u32, u32);

/// Texture lookup key that uses scale & offset as integers attained
/// by dividing by the relevant tolerance.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct LossyGlyphInfo {
    font_id: FontId,
    glyph_id: GlyphId,
    /// x & y scales divided by `scale_tolerance` & rounded
    scale_over_tolerance: (u32, u32),
    /// Normalised subpixel positions divided by `position_tolerance` & rounded
    ///
    /// `u16` is enough as subpixel position `[-0.5, 0.5]` converted to `[0, 1]`
    ///  divided by the min `position_tolerance` (`0.001`) is small.
    offset_over_tolerance: (u16, u16),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ByteArray2d {
    inner_array: Vec<u8>,
    row: usize,
    col: usize,
}

impl ByteArray2d {
    #[inline]
    pub fn zeros(row: usize, col: usize) -> Self {
        ByteArray2d {
            inner_array: vec![0; row * col],
            row,
            col,
        }
    }

    #[inline]
    fn as_slice(&self) -> &[u8] {
        self.inner_array.as_slice()
    }

    #[inline]
    fn get_vec_index(&self, row: usize, col: usize) -> usize {
        debug_assert!(
            row < self.row,
            "row out of range: row={}, given={}",
            self.row,
            row
        );
        debug_assert!(
            col < self.col,
            "column out of range: col={}, given={}",
            self.col,
            col
        );
        row * self.col + col
    }
}

impl std::ops::Index<(usize, usize)> for ByteArray2d {
    type Output = u8;

    #[inline]
    fn index(&self, (row, col): (usize, usize)) -> &u8 {
        &self.inner_array[self.get_vec_index(row, col)]
    }
}

impl std::ops::IndexMut<(usize, usize)> for ByteArray2d {
    #[inline]
    fn index_mut(&mut self, (row, col): (usize, usize)) -> &mut u8 {
        let vec_index = self.get_vec_index(row, col);
        &mut self.inner_array[vec_index]
    }
}

/// Row of pixel data
struct Row {
    /// Row pixel height
    height: u32,
    /// Pixel width current in use by glyphs
    width: u32,
    glyphs: Vec<GlyphTexInfo>,
}

struct GlyphTexInfo {
    glyph_info: LossyGlyphInfo,
    /// Actual (lossless) normalised subpixel offset of rasterized glyph
    offset: Vector<f32>,
    tex_coords: Rect<u32>,
}

trait PaddingAware {
    fn unpadded(self) -> Self;
}

impl PaddingAware for Rect<u32> {
    /// A padded texture has 1 extra pixel on all sides
    fn unpadded(mut self) -> Self {
        self.min.x += 1;
        self.min.y += 1;
        self.max.x -= 1;
        self.max.y -= 1;
        self
    }
}

/// An implementation of a dynamic GPU glyph cache. See the module documentation
/// for more information.
pub struct Cache<'font> {
    scale_tolerance: f32,
    position_tolerance: f32,
    width: u32,
    height: u32,
    rows: LinkedHashMap<u32, Row, FxBuildHasher>,
    /// Mapping of row gaps bottom -> top
    space_start_for_end: FxHashMap<u32, u32>,
    /// Mapping of row gaps top -> bottom
    space_end_for_start: FxHashMap<u32, u32>,
    queue: Vec<(FontId, PositionedGlyph<'font>)>,
    all_glyphs: FxHashMap<LossyGlyphInfo, TextureRowGlyphIndex>,
    pad_glyphs: bool,
    multithread: bool,
}

/// Builder & rebuilder for `Cache`.
///
/// # Example
///
/// ```
/// use rusttype::gpu_cache::Cache;
///
/// // Create a cache with all default values set explicitly
/// // equivalent to `Cache::builder().build()`
/// let default_cache = Cache::builder()
///     .dimensions(256, 256)
///     .scale_tolerance(0.1)
///     .position_tolerance(0.1)
///     .pad_glyphs(true)
///     .multithread(true)
///     .build();
///
/// // Create a cache with all default values, except with a dimension of 1024x1024
/// let bigger_cache = Cache::builder().dimensions(1024, 1024).build();
/// ```
#[derive(Debug, Clone)]
pub struct CacheBuilder {
    dimensions: (u32, u32),
    scale_tolerance: f32,
    position_tolerance: f32,
    pad_glyphs: bool,
    multithread: bool,
}

impl Default for CacheBuilder {
    fn default() -> Self {
        Self {
            dimensions: (256, 256),
            scale_tolerance: 0.1,
            position_tolerance: 0.1,
            pad_glyphs: true,
            multithread: true,
        }
    }
}

impl CacheBuilder {
    /// `width` & `height` dimensions of the 2D texture that will hold the
    /// cache contents on the GPU.
    ///
    /// This must match the dimensions of the actual texture used, otherwise
    /// `cache_queued` will try to cache into coordinates outside the bounds of
    /// the texture.
    ///
    /// # Example (set to default value)
    ///
    /// ```
    /// # use rusttype::gpu_cache::Cache;
    /// let cache = Cache::builder().dimensions(256, 256).build();
    /// ```
    pub fn dimensions(mut self, width: u32, height: u32) -> Self {
        self.dimensions = (width, height);
        self
    }

    /// Specifies the tolerances (maximum allowed difference) for judging
    /// whether an existing glyph in the cache is close enough to the
    /// requested glyph in scale to be used in its place. Due to floating
    /// point inaccuracies a min value of `0.001` is enforced.
    ///
    /// Both `scale_tolerance` and `position_tolerance` are measured in pixels.
    ///
    /// Tolerances produce even steps for scale and subpixel position. Only a
    /// single glyph texture will be used within a single step. For example,
    /// `scale_tolerance = 0.1` will have a step `9.95-10.05` so similar glyphs
    /// with scale `9.98` & `10.04` will match.
    ///
    /// A typical application will produce results with no perceptible
    /// inaccuracies with `scale_tolerance` and `position_tolerance` set to
    /// 0.1. Depending on the target DPI higher tolerance may be acceptable.
    ///
    /// # Example (set to default value)
    ///
    /// ```
    /// # use rusttype::gpu_cache::Cache;
    /// let cache = Cache::builder().scale_tolerance(0.1).build();
    /// ```
    pub fn scale_tolerance<V: Into<f32>>(mut self, scale_tolerance: V) -> Self {
        self.scale_tolerance = scale_tolerance.into();
        self
    }
    /// Specifies the tolerances (maximum allowed difference) for judging
    /// whether an existing glyph in the cache is close enough to the requested
    /// glyph in subpixel offset to be used in its place. Due to floating
    /// point inaccuracies a min value of `0.001` is enforced.
    ///
    /// Both `scale_tolerance` and `position_tolerance` are measured in pixels.
    ///
    /// Tolerances produce even steps for scale and subpixel position. Only a
    /// single glyph texture will be used within a single step. For example,
    /// `scale_tolerance = 0.1` will have a step `9.95-10.05` so similar glyphs
    /// with scale `9.98` & `10.04` will match.
    ///
    /// Note that since `position_tolerance` is a tolerance of subpixel
    /// offsets, setting it to 1.0 or higher is effectively a "don't care"
    /// option.
    ///
    /// A typical application will produce results with no perceptible
    /// inaccuracies with `scale_tolerance` and `position_tolerance` set to
    /// 0.1. Depending on the target DPI higher tolerance may be acceptable.
    ///
    /// # Example (set to default value)
    ///
    /// ```
    /// # use rusttype::gpu_cache::Cache;
    /// let cache = Cache::builder().position_tolerance(0.1).build();
    /// ```
    pub fn position_tolerance<V: Into<f32>>(mut self, position_tolerance: V) -> Self {
        self.position_tolerance = position_tolerance.into();
        self
    }
    /// Pack glyphs in texture with a padding of a single zero alpha pixel to
    /// avoid bleeding from interpolated shader texture lookups near edges.
    ///
    /// If glyphs are never transformed this may be set to `false` to slightly
    /// improve the glyph packing.
    ///
    /// # Example (set to default value)
    ///
    /// ```
    /// # use rusttype::gpu_cache::Cache;
    /// let cache = Cache::builder().pad_glyphs(true).build();
    /// ```
    pub fn pad_glyphs(mut self, pad_glyphs: bool) -> Self {
        self.pad_glyphs = pad_glyphs;
        self
    }
    /// When multiple CPU cores are available spread rasterization work across
    /// all cores.
    ///
    /// Significantly reduces worst case latency in multicore environments.
    ///
    /// # Example (set to default value)
    ///
    /// ```
    /// # use rusttype::gpu_cache::Cache;
    /// let cache = Cache::builder().multithread(true).build();
    /// ```
    pub fn multithread(mut self, multithread: bool) -> Self {
        self.multithread = multithread;
        self
    }

    fn validated(self) -> Self {
        assert!(self.scale_tolerance >= 0.0);
        assert!(self.position_tolerance >= 0.0);
        let scale_tolerance = self.scale_tolerance.max(0.001);
        let position_tolerance = self.position_tolerance.max(0.001);
        let multithread = self.multithread && num_cpus::get() > 1;
        Self {
            scale_tolerance,
            position_tolerance,
            multithread,
            ..self
        }
    }

    /// Constructs a new cache. Note that this is just the CPU side of the
    /// cache. The GPU texture is managed by the user.
    ///
    /// # Panics
    ///
    /// `scale_tolerance` or `position_tolerance` are less than or equal to
    /// zero.
    ///
    /// # Example
    ///
    /// ```
    /// # use rusttype::gpu_cache::Cache;
    /// let cache = Cache::builder().build();
    /// ```
    pub fn build<'a>(self) -> Cache<'a> {
        let CacheBuilder {
            dimensions: (width, height),
            scale_tolerance,
            position_tolerance,
            pad_glyphs,
            multithread,
        } = self.validated();

        Cache {
            scale_tolerance,
            position_tolerance,
            width,
            height,
            rows: LinkedHashMap::default(),
            space_start_for_end: {
                let mut m = HashMap::default();
                m.insert(height, 0);
                m
            },
            space_end_for_start: {
                let mut m = HashMap::default();
                m.insert(0, height);
                m
            },
            queue: Vec::new(),
            all_glyphs: HashMap::default(),
            pad_glyphs,
            multithread,
        }
    }

    /// Rebuilds a cache with new attributes. Carries over the existing cache
    /// queue unmodified.
    ///
    /// # Panics
    ///
    /// `scale_tolerance` or `position_tolerance` are less than or equal to
    /// zero.
    ///
    /// # Example
    ///
    /// ```
    /// # use rusttype::gpu_cache::Cache;
    /// # let mut cache = Cache::builder().build();
    /// // Rebuild the cache with different dimensions
    /// cache.to_builder().dimensions(768, 768).rebuild(&mut cache);
    /// ```
    pub fn rebuild(self, cache: &mut Cache) {
        let CacheBuilder {
            dimensions: (width, height),
            scale_tolerance,
            position_tolerance,
            pad_glyphs,
            multithread,
        } = self.validated();

        cache.width = width;
        cache.height = height;
        cache.scale_tolerance = scale_tolerance;
        cache.position_tolerance = position_tolerance;
        cache.pad_glyphs = pad_glyphs;
        cache.multithread = multithread;
        cache.clear();
    }
}

/// Returned from `Cache::rect_for`.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CacheReadErr {
    /// Indicates that the requested glyph is not present in the cache
    GlyphNotCached,
}
impl fmt::Display for CacheReadErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", error::Error::description(self))
    }
}
impl error::Error for CacheReadErr {
    fn description(&self) -> &str {
        match *self {
            CacheReadErr::GlyphNotCached => "Glyph not cached",
        }
    }
}

/// Returned from `Cache::cache_queued`.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CacheWriteErr {
    /// At least one of the queued glyphs is too big to fit into the cache, even
    /// if all other glyphs are removed.
    GlyphTooLarge,
    /// Not all of the requested glyphs can fit into the cache, even if the
    /// cache is completely cleared before the attempt.
    NoRoomForWholeQueue,
}
impl fmt::Display for CacheWriteErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", error::Error::description(self))
    }
}
impl error::Error for CacheWriteErr {
    fn description(&self) -> &str {
        match *self {
            CacheWriteErr::GlyphTooLarge => "Glyph too large",
            CacheWriteErr::NoRoomForWholeQueue => "No room for whole queue",
        }
    }
}

/// Successful method of caching of the queue.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CachedBy {
    /// Added any additional glyphs into the texture without affecting
    /// the position of any already cached glyphs in the latest queue.
    ///
    /// Glyphs not in the latest queue may have been removed.
    Adding,
    /// Fit the glyph queue by re-ordering all glyph texture positions.
    /// Previous texture positions are no longer valid.
    Reordering,
}

fn normalised_offset_from_position(position: Point<f32>) -> Vector<f32> {
    let mut offset = vector(position.x.fract(), position.y.fract());
    if offset.x > 0.5 {
        offset.x -= 1.0;
    } else if offset.x < -0.5 {
        offset.x += 1.0;
    }
    if offset.y > 0.5 {
        offset.y -= 1.0;
    } else if offset.y < -0.5 {
        offset.y += 1.0;
    }
    offset
}

impl<'font> Cache<'font> {
    /// Returns a default `CacheBuilder`.
    #[inline]
    pub fn builder() -> CacheBuilder {
        CacheBuilder::default()
    }

    /// Returns the current scale tolerance for the cache.
    pub fn scale_tolerance(&self) -> f32 {
        self.scale_tolerance
    }

    /// Returns the current subpixel position tolerance for the cache.
    pub fn position_tolerance(&self) -> f32 {
        self.position_tolerance
    }

    /// Returns the cache texture dimensions assumed by the cache. For proper
    /// operation this should match the dimensions of the used GPU texture.
    pub fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    /// Queue a glyph for caching by the next call to `cache_queued`. `font_id`
    /// is used to disambiguate glyphs from different fonts. The user should
    /// ensure that `font_id` is unique to the font the glyph is from.
    pub fn queue_glyph(&mut self, font_id: usize, glyph: PositionedGlyph<'font>) {
        if glyph.pixel_bounding_box().is_some() {
            self.queue.push((font_id, glyph));
        }
    }

    /// Clears the cache. Does not affect the glyph queue.
    pub fn clear(&mut self) {
        self.rows.clear();
        self.space_end_for_start.clear();
        self.space_end_for_start.insert(0, self.height);
        self.space_start_for_end.clear();
        self.space_start_for_end.insert(self.height, 0);
        self.all_glyphs.clear();
    }

    /// Clears the glyph queue.
    pub fn clear_queue(&mut self) {
        self.queue.clear();
    }

    /// Returns a `CacheBuilder` with this cache's attributes.
    pub fn to_builder(&self) -> CacheBuilder {
        CacheBuilder {
            dimensions: (self.width, self.height),
            position_tolerance: self.position_tolerance,
            scale_tolerance: self.scale_tolerance,
            pad_glyphs: self.pad_glyphs,
            multithread: self.multithread,
        }
    }

    /// Returns glyph info with accuracy according to the set tolerances.
    fn lossy_info_for(&self, font_id: FontId, glyph: &PositionedGlyph<'font>) -> LossyGlyphInfo {
        let scale = glyph.scale();
        let offset = normalised_offset_from_position(glyph.position());

        LossyGlyphInfo {
            font_id,
            glyph_id: glyph.id(),
            scale_over_tolerance: (
                (scale.x / self.scale_tolerance + 0.5) as u32,
                (scale.y / self.scale_tolerance + 0.5) as u32,
            ),
            // convert [-0.5, 0.5] -> [0, 1] then divide
            offset_over_tolerance: (
                ((offset.x + 0.5) / self.position_tolerance + 0.5) as u16,
                ((offset.y + 0.5) / self.position_tolerance + 0.5) as u16,
            ),
        }
    }

    /// Caches the queued glyphs. If this is unsuccessful, the queue is
    /// untouched. Any glyphs cached by previous calls to this function may be
    /// removed from the cache to make room for the newly queued glyphs. Thus if
    /// you want to ensure that a glyph is in the cache, the most recently
    /// cached queue must have contained that glyph.
    ///
    /// `uploader` is the user-provided function that should perform the texture
    /// uploads to the GPU. The information provided is the rectangular region
    /// to insert the pixel data into, and the pixel data itself. This data is
    /// provided in horizontal scanline format (row major), with stride equal to
    /// the rectangle width.
    ///
    /// If successful returns a `CachedBy` that can indicate the validity of
    /// previously cached glyph textures.
    #[perf_viz::record]
    pub fn cache_queued<F: FnMut(Rect<u32>, &[u8])>(
        &mut self,
        mut uploader: F,
    ) -> Result<CachedBy, CacheWriteErr> {
        let mut queue_success = true;
        let from_empty = self.all_glyphs.is_empty();

        {
            perf_viz::record_guard!("inner cache_queued block");
            let (mut in_use_rows, mut uncached_glyphs) = {
                let mut in_use_rows =
                    HashSet::with_capacity_and_hasher(self.rows.len(), FxBuildHasher::default());
                let mut uncached_glyphs = Vec::with_capacity(self.queue.len());

                // divide glyphs into texture rows where a matching glyph texture
                // already exists & glyphs where new textures must be cached
                for (font_id, ref glyph) in &self.queue {
                    let glyph_info = self.lossy_info_for(*font_id, glyph);
                    if let Some((row, ..)) = self.all_glyphs.get(&glyph_info) {
                        in_use_rows.insert(*row);
                    } else {
                        uncached_glyphs.push((glyph, glyph_info));
                    }
                }

                (in_use_rows, uncached_glyphs)
            };

            perf_viz::start_record!("get_refresh loop");
            for row in &in_use_rows {
                self.rows.get_refresh(row);
            }
            perf_viz::end_record!("get_refresh loop");

            perf_viz::start_record!("uncached_glyphs.sort_unstable_by_key");
            // tallest first gives better packing
            // can use 'sort_unstable' as order of equal elements is unimportant
            uncached_glyphs
                .sort_unstable_by_key(|(glyph, ..)| -glyph.pixel_bounding_box().unwrap().height());
            perf_viz::end_record!("uncached_glyphs.sort_unstable_by_key");

            self.all_glyphs.reserve(uncached_glyphs.len());
            let mut draw_and_upload = Vec::with_capacity(uncached_glyphs.len());

            'per_glyph: for (glyph, glyph_info) in uncached_glyphs {
                // glyph may match a texture cached by a previous iteration
                if self.all_glyphs.contains_key(&glyph_info) {
                    continue;
                }

                // Not cached, so add it:
                let (width, height) = {
                    let bb = glyph.pixel_bounding_box().unwrap();
                    if self.pad_glyphs {
                        (bb.width() as u32 + 2, bb.height() as u32 + 2)
                    } else {
                        (bb.width() as u32, bb.height() as u32)
                    }
                };
                if width >= self.width || height >= self.height {
                    return Result::Err(CacheWriteErr::GlyphTooLarge);
                }
                // find row to put the glyph in, most used rows first
                let mut row_top = None;
                for (top, row) in self.rows.iter().rev() {
                    if row.height >= height && self.width - row.width >= width {
                        // found a spot on an existing row
                        row_top = Some(*top);
                        break;
                    }
                }

                if row_top.is_none() {
                    let mut gap = None;
                    // See if there is space for a new row
                    for (start, end) in &self.space_end_for_start {
                        if end - start >= height {
                            gap = Some((*start, *end));
                            break;
                        }
                    }
                    if gap.is_none() {
                        // Remove old rows until room is available
                        while !self.rows.is_empty() {
                            // check that the oldest row isn't also in use
                            if !in_use_rows.contains(self.rows.front().unwrap().0) {
                                // Remove row
                                let (top, row) = self.rows.pop_front().unwrap();

                                for g in row.glyphs {
                                    self.all_glyphs.remove(&g.glyph_info);
                                }

                                let (mut new_start, mut new_end) = (top, top + row.height);
                                // Update the free space maps
                                // Combine with neighbouring free space if possible
                                if let Some(end) = self.space_end_for_start.remove(&new_end) {
                                    new_end = end;
                                }
                                if let Some(start) = self.space_start_for_end.remove(&new_start) {
                                    new_start = start;
                                }
                                self.space_start_for_end.insert(new_end, new_start);
                                self.space_end_for_start.insert(new_start, new_end);
                                if new_end - new_start >= height {
                                    // The newly formed gap is big enough
                                    gap = Some((new_start, new_end));
                                    break;
                                }
                            }
                            // all rows left are in use
                            // try a clean insert of all needed glyphs
                            // if that doesn't work, fail
                            else if from_empty {
                                // already trying a clean insert, don't do it again
                                return Err(CacheWriteErr::NoRoomForWholeQueue);
                            } else {
                                // signal that a retry is needed
                                queue_success = false;
                                break 'per_glyph;
                            }
                        }
                    }
                    let (gap_start, gap_end) = gap.unwrap();
                    // fill space for new row
                    let new_space_start = gap_start + height;
                    self.space_end_for_start.remove(&gap_start);
                    if new_space_start == gap_end {
                        self.space_start_for_end.remove(&gap_end);
                    } else {
                        self.space_end_for_start.insert(new_space_start, gap_end);
                        self.space_start_for_end.insert(gap_end, new_space_start);
                    }
                    // add the row
                    self.rows.insert(
                        gap_start,
                        Row {
                            width: 0,
                            height,
                            glyphs: Vec::new(),
                        },
                    );
                    row_top = Some(gap_start);
                }
                let row_top = row_top.unwrap();
                // calculate the target rect
                let row = self.rows.get_refresh(&row_top).unwrap();
                let tex_coords = Rect {
                    min: point(row.width, row_top),
                    max: point(row.width + width, row_top + height),
                };

                draw_and_upload.push((tex_coords, glyph));

                // add the glyph to the row
                row.glyphs.push(GlyphTexInfo {
                    glyph_info,
                    offset: normalised_offset_from_position(glyph.position()),
                    tex_coords,
                });
                row.width += width;
                in_use_rows.insert(row_top);

                self.all_glyphs
                    .insert(glyph_info, (row_top, row.glyphs.len() as u32 - 1));
            }

            if queue_success {
                perf_viz::start_record!("if queue_success");
                let glyph_count = draw_and_upload.len();

                if self.multithread && glyph_count > 1 {
                    perf_viz::record_guard!("multithread rasterization");
                    // multithread rasterization
                    use crossbeam_deque::Steal;
                    use std::{
                        mem,
                        sync::mpsc::{self, TryRecvError},
                    };

                    let rasterize_queue = crossbeam_deque::Injector::new();
                    let (to_main, from_stealers) = mpsc::channel();
                    let pad_glyphs = self.pad_glyphs;

                    for el in draw_and_upload {
                        rasterize_queue.push(el);
                    }
                    crossbeam_utils::thread::scope(|scope| {
                        perf_viz::record_guard!("crossbeam_utils::thread::scope");
                        for _ in 0..num_cpus::get().min(glyph_count).saturating_sub(1) {
                            perf_viz::record_guard!("num_cpus::get()");
                            let rasterize_queue = &rasterize_queue;
                            let to_main = to_main.clone();
                            scope.spawn(move |_| loop {
                                match rasterize_queue.steal() {
                                    Steal::Success((tex_coords, glyph)) => {
                                        let pixels = draw_glyph(tex_coords, glyph, pad_glyphs);
                                        to_main.send((tex_coords, pixels)).unwrap();
                                    }
                                    Steal::Empty => break,
                                    Steal::Retry => {}
                                }
                            });
                        }
                        mem::drop(to_main);

                        let mut workers_finished = false;
                        loop {
                            perf_viz::record_guard!("workers_finished outer loop");
                            match rasterize_queue.steal() {
                                Steal::Success((tex_coords, glyph)) => {
                                    perf_viz::start_record!("workers_finished draw_glyph");
                                    let pixels = draw_glyph(tex_coords, glyph, pad_glyphs);
                                    perf_viz::end_record!("workers_finished draw_glyph");
                                    perf_viz::start_record!("workers_finished uploader");
                                    uploader(tex_coords, pixels.as_slice());
                                    perf_viz::end_record!("workers_finished uploader");
                                }
                                Steal::Empty if workers_finished => break,
                                Steal::Empty | Steal::Retry => {}
                            }

                            while !workers_finished {
                                perf_viz::record_guard!("while !workers_finished loop");
                                match from_stealers.try_recv() {
                                    Ok((tex_coords, pixels)) => {
                                        uploader(tex_coords, pixels.as_slice())
                                    }
                                    Err(TryRecvError::Disconnected) => workers_finished = true,
                                    Err(TryRecvError::Empty) => break,
                                }
                            }
                        }
                    })
                    .unwrap();
                } else {
                    perf_viz::record_guard!("single thread rasterization");
                    // single thread rasterization
                    for (tex_coords, glyph) in draw_and_upload {
                        let pixels = draw_glyph(tex_coords, glyph, self.pad_glyphs);
                        uploader(tex_coords, pixels.as_slice());
                    }
                }
                perf_viz::end_record!("if queue_success");
            }
        }

        if queue_success {
            self.queue.clear();
            Ok(CachedBy::Adding)
        } else {
            // clear the cache then try again with optimal packing
            self.clear();
            self.cache_queued(uploader).map(|_| CachedBy::Reordering)
        }
    }

    /// Retrieves the (floating point) texture coordinates of the quad for a
    /// glyph in the cache, as well as the pixel-space (integer) coordinates
    /// that this region should be drawn at. These pixel-space coordinates
    /// assume an origin at the top left of the quad. In the majority of cases
    /// these pixel-space coordinates should be identical to the bounding box of
    /// the input glyph. They only differ if the cache has returned a substitute
    /// glyph that is deemed close enough to the requested glyph as specified by
    /// the cache tolerance parameters.
    ///
    /// A sucessful result is `Some` if the glyph is not an empty glyph (no
    /// shape, and thus no rect to return).
    ///
    /// Ensure that `font_id` matches the `font_id` that was passed to
    /// `queue_glyph` with this `glyph`.
    pub fn rect_for(
        &self,
        font_id: usize,
        glyph: &PositionedGlyph,
    ) -> Result<Option<TextureCoords>, CacheReadErr> {
        if glyph.pixel_bounding_box().is_none() {
            return Ok(None);
        }

        let (row, index) = self
            .all_glyphs
            .get(&self.lossy_info_for(font_id, glyph))
            .ok_or(CacheReadErr::GlyphNotCached)?;

        let (tex_width, tex_height) = (self.width as f32, self.height as f32);

        let GlyphTexInfo {
            tex_coords: mut tex_rect,
            offset: tex_offset,
            ..
        } = self.rows[&row].glyphs[*index as usize];
        if self.pad_glyphs {
            tex_rect = tex_rect.unpadded();
        }
        let uv_rect = Rect {
            min: point(
                tex_rect.min.x as f32 / tex_width,
                tex_rect.min.y as f32 / tex_height,
            ),
            max: point(
                tex_rect.max.x as f32 / tex_width,
                tex_rect.max.y as f32 / tex_height,
            ),
        };

        let local_bb = glyph
            .unpositioned()
            .clone()
            .positioned(point(0.0, 0.0) + tex_offset)
            .pixel_bounding_box()
            .unwrap();
        let min_from_origin =
            point(local_bb.min.x as f32, local_bb.min.y as f32) - (point(0.0, 0.0) + tex_offset);
        let ideal_min = min_from_origin + glyph.position();
        let min = point(ideal_min.x.round() as i32, ideal_min.y.round() as i32);
        let bb_offset = min - local_bb.min;
        let bb = Rect {
            min,
            max: local_bb.max + bb_offset,
        };
        Ok(Some((uv_rect, bb)))
    }
}

#[inline]
fn draw_glyph(tex_coords: Rect<u32>, glyph: &PositionedGlyph<'_>, pad_glyphs: bool) -> ByteArray2d {
    let mut pixels = ByteArray2d::zeros(tex_coords.height() as usize, tex_coords.width() as usize);
    if pad_glyphs {
        glyph.draw(|x, y, v| {
            let v = (v * 255.0).round().max(0.0).min(255.0) as u8;
            // `+ 1` accounts for top/left glyph padding
            pixels[(y as usize + 1, x as usize + 1)] = v;
        });
    } else {
        glyph.draw(|x, y, v| {
            let v = (v * 255.0).round().max(0.0).min(255.0) as u8;
            pixels[(y as usize, x as usize)] = v;
        });
    }
    pixels
}
