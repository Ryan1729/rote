use crate::{DefaultSectionHasher, Font, FontId, GlyphBrush, SharedBytes};
use full_rusttype::gpu_cache::Cache;
use std::hash::BuildHasher;

/// Builder for a [`GlyphBrush`](struct.GlyphBrush.html).
pub struct GlyphBrushBuilder<'a, H = DefaultSectionHasher> {
    pub font_data: Vec<Font<'a>>,
    pub initial_cache_size: (u32, u32),
    pub gpu_cache_scale_tolerance: f32,
    pub gpu_cache_position_tolerance: f32,
    pub cache_glyph_positioning: bool,
    pub cache_glyph_drawing: bool,
    pub section_hasher: H,
}

impl<'a> GlyphBrushBuilder<'a> {
    /// Specifies the default font data used to render glyphs.
    /// Referenced with `FontId(0)`, which is default.
    #[inline]
    pub fn using_font_bytes<B: Into<SharedBytes<'a>>>(font_0_data: B) -> Self {
        Self::using_font(Font::from_bytes(font_0_data).unwrap())
    }

    #[inline]
    pub fn using_fonts_bytes<B, V>(font_data: V) -> Self
    where
        B: Into<SharedBytes<'a>>,
        V: Into<Vec<B>>,
    {
        Self::using_fonts(
            font_data
                .into()
                .into_iter()
                .map(|data| Font::from_bytes(data).unwrap())
                .collect::<Vec<_>>(),
        )
    }

    /// Specifies the default font used to render glyphs.
    /// Referenced with `FontId(0)`, which is default.
    #[inline]
    pub fn using_font(font_0: Font<'a>) -> Self {
        Self::using_fonts(vec![font_0])
    }

    pub fn using_fonts<V: Into<Vec<Font<'a>>>>(fonts: V) -> Self {
        GlyphBrushBuilder {
            font_data: fonts.into(),
            initial_cache_size: (256, 256),
            gpu_cache_scale_tolerance: 0.5,
            gpu_cache_position_tolerance: 0.1,
            cache_glyph_positioning: true,
            cache_glyph_drawing: true,
            section_hasher: DefaultSectionHasher::default(),
        }
    }
}

impl<'a, H: BuildHasher> GlyphBrushBuilder<'a, H> {
    /// Adds additional fonts to the one added in [`using_font`](#method.using_font) /
    /// [`using_font_bytes`](#method.using_font_bytes).
    /// Returns a [`FontId`](struct.FontId.html) to reference this font.
    pub fn add_font_bytes<B: Into<SharedBytes<'a>>>(&mut self, font_data: B) -> FontId {
        self.font_data
            .push(Font::from_bytes(font_data.into()).unwrap());
        FontId(self.font_data.len() - 1)
    }

    /// Adds additional fonts to the one added in [`using_font`](#method.using_font) /
    /// [`using_font_bytes`](#method.using_font_bytes).
    /// Returns a [`FontId`](struct.FontId.html) to reference this font.
    pub fn add_font(&mut self, font_data: Font<'a>) -> FontId {
        self.font_data.push(font_data);
        FontId(self.font_data.len() - 1)
    }

    /// Initial size of 2D texture used as a gpu cache, pixels (width, height).
    /// The GPU cache will dynamically quadruple in size whenever the current size
    /// is insufficient.
    ///
    /// Defaults to `(256, 256)`
    pub fn initial_cache_size(mut self, size: (u32, u32)) -> Self {
        self.initial_cache_size = size;
        self
    }

    /// Sets the maximum allowed difference in scale used for judging whether to reuse an
    /// existing glyph in the GPU cache.
    ///
    /// Defaults to `0.5`
    ///
    /// See rusttype docs for `rusttype::gpu_cache::Cache`
    pub fn gpu_cache_scale_tolerance(mut self, tolerance: f32) -> Self {
        self.gpu_cache_scale_tolerance = tolerance;
        self
    }

    /// Sets the maximum allowed difference in subpixel position used for judging whether
    /// to reuse an existing glyph in the GPU cache. Anything greater than or equal to
    /// 1.0 means "don't care".
    ///
    /// Defaults to `0.1`
    ///
    /// See rusttype docs for `rusttype::gpu_cache::Cache`
    pub fn gpu_cache_position_tolerance(mut self, tolerance: f32) -> Self {
        self.gpu_cache_position_tolerance = tolerance;
        self
    }

    /// Sets whether perform the calculation of glyph positioning according to the layout
    /// every time, or use a cached result if the input `Section` and `GlyphPositioner` are the
    /// same hash as a previous call.
    ///
    /// Improves performance. Should only disable if using a custom GlyphPositioner that is
    /// impure according to it's inputs, so caching a previous call is not desired. Disabling
    /// also disables [`cache_glyph_drawing`](#method.cache_glyph_drawing).
    ///
    /// Defaults to `true`
    pub fn cache_glyph_positioning(mut self, cache: bool) -> Self {
        self.cache_glyph_positioning = cache;
        self
    }

    /// Sets optimising drawing by reusing the last draw requesting an identical draw queue.
    ///
    /// Improves performance. Is disabled if
    /// [`cache_glyph_positioning`](#method.cache_glyph_positioning) is disabled.
    ///
    /// Defaults to `true`
    pub fn cache_glyph_drawing(mut self, cache: bool) -> Self {
        self.cache_glyph_drawing = cache;
        self
    }

    /// Sets the section hasher. `GlyphBrush` cannot handle absolute section hash collisions
    /// so use a good hash algorithm.
    ///
    /// This hasher is used to distinguish sections, rather than for hashmap internal use.
    ///
    /// Defaults to [xxHash](https://docs.rs/twox-hash).
    ///
    pub fn section_hasher<T: BuildHasher>(self, section_hasher: T) -> GlyphBrushBuilder<'a, T> {
        GlyphBrushBuilder {
            section_hasher,
            font_data: self.font_data,
            initial_cache_size: self.initial_cache_size,
            gpu_cache_scale_tolerance: self.gpu_cache_scale_tolerance,
            gpu_cache_position_tolerance: self.gpu_cache_position_tolerance,
            cache_glyph_positioning: self.cache_glyph_positioning,
            cache_glyph_drawing: self.cache_glyph_drawing,
        }
    }

    /// Builds a `GlyphBrush` using the input gfx factory
    pub fn build<V: Clone + 'static>(self) -> GlyphBrush<'a, V, H> {
        let (cache_width, cache_height) = self.initial_cache_size;

        GlyphBrush {
            fonts: self.font_data,
            texture_cache: Cache::builder()
                .dimensions(cache_width, cache_height)
                .scale_tolerance(self.gpu_cache_scale_tolerance)
                .position_tolerance(self.gpu_cache_position_tolerance)
                .build(),

            last_draw: <_>::default(),
            section_buffer: <_>::default(),
            calculate_glyph_cache: <_>::default(),

            last_frame_seq_id_sections: <_>::default(),
            frame_seq_id_sections: <_>::default(),

            keep_in_cache: <_>::default(),

            cache_glyph_positioning: self.cache_glyph_positioning,
            cache_glyph_drawing: self.cache_glyph_drawing && self.cache_glyph_positioning,

            section_hasher: self.section_hasher,

            last_pre_positioned: <_>::default(),
            pre_positioned: <_>::default(),
        }
    }
}

/// Macro to delegate builder methods to an inner `glyph_brush::GlyphBrushBuilder`
///
/// Implements:
/// * `add_font_bytes`
/// * `add_font`
/// * `initial_cache_size`
/// * `gpu_cache_scale_tolerance`
/// * `gpu_cache_position_tolerance`
/// * `cache_glyph_positioning`
/// * `cache_glyph_drawing`
///
/// # Example
/// ```
/// use glyph_brush::*;
/// use std::hash::BuildHasher;
///
/// # pub struct DownstreamGlyphBrush;
/// pub struct DownstreamGlyphBrushBuilder<'a, H> {
///     inner: glyph_brush::GlyphBrushBuilder<'a, H>,
///     some_config: bool,
/// }
///
/// impl<'a, H: BuildHasher> DownstreamGlyphBrushBuilder<'a, H> {
///     delegate_glyph_brush_builder_fns!(inner);
///
///     /// Sets some downstream configuration
///     pub fn some_config(mut self, some_config: bool) -> Self {
///         self.some_config = some_config;
///         self
///     }
///
///     // Must be manually delegated
///     pub fn section_hasher<T: BuildHasher>(
///         self,
///         section_hasher: T,
///     ) -> DownstreamGlyphBrushBuilder<'a, T> {
///         DownstreamGlyphBrushBuilder {
///             inner: self.inner.section_hasher(section_hasher),
///             some_config: self.some_config,
///         }
///     }
///
///     pub fn build(self) -> DownstreamGlyphBrush {
///         // ...
///         # DownstreamGlyphBrush
///     }
/// }
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! delegate_glyph_brush_builder_fns {
    ($inner:ident) => {
        /// Adds additional fonts to the one added in [`using_font`](#method.using_font) /
        /// [`using_font_bytes`](#method.using_font_bytes).
        /// Returns a [`FontId`](struct.FontId.html) to reference this font.
        pub fn add_font_bytes<B: Into<$crate::rusttype::SharedBytes<'a>>>(&mut self, font_data: B) -> $crate::FontId {
            self.$inner.add_font_bytes(font_data)
        }

        /// Adds additional fonts to the one added in [`using_font`](#method.using_font) /
        /// [`using_font_bytes`](#method.using_font_bytes).
        /// Returns a [`FontId`](struct.FontId.html) to reference this font.
        pub fn add_font(&mut self, font_data: $crate::rusttype::Font<'a>) -> $crate::FontId {
            self.$inner.add_font(font_data)
        }

        /// Initial size of 2D texture used as a gpu cache, pixels (width, height).
        /// The GPU cache will dynamically quadruple in size whenever the current size
        /// is insufficient.
        ///
        /// Defaults to `(256, 256)`
        pub fn initial_cache_size(mut self, size: (u32, u32)) -> Self {
            self.$inner = self.$inner.initial_cache_size(size);
            self
        }

        /// Sets the maximum allowed difference in scale used for judging whether to reuse an
        /// existing glyph in the GPU cache.
        ///
        /// Defaults to `0.5`
        ///
        /// See rusttype docs for `rusttype::gpu_cache::Cache`
        pub fn gpu_cache_scale_tolerance(mut self, tolerance: f32) -> Self {
            self.$inner = self.$inner.gpu_cache_scale_tolerance(tolerance);
            self
        }

        /// Sets the maximum allowed difference in subpixel position used for judging whether
        /// to reuse an existing glyph in the GPU cache. Anything greater than or equal to
        /// 1.0 means "don't care".
        ///
        /// Defaults to `0.1`
        ///
        /// See rusttype docs for `rusttype::gpu_cache::Cache`
        pub fn gpu_cache_position_tolerance(mut self, tolerance: f32) -> Self {
            self.$inner = self.$inner.gpu_cache_position_tolerance(tolerance);
            self
        }

        /// Sets whether perform the calculation of glyph positioning according to the layout
        /// every time, or use a cached result if the input `Section` and `GlyphPositioner` are the
        /// same hash as a previous call.
        ///
        /// Improves performance. Should only disable if using a custom GlyphPositioner that is
        /// impure according to it's inputs, so caching a previous call is not desired. Disabling
        /// also disables [`cache_glyph_drawing`](#method.cache_glyph_drawing).
        ///
        /// Defaults to `true`
        pub fn cache_glyph_positioning(mut self, cache: bool) -> Self {
            self.$inner = self.$inner.cache_glyph_positioning(cache);
            self
        }

        /// Sets optimising drawing by reusing the last draw requesting an identical draw queue.
        ///
        /// Improves performance. Is disabled if
        /// [`cache_glyph_positioning`](#method.cache_glyph_positioning) is disabled.
        ///
        /// Defaults to `true`
        pub fn cache_glyph_drawing(mut self, cache: bool) -> Self {
            self.$inner = self.$inner.cache_glyph_drawing(cache);
            self
        }
    }
}
