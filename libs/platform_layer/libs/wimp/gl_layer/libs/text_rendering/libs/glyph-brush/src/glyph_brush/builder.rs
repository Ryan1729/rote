use crate::{DefaultSectionHasher, Font, GlyphBrush};
use full_rusttype::gpu_cache::{Cache, CacheBuilder};
use std::hash::BuildHasher;

/// Builder for a [`GlyphBrush`](struct.GlyphBrush.html).
pub struct GlyphBrushBuilder<'a, H = DefaultSectionHasher> {
    font_data: Vec<Font<'a>>,
    section_hasher: H,
    gpu_cache_builder: CacheBuilder,
}

impl<'a> GlyphBrushBuilder<'a> {
    /// Create a new builder with a single font that will be used to render glyphs.
    /// Referenced with `FontId(0)`, which is default.
    pub fn using_font(font_0: Font<'a>) -> Self {
        Self::using_fonts(vec![font_0])
    }

    /// Create a new builder with multiple fonts.
    pub(crate) fn using_fonts<V: Into<Vec<Font<'a>>>>(fonts: V) -> Self {
        GlyphBrushBuilder {
            font_data: fonts.into(),
            section_hasher: DefaultSectionHasher::default(),
            gpu_cache_builder: Cache::builder()
                .dimensions(256, 256)
                .scale_tolerance(0.5)
                .position_tolerance(0.25)
                .align_4x4(false),
        }
    }
}

impl<'a, H: BuildHasher> GlyphBrushBuilder<'a, H> {
    /// Builds a `GlyphBrush` using the input gfx factory
    pub fn build<V>(self) -> GlyphBrush<'a, V, H> {
        GlyphBrush {
            fonts: self.font_data,
            texture_cache: self.gpu_cache_builder.build(),

            last_draw: <_>::default(),
            section_buffer: <_>::default(),
            calculate_glyph_cache: <_>::default(),

            last_frame_seq_id_sections: <_>::default(),
            frame_seq_id_sections: <_>::default(),

            keep_in_cache: <_>::default(),

            cache_glyph_positioning: true,
            cache_glyph_drawing: true,

            section_hasher: self.section_hasher,

            last_pre_positioned: <_>::default(),
            pre_positioned: <_>::default(),
        }
    }
}