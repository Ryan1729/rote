#[cfg(not(any(feature = "rusttype", feature = "glyph_brush_draw_cache")))]
compile_error!("Either feature \"rusttype\" or \"glyph_brush_draw_cache\" must be enabled for this crate.");

#[cfg(feature = "rusttype")]
pub use rusttype::{
    Font, Glyph, GlyphId, HMetrics, Point, PositionedGlyph,
    Rect, Scale, ScaledGlyph, VMetrics,
    point,
    gpu_cache::{Cache, CachedBy},
};

#[cfg(feature = "glyph_brush_draw_cache")]
pub use glyph_brush_draw_cache::{
    ab_glyph::{
        Font, Glyph, GlyphId, HMetrics, Point, PositionedGlyph,
        Rect, PxScale as Scale, ScaledGlyph, VMetrics,
        point,
    },
    DrawCache as Cache, CachedBy
};
