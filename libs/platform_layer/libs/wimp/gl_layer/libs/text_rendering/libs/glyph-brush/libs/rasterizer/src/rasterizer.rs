#[cfg(not(any(feature = "rusttype", feature = "ab_glyph")))]
compile_error!("Either feature \"rusttype\" or \"ab_glyph\" must be enabled for this crate.");

#[cfg(feature = "rusttype")]
pub use rusttype::{
    Font, Glyph, GlyphId, HMetrics, Point, PositionedGlyph,
    Rect, Scale, ScaledGlyph, VMetrics,
    point,
};
#[cfg(feature = "rusttype")]
pub use rusttype::gpu_cache::{Cache, CachedBy};

#[cfg(feature = "ab_glyph")]
pub use ab_glyph::{ // TODO use real ab_glyph exports
    Font, Glyph, GlyphId, HMetrics, Point, PositionedGlyph,
    Rect, Scale, ScaledGlyph, VMetrics,
    point,
};
#[cfg(feature = "ab_glyph")]
pub use ab_glyph::gpu_cache::{Cache, CachedBy};
