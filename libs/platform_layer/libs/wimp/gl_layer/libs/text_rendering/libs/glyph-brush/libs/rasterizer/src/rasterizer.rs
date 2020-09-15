#![deny(unused)]

#[cfg(not(any(feature = "rusttype", feature = "glyph_brush_draw_cache")))]
compile_error!("Either feature \"rusttype\" or \"glyph_brush_draw_cache\" must be enabled for this crate.");

#[cfg(feature = "rusttype")]
pub use rusttype::{
    Font, PositionedGlyph as Glyph, GlyphId, HMetrics,
    Rect, Scale, ScaledGlyph, VMetrics,
    point,
    gpu_cache::{Cache, CachedBy},
};

pub type Point = rusttype::Point<f32>;

#[cfg(feature = "rusttype")]
pub fn new_glyph<'font>(
    font: &Font<'font>,
    c: char,
    scale: Scale,
    position: Point
) -> Glyph<'font> {
    font.glyph(c).scaled(scale).positioned(position)
}

#[cfg(feature = "rusttype")]
pub fn has_bounding_box(glyph: &Glyph) -> bool {
    glyph.pixel_bounding_box().is_some()
}

#[cfg(feature = "rusttype")]
pub fn get_position(glyph: &Glyph) -> Point {
    glyph.position()
}

#[cfg(feature = "rusttype")]
pub fn set_position(glyph: &mut Glyph, pos: Point) {
    glyph.set_position(pos);
}

#[cfg(feature = "rusttype")]
pub fn add_position(glyph: &mut Glyph, position: Point) {
    let mut pos = glyph.position();

    pos.x += position.x;
    pos.y += position.y;

    glyph.set_position(pos);
}

#[cfg(feature = "rusttype")]
pub fn get_advance_width(glyph: &Glyph) -> f32 {
    glyph.unpositioned().h_metrics().advance_width
}

#[cfg(feature = "glyph_brush_draw_cache")]
pub use glyph_brush_draw_cache::{
    ab_glyph::{
        Font, Glyph, GlyphId, HMetrics, Point,
        Rect, PxScale as Scale, VMetrics,
        point,
    },
    DrawCache as Cache, CachedBy
};

#[cfg(feature = "glyph_brush_draw_cache")]
pub fn new_glyph(font: &Font, c: char, scale: Scale, position: Point) -> Glyph {
    Glyph {
        id: font.glyph_id(c),
        scale,
        position,
    }
}

#[cfg(feature = "glyph_brush_draw_cache")]
pub fn has_bounding_box(glyph: &Glyph) -> bool {
    glyph.pixel_bounding_box().is_some()
}

#[cfg(feature = "glyph_brush_draw_cache")]
pub fn get_position(glyph: &Glyph) -> Point {
    glyph.position
}

#[cfg(feature = "glyph_brush_draw_cache")]
pub fn set_position(glyph: &mut Glyph, pos: Point) {
    glyph.position = pos;
}

#[cfg(feature = "glyph_brush_draw_cache")]
pub fn add_position(glyph: &mut Glyph, position: Point) {
    glyph.position.x += position.x;
    glyph.position.x += position.y;
}

#[cfg(feature = "glyph_brush_draw_cache")]
pub fn get_advance_width(glyph: &Glyph) -> f32 {
    glyph.advance_width()
}
