#![deny(unused)]

#[cfg(not(any(feature = "rusttype", feature = "glyph_brush_draw_cache")))]
compile_error!("Either feature \"rusttype\" or \"glyph_brush_draw_cache\" must be enabled for this crate.");

//
// rusttype
//

#[cfg(feature = "rusttype")]
pub use rusttype::{
    Font, PositionedGlyph as Glyph, GlyphId,
    Rect, Scale, ScaledGlyph,
    point,
    gpu_cache::{Cache, CachedBy},
};

#[cfg(feature = "rusttype")]
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
pub fn add_position(glyph: &mut Glyph, position: Point) {
    let mut pos = glyph.position();

    pos.x += position.x;
    pos.y += position.y;

    glyph.set_position(pos);
}

#[cfg(feature = "rusttype")]
pub fn get_scale(glyph: &Glyph) -> Scale {
    glyph.scale()
}

#[cfg(feature = "rusttype")]
pub fn get_advance_width(_: &Font, glyph: &Glyph) -> f32 {
    glyph.unpositioned().h_metrics().advance_width
}

#[cfg(feature = "rusttype")]
pub fn get_line_height(font: &Font, scale: Scale) -> f32 {
    let v_metrics = font.v_metrics(scale);
    v_metrics.ascent - v_metrics.descent + v_metrics.line_gap
}

//
// glyph_brush_draw_cache
//

#[cfg(feature = "glyph_brush_draw_cache")]
pub use glyph_brush_draw_cache::{
    ab_glyph::{
        FontVec as Font, Glyph, GlyphId, Point,
        Rect, PxScale as Scale,
        point,
    },
    DrawCache as Cache, CachedBy
};

#[cfg(feature = "glyph_brush_draw_cache")]
use glyph_brush_draw_cache::ab_glyph::{
    Font as _,
    ScaleFont as _,
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
pub fn add_position(glyph: &mut Glyph, position: Point) {
    glyph.position.x += position.x;
    glyph.position.x += position.y;
}

#[cfg(feature = "glyph_brush_draw_cache")]
pub fn get_scale(glyph: &Glyph) -> Scale {
    glyph.scale
}

#[cfg(feature = "glyph_brush_draw_cache")]
pub fn get_advance_width(font: &Font, glyph: &Glyph) -> f32 {
    font.as_scaled(glyph.scale).h_advance(glyph.id)
}

#[cfg(feature = "glyph_brush_draw_cache")]
pub fn get_line_height(font: &Font, scale: Scale) -> f32 {
    let v_metrics = font.v_metrics(scale);
    v_metrics.ascent - v_metrics.descent + v_metrics.line_gap
}
