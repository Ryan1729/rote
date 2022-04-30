#![deny(unused)]

#[cfg(not(any(feature = "rusttype", feature = "glyph_brush_draw_cache")))]
compile_error!("Either feature \"rusttype\" or \"glyph_brush_draw_cache\" must be enabled for this crate.");

/// RGBA `[0, 1]` colour data.
pub type Colour = [f32; 4];

/// A `TextureCoords` struct specifies floating point coordinates on the given
/// texture.
pub type TextureCoords = per_backend::Rect;

/// A `PixelCoords` struct specifies floating point coordinates on the screen
/// for the given glyph.
pub type PixelCoords = per_backend::Rect;

/// A `TextureRect` specifies a rectangular integer section of the given texture.
pub type TextureRect = per_backend::TextureRect;

pub struct Coords {
    pub texture: TextureCoords,
    pub pixel: PixelCoords,
}

pub use per_backend::*;

//
// rusttype
//
#[cfg(feature = "rusttype")]
mod per_backend {
    use crate::{Colour, Coords};

    pub use rusttype::{
        Font, PositionedGlyph as Glyph, GlyphId,
        Scale, ScaledGlyph,
        point,
        gpu_cache::{CachedBy, CacheReadErr, CacheWriteErr},
    };

    use rusttype::gpu_cache;

    pub type TextureRect = U32Rect;
    
    pub struct Cache<'font>{
        cache: gpu_cache::Cache<'font>,
    }
    
    pub fn new_cache<'font>() -> Cache<'font> {
        Cache {
            cache: gpu_cache::Cache::builder()
                .dimensions(256, 256)
                .scale_tolerance(0.5)
                .position_tolerance(0.25)
                .align_4x4(false)
                .build()
        }
    }
    
    pub fn queue_glyph<'font>(cache: &mut Cache<'font>, font_index: usize, glyph: Glyph<'font>) {
        cache.cache.queue_glyph(font_index, glyph);
    }
    
    pub fn cache_queued<'font, UpdateTexture>(
        cache: &mut Cache<'font>,
        _: &[Font],
        update_texture: UpdateTexture
    ) -> Result<CachedBy, CacheWriteErr>
    where for <'r> UpdateTexture: FnMut(TextureRect, &'r [u8]) {
        cache.cache.cache_queued(update_texture)
    }
    
    pub fn dimensions<'font>(cache: &Cache<'font>) -> (u32, u32) {
        cache.cache.dimensions()
    }
    
    pub fn rect_for(cache: &Cache<'_>, font_index: usize, glyph: &Glyph) -> Result<Option<Coords>, CacheReadErr> {
        cache.cache
            .rect_for(font_index, glyph)
            .map(|op| 
                op.map(|(texture, pixel)| Coords {
                    texture,
                    pixel: Rect {
                        min: point(pixel.min.x as f32, pixel.min.y as f32),
                        max: point(pixel.max.x as f32, pixel.max.y as f32),
                    },
                })
            )
    }
    
    pub fn resize_texture<'font>(
        cache: &mut Cache<'font>,
        new_width: u32,
        new_height: u32,
    ) {
        cache.cache
            .to_builder()
            .dimensions(new_width, new_height)
            .rebuild(&mut cache.cache);
    }
    
    pub type Point = rusttype::Point<f32>;
    pub type Rect = rusttype::Rect<f32>;
    type U32Rect = rusttype::Rect<u32>;
    
    #[derive(Clone, Debug)]
    pub struct CalculatedGlyph<'font> {
        pub glyph: Glyph<'font>,
        pub colour: Colour,
    }

    impl PartialEq for CalculatedGlyph<'_> {
        fn eq(&self, other: &Self) -> bool {
            self.colour == other.colour
            && self.glyph.id() == other.glyph.id()
            && self.glyph.position() == other.glyph.position()
            && self.glyph.scale() == other.glyph.scale()
        }
    }

    pub fn new_glyph<'font>(
        font: &Font<'font>,
        c: char,
        scale: Scale,
        position: Point
    ) -> Glyph<'font> {
        font.glyph(c).scaled(scale).positioned(position)
    }

    pub fn add_position(glyph: &mut Glyph, position: Point) {
        let mut pos = glyph.position();
    
        pos.x += position.x;
        pos.y += position.y;
    
        glyph.set_position(pos);
    }
    
    pub fn get_scale(glyph: &Glyph) -> Scale {
        glyph.scale()
    }
    
    pub fn get_advance_width(_: &Font, glyph: &Glyph) -> f32 {
        glyph.unpositioned().h_metrics().advance_width
    }
    
    pub fn get_line_height(font: &Font, scale: Scale) -> f32 {
        let v_metrics = font.v_metrics(scale);
        v_metrics.ascent - v_metrics.descent + v_metrics.line_gap
    }
    
    pub fn intersects(_: &Font, glyph: &Glyph, clip: &Rect) -> bool {
        glyph
            // TODO when is this None?
            .pixel_bounding_box()
            .map(move |pixel_coords| {
                // true if pixel_coords intersects clip
                pixel_coords.min.x as f32 <= clip.max.x
                && pixel_coords.min.y as f32 <= clip.max.y
                && clip.min.x <= pixel_coords.max.x as f32
                && clip.min.y <= pixel_coords.max.y as f32
            })
            .unwrap_or(true)
    }
}

//
// glyph_brush_draw_cache
//

#[cfg(feature = "glyph_brush_draw_cache")]
mod per_backend {
    use crate::{Colour, Coords};

    pub use glyph_brush_draw_cache::{
        ab_glyph::{
            GlyphId, Point,
            Rect, PxScale as Scale,
            point,
        },
        CachedBy,
        CacheWriteErr,
    };
    
    use glyph_brush_draw_cache::{
        ab_glyph::{
            self,
            Font as _,
            ScaleFont as _,
        },
        DrawCache,
        Rectangle,
    };
    
    use std::marker::PhantomData;

    pub type TextureRect = Rectangle<u32>;
    
    pub struct Cache<'font>{
        cache: DrawCache,
        // We'll need this before can compile under glyph_brush_draw_cache
        //cache: glyph_brush_draw_cache::DrawCache,
        allow_lifetime_param: PhantomData<&'font ()>,
    }

    pub fn new_cache<'font>() -> Cache<'font> {
        Cache {
            cache: DrawCache::builder()
                .dimensions(256, 256)
                .scale_tolerance(0.5)
                .position_tolerance(0.25)
                .align_4x4(false)
                .build(),
            allow_lifetime_param: PhantomData,
        }
    }
    
    pub fn queue_glyph<'font>(
        cache: &mut Cache<'font>,
        font_index: usize,
        glyph: Glyph<'font>
    ) {
        cache.cache.queue_glyph(font_index, glyph.glyph);
    }

    pub fn cache_queued<'font, UpdateTexture>(
        cache: &mut Cache<'font>,
        fonts: &[Font],
        update_texture: UpdateTexture
    ) -> Result<CachedBy, CacheWriteErr>
    where for <'r> UpdateTexture: FnMut(TextureRect, &'r [u8]) {
        cache.cache.cache_queued(fonts, update_texture)
    }
    
    pub fn dimensions<'font>(cache: &Cache<'font>) -> (u32, u32) {
        cache.cache.dimensions()
    }

    /// This has no variants because we're just trying to match the signature for 
    /// `rect_for`
    pub enum CacheReadErr {}

    impl core::fmt::Display for CacheReadErr {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            // This shouldn't be possible, but, famous last words...
            write!(f, "CacheReadErr")
        }
    }
    
    pub fn rect_for(cache: &Cache<'_>, font_index: usize, glyph: &Glyph) -> Result<Option<Coords>, CacheReadErr> {
        Ok(
            cache.cache
                .rect_for(font_index, &glyph.glyph)
                .map(|(texture, pixel)| Coords {
                    texture,
                    pixel,
                })
        )
    }

    pub fn resize_texture<'font>(
        cache: &mut Cache<'font>,
        new_width: u32,
        new_height: u32,
    ) {
        cache.cache
            .to_builder()
            .dimensions(new_width, new_height)
            .rebuild(&mut cache.cache);
    }
    
    pub struct Font<'font>{
        font: ab_glyph::FontVec,
        allow_lifetime_param: PhantomData<&'font ()>,
    }
    
    #[derive(Clone, Debug)]
    pub struct CalculatedGlyph<'font> {
        pub glyph: Glyph<'font>,
        pub colour: Colour,
    }

    impl PartialEq for CalculatedGlyph<'_> {
        fn eq(&self, other: &Self) -> bool {
            self.colour == other.colour
            && self.glyph.glyph == other.glyph.glyph
        }
    }

    #[derive(Clone, Debug)]
    pub struct Glyph<'font>{
        glyph: ab_glyph::Glyph,
        allow_lifetime_param: PhantomData<&'font ()>,
    }
    
    pub fn new_glyph<'font>(
        font: &Font<'font>,
        c: char,
        scale: Scale,
        position: Point,
    ) -> Glyph<'font> {
        Glyph {
            glyph: ab_glyph::Glyph {
                id: font.font.glyph_id(c),
                scale,
                position,
            },
            allow_lifetime_param: PhantomData,
        }
    }
    
    pub fn add_position(glyph: &mut Glyph, position: Point) {
        glyph.glyph.position.x += position.x;
        glyph.glyph.position.x += position.y;
    }
    
    pub fn get_scale(glyph: &Glyph) -> Scale {
        glyph.glyph.scale
    }
    
    pub fn get_advance_width(font: &Font, glyph: &Glyph) -> f32 {
        font.font.as_scaled(glyph.glyph.scale).h_advance(glyph.glyph.id)
    }
    
    pub fn get_line_height(font: &Font, scale: Scale) -> f32 {
        let scale_font = font.font.as_scaled(scale);
        scale_font.height() + scale_font.line_gap()
    }

    pub fn intersects(font: &Font, glyph: &Glyph, clip: &Rect) -> bool {
        let pixel_coords = font.glyph_bounds(&glyph.glyph);
        
        // true if pixel_coords intersects clip
        pixel_coords.min.x as f32 <= clip.max.x
        && pixel_coords.min.y as f32 <= clip.max.y
        && clip.min.x <= pixel_coords.max.x as f32
        && clip.min.y <= pixel_coords.max.y as f32
    }

    impl ab_glyph::Font for Font<'_> {
        fn units_per_em(&self) -> std::option::Option<f32> { 
            self.font.units_per_em()
        }
        fn ascent_unscaled(&self) -> f32 { 
            self.font.ascent_unscaled()
        }
        fn descent_unscaled(&self) -> f32 { 
            self.font.descent_unscaled()
        }
        fn line_gap_unscaled(&self) -> f32 { 
            self.font.line_gap_unscaled()
        }
        fn glyph_id(&self, c: char) -> glyph_brush_draw_cache::ab_glyph::GlyphId { 
            self.font.glyph_id(c)
        }
        fn h_advance_unscaled(&self, id: glyph_brush_draw_cache::ab_glyph::GlyphId) -> f32 { 
            self.font.h_advance_unscaled(id)
        }
        fn h_side_bearing_unscaled(&self, id: glyph_brush_draw_cache::ab_glyph::GlyphId) -> f32 { 
            self.font.h_side_bearing_unscaled(id)
        }
        fn v_advance_unscaled(&self, id: glyph_brush_draw_cache::ab_glyph::GlyphId) -> f32 { 
            self.font.v_advance_unscaled(id)
        }
        fn v_side_bearing_unscaled(&self, id: glyph_brush_draw_cache::ab_glyph::GlyphId) -> f32 { 
            self.font.v_side_bearing_unscaled(id)
        }
        fn kern_unscaled(&self, id_a: glyph_brush_draw_cache::ab_glyph::GlyphId, id_b: glyph_brush_draw_cache::ab_glyph::GlyphId) -> f32 { 
            self.font.kern_unscaled(id_a, id_b)
        }
        fn outline(&self, id: glyph_brush_draw_cache::ab_glyph::GlyphId) -> std::option::Option<glyph_brush_draw_cache::ab_glyph::Outline> { 
            self.font.outline(id)
        }
        fn glyph_count(&self) -> usize { 
            self.font.glyph_count()
        }
    }
}