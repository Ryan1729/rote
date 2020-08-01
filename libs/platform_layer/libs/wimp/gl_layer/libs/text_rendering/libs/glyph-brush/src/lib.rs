#![deny(unused)]
mod font;
mod glyph_brush;
mod glyph_calculator;
mod owned_section;
mod section;

pub use crate::{
    font::*,
    glyph_brush::*, 
    glyph_calculator::*, 
    owned_section::*,
    rusttype::*,
    section::*,
};

use std::borrow::Cow;
use std::hash::Hash;

/// A scaled glyph that's relatively positioned.
pub struct RelativePositionedGlyph<'font> {
    pub relative: Point<f32>,
    pub glyph: ScaledGlyph<'font>,
}

impl<'font> RelativePositionedGlyph<'font> {
    #[inline]
    pub fn bounds(&self) -> Option<Rect<f32>> {
        self.glyph.exact_bounding_box().map(|mut bb| {
            bb.min.x += self.relative.x;
            bb.min.y += self.relative.y;
            bb.max.x += self.relative.x;
            bb.max.y += self.relative.y;
            bb
        })
    }

    #[inline]
    pub fn screen_positioned(self, mut pos: Point<f32>) -> PositionedGlyph<'font> {
        pos.x += self.relative.x;
        pos.y += self.relative.y;
        self.glyph.positioned(pos)
    }
}

/// Re-exported rusttype types.
pub mod rusttype {
    pub use full_rusttype::{
        point, vector, Error, Font, Glyph, GlyphId, HMetrics, Point, PositionedGlyph,
        Rect, Scale, ScaledGlyph, SharedBytes, Vector, VMetrics,
    };
}

pub type CalculatedGlyph<'font> = (PositionedGlyph<'font>, Color);

/// Logic to calculate glyph positioning using [`Font`](struct.Font.html),
/// [`SectionGeometry`](struct.SectionGeometry.html) and
/// [`SectionText`](struct.SectionText.html).
pub trait GlyphPositioner: Hash {
    /// Calculate a sequence of positioned glyphs to render. Custom implementations should
    /// return the same result when called with the same arguments to allow layout caching.
    fn calculate_glyphs<'font>(
        &self,
        font: &Font<'font>,
        scale: Scale,
        geometry: &SectionGeometry,
        sections: &[SectionText<'_>],
    ) -> Vec<CalculatedGlyph<'font>>;

    /// Recalculate a glyph sequence after a change.
    ///
    /// The default implementation simply calls `calculate_glyphs` so must be implemented
    /// to provide benefits as such benefits are spefic to the internal layout logic.
    fn recalculate_glyphs<'font>(
        &self,
        previous: Cow<'_, Vec<CalculatedGlyph<'font>>>,
        change: GlyphChange,
        font: &Font<'font>,
        scale: Scale,
        geometry: &SectionGeometry,
        sections: &[SectionText<'_>],
    ) -> Vec<CalculatedGlyph<'font>>
    {
        let _ = (previous, change);
        self.calculate_glyphs(font, scale, geometry, sections)
    }
}

#[derive(Debug)]
pub enum GlyphChange {
    /// Only the geometry has changed, contains the old geometry
    Geometry(SectionGeometry),
    /// Only the colors have changed (including alpha)
    Color,
    /// Only the alpha has changed
    Alpha,
    Unknown,
}

/// A "practically collision free" `Section` hasher
#[cfg(not(target_arch = "wasm32"))]
pub type DefaultSectionHasher = twox_hash::RandomXxHashBuilder;
// Work around for rand issues in wasm #61
#[cfg(target_arch = "wasm32")]
pub type DefaultSectionHasher = std::hash::BuildHasherDefault<twox_hash::XxHash>;

#[test]
fn default_section_hasher() {
    use std::hash::{BuildHasher, Hash, Hasher};

    let section_a = Section {
        text: "Hovered Tile: Some((0, 0))",
        screen_position: (5.0, 60.0),
        scale: Scale { x: 20.0, y: 20.0 },
        color: [1.0, 1.0, 1.0, 1.0],
        ..<_>::default()
    };
    let section_b = Section {
        text: "Hovered Tile: Some((1, 0))",
        screen_position: (5.0, 60.0),
        scale: Scale { x: 20.0, y: 20.0 },
        color: [1.0, 1.0, 1.0, 1.0],
        ..<_>::default()
    };
    let hash = |s: &Section| {
        let s: VariedSection = s.into();
        let mut hasher = DefaultSectionHasher::default().build_hasher();
        s.hash(&mut hasher);
        hasher.finish()
    };
    assert_ne!(hash(&section_a), hash(&section_b));
}
