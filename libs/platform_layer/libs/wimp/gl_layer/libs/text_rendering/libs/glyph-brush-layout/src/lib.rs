//! Text layout for [rusttype](https://gitlab.redox-os.org/redox-os/rusttype).
#![deny(unused)]
mod font;
mod section;

pub use self::{font::*, section::*};

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

use std::borrow::Cow;

/// Re-exported rusttype types.
pub mod rusttype {
    pub use full_rusttype::{
        point, vector, Error, Font, Glyph, GlyphId, HMetrics, Point, PositionedGlyph,
        Rect, Scale, ScaledGlyph, SharedBytes, Vector, VMetrics,
    };
}

use crate::rusttype::*;
use std::hash::Hash;

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
