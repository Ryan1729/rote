// This file has been modified for use in the `rote ` project.

//! Text layout for [rusttype](https://gitlab.redox-os.org/redox-os/rusttype).
mod builtin;
mod characters;
mod font;
mod linebreak;
mod lines;
mod section;
mod words;

pub use self::{builtin::*, font::*, linebreak::*, section::*};
use std::borrow::Cow;

/// Re-exported rusttype types.
pub mod rusttype {
    pub use full_rusttype::{
        point, Error, Font, Glyph, GlyphId, HMetrics, Point, PositionedGlyph, Rect, Scale,
        ScaledGlyph, SharedBytes, VMetrics,
    };
}

use crate::rusttype::*;
use std::hash::Hash;

/// Logic to calculate glyph positioning using [`Font`](struct.Font.html),
/// [`SectionGeometry`](struct.SectionGeometry.html) and
/// [`SectionText`](struct.SectionText.html).
pub trait GlyphPositioner: Hash {
    /// Calculate a sequence of positioned glyphs to render. Custom implementations should
    /// return the same result when called with the same arguments to allow layout caching.
    fn calculate_glyphs<'font, F: FontMap<'font>>(
        &self,
        fonts: &F,
        geometry: &SectionGeometry,
        sections: &[SectionText<'_>],
    ) -> Vec<(PositionedGlyph<'font>, Color, FontId)>;

    /// Return a screen rectangle according to the requested render position and bounds
    /// appropriate for the glyph layout.
    fn bounds_rect(&self, geometry: &SectionGeometry) -> Rect<f32>;

    /// Recalculate a glyph sequence after a change.
    ///
    /// The default implementation simply calls `calculate_glyphs` so must be implemented
    /// to provide benefits as such benefits are spefic to the internal layout logic.
    fn recalculate_glyphs<'font, F>(
        &self,
        previous: Cow<'_, Vec<(PositionedGlyph<'font>, Color, FontId)>>,
        change: GlyphChange,
        fonts: &F,
        geometry: &SectionGeometry,
        sections: &[SectionText<'_>],
    ) -> Vec<(PositionedGlyph<'font>, Color, FontId)>
    where
        F: FontMap<'font>,
    {
        let _ = (previous, change);
        self.calculate_glyphs(fonts, geometry, sections)
    }
}

// #[non_exhaustive] TODO use when stable
#[derive(Debug)]
pub enum GlyphChange {
    /// Only the geometry has changed, contains the old geometry
    Geometry(SectionGeometry),
    /// Only the colors have changed
    Color,
    /// Only the alpha has changed
    Alpha,
    Unknown,
    #[doc(hidden)]
    __Nonexhaustive,
}
