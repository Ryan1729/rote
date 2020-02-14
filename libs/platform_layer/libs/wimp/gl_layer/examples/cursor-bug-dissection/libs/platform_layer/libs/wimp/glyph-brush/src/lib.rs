// This file has been modified for use in the `rote ` project.
mod glyph_brush;
mod glyph_calculator;
mod owned_section;
mod section;

pub use crate::{glyph_brush::*, glyph_calculator::*, owned_section::*, section::*};
pub use glyph_brush_layout::*;

use glyph_brush_layout::rusttype::*;

/// A "practically collision free" `Section` hasher
pub type DefaultSectionHasher = twox_hash::RandomXxHashBuilder;

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
