use super::*;
use std::{borrow::Cow, f32};

#[derive(Debug, Clone, PartialEq)]
pub struct OwnedVariedSection {
    /// Position on screen to render text, in pixels from top-left. Defaults to (0, 0).
    pub screen_position: (f32, f32),
    /// Max (width, height) bounds, in pixels from top-left. Defaults to unbounded.
    pub bounds: (f32, f32),
    /// Z values for use in depth testing. Defaults to 0.0
    pub z: f32,
    pub font_id: FontId,
    /// Text to render, rendered next to one another according the layout.
    pub text: Vec<OwnedSectionText>,
}

impl Default for OwnedVariedSection {
    fn default() -> Self {
        Self {
            screen_position: (0.0, 0.0),
            bounds: (f32::INFINITY, f32::INFINITY),
            z: 0.0,
            font_id: FontId::default(),
            text: vec![],
        }
    }
}

impl OwnedVariedSection {
    pub fn to_borrowed(&self) -> VariedSection<'_> {
        VariedSection {
            screen_position: self.screen_position,
            bounds: self.bounds,
            z: self.z,
            font_id: self.font_id,
            text: self.text.iter().map(|t| t.into()).collect(),
        }
    }
}

impl<'a> From<&'a OwnedVariedSection> for VariedSection<'a> {
    fn from(owned: &'a OwnedVariedSection) -> Self {
        owned.to_borrowed()
    }
}

impl<'a> From<&'a OwnedVariedSection> for Cow<'a, VariedSection<'a>> {
    fn from(owned: &'a OwnedVariedSection) -> Self {
        Cow::Owned(owned.to_borrowed())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OwnedSectionText {
    /// Text to render
    pub text: String,
    /// Position on screen to render text, in pixels from top-left. Defaults to (0, 0).
    pub scale: Scale,
    /// Rgba color of rendered text. Defaults to black.
    pub color: [f32; 4],
}

impl Default for OwnedSectionText {
    fn default() -> Self {
        Self {
            text: String::new(),
            scale: Scale::uniform(16.0),
            color: [0.0, 0.0, 0.0, 1.0],
        }
    }
}

impl<'a> From<&'a OwnedSectionText> for SectionText<'a> {
    fn from(owned: &'a OwnedSectionText) -> Self {
        Self {
            text: owned.text.as_str(),
            scale: owned.scale,
            color: owned.color,
        }
    }
}

impl From<&SectionText<'_>> for OwnedSectionText {
    fn from(st: &SectionText<'_>) -> Self {
        Self {
            text: st.text.into(),
            scale: st.scale,
            color: st.color,
        }
    }
}
