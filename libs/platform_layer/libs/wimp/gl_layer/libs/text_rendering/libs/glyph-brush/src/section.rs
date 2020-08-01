use super::*;
use ordered_float::OrderedFloat;
use std::{borrow::Cow, f32, hash::*};

/// RGBA `[0, 1]` color data.
pub type Color = [f32; 4];

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SectionGeometry {
    /// Position on screen to render text, in pixels from top-left. Defaults to (0, 0).
    pub screen_position: (f32, f32),
    /// Max (width, height) bounds, in pixels from top-left. Defaults to unbounded.
    pub bounds: (f32, f32),
}

impl Default for SectionGeometry {
    #[inline]
    fn default() -> Self {
        Self {
            screen_position: (0.0, 0.0),
            bounds: (f32::INFINITY, f32::INFINITY),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SectionText<'a> {
    /// Text to render
    pub text: &'a str,
    /// Rgba color of rendered text. Defaults to black.
    pub color: Color,
}

impl Default for SectionText<'static> {
    #[inline]
    fn default() -> Self {
        Self {
            text: "",
            color: [0.0, 0.0, 0.0, 1.0],
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct VariedSection<'a> {
    /// Position on screen to render text, in pixels from top-left. Defaults to (0, 0).
    pub screen_position: (f32, f32),
    /// Max (width, height) bounds, in pixels from top-left. Defaults to unbounded.
    pub bounds: (f32, f32),
    /// Z values for use in depth testing. Defaults to 0.0
    pub z: f32,
    pub font_id: FontId,
    pub scale: Scale,
    /// Text to render, rendered next to one another according the layout.
    pub text: Vec<SectionText<'a>>,
}

impl Default for VariedSection<'static> {
    #[inline]
    fn default() -> Self {
        Self {
            screen_position: (0.0, 0.0),
            bounds: (f32::INFINITY, f32::INFINITY),
            z: 0.0,
            font_id: FontId::default(),
            scale: Scale::uniform(16.0),
            text: vec![],
        }
    }
}

impl<'a> From<VariedSection<'a>> for Cow<'a, VariedSection<'a>> {
    fn from(owned: VariedSection<'a>) -> Self {
        Cow::Owned(owned)
    }
}

impl<'a, 'b> From<&'b VariedSection<'a>> for Cow<'b, VariedSection<'a>> {
    fn from(owned: &'b VariedSection<'a>) -> Self {
        Cow::Borrowed(owned)
    }
}

impl Hash for VariedSection<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let VariedSection {
            screen_position: (screen_x, screen_y),
            bounds: (bound_w, bound_h),
            font_id,
            scale,
            z,
            ref text,
        } = *self;

        font_id.hash(state);

        let ord_floats: &[OrderedFloat<_>] = &[
            screen_x.into(),
            screen_y.into(),
            bound_w.into(),
            bound_h.into(),
            scale.x.into(),
            scale.y.into(),
            z.into(),
        ];

        hash_section_text(state, text);

        ord_floats.hash(state);
    }
}

#[inline]
fn hash_section_text<H: Hasher>(state: &mut H, text: &[SectionText]) {
    for t in text {
        let SectionText {
            text,
            color,
        } = *t;

        let ord_floats: &[OrderedFloat<_>] = &[
            color[0].into(),
            color[1].into(),
            color[2].into(),
            color[3].into(),
        ];

        (text, ord_floats).hash(state);
    }
}

impl<'text> VariedSection<'text> {
    pub(crate) fn to_hashable_parts(&self) -> HashableVariedSectionParts<'_> {
        let VariedSection {
            screen_position: (screen_x, screen_y),
            bounds: (bound_w, bound_h),
            z,
            ref text,
            ..
        } = *self;

        let geometry = [
            screen_x.into(),
            screen_y.into(),
            bound_w.into(),
            bound_h.into(),
        ];

        HashableVariedSectionParts {
            geometry,
            z: z.into(),
            text,
        }
    }
}

impl From<&VariedSection<'_>> for SectionGeometry {
    fn from(section: &VariedSection<'_>) -> Self {
        Self {
            bounds: section.bounds,
            screen_position: section.screen_position,
        }
    }
}

/// An object that contains all the info to render a section of text.
///
/// For varied font/scale/color sections see [`VariedSection`](struct.VariedSection.html).
///
/// # Example
///
/// ```
/// use glyph_brush::Section;
///
/// let section = Section {
///     text: "Hello glyph_brush",
///     ..Section::default()
/// };
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Section<'a> {
    /// Text to render
    pub text: &'a str,
    /// Position on screen to render text, in pixels from top-left. Defaults to (0, 0).
    pub screen_position: (f32, f32),
    /// Max (width, height) bounds, in pixels from top-left. Defaults to unbounded.
    pub bounds: (f32, f32),
    /// Font scale. Defaults to 16
    pub scale: Scale,
    /// Rgba color of rendered text. Defaults to black.
    pub color: [f32; 4],
    /// Z values for use in depth testing. Defaults to 0.0
    pub z: f32,
    /// Font id to use for this section.
    ///
    /// It must be known to the `GlyphBrush` it is being used with,
    /// either `FontId::default()` or the return of
    /// [`add_font`](struct.GlyphBrushBuilder.html#method.add_font).
    pub font_id: FontId,
}

impl Default for Section<'static> {
    #[inline]
    fn default() -> Self {
        Self {
            text: "",
            screen_position: (0.0, 0.0),
            bounds: (f32::INFINITY, f32::INFINITY),
            scale: Scale::uniform(16.0),
            color: [0.0, 0.0, 0.0, 1.0],
            z: 0.0,
            font_id: FontId::default(),
        }
    }
}

impl<'a> From<&Section<'a>> for VariedSection<'a> {
    fn from(s: &Section<'a>) -> Self {
        let Section {
            text,
            scale,
            color,
            screen_position,
            bounds,
            z,
            font_id,
        } = *s;

        VariedSection {
            text: vec![SectionText {
                text,
                color,
            }],
            screen_position,
            bounds,
            font_id,
            scale,
            z,
        }
    }
}

impl<'a> From<Section<'a>> for VariedSection<'a> {
    fn from(s: Section<'a>) -> Self {
        VariedSection::from(&s)
    }
}

impl<'a> From<Section<'a>> for Cow<'a, VariedSection<'a>> {
    fn from(section: Section<'a>) -> Self {
        Cow::Owned(VariedSection::from(section))
    }
}

impl<'a> From<&Section<'a>> for Cow<'a, VariedSection<'a>> {
    fn from(section: &Section<'a>) -> Self {
        Cow::Owned(VariedSection::from(section))
    }
}

pub(crate) struct HashableVariedSectionParts<'a> {
    geometry: [OrderedFloat<f32>; 4],
    z: OrderedFloat<f32>,
    text: &'a [SectionText<'a>],
}

impl HashableVariedSectionParts<'_> {
    #[inline]
    pub fn hash_geometry<H: Hasher>(&self, state: &mut H) {
        self.geometry.hash(state);
    }

    #[inline]
    pub fn hash_z<H: Hasher>(&self, state: &mut H) {
        self.z.hash(state);
    }

    #[inline]
    pub fn hash_text_no_color<H: Hasher>(&self, state: &mut H) {
        for t in self.text {
            let SectionText {
                text,
                ..
            } = *t;

            text.hash(state);
        }
    }

    #[inline]
    pub fn hash_alpha<H: Hasher>(&self, state: &mut H) {
        for t in self.text {
            OrderedFloat(t.color[3]).hash(state);
        }
    }

    #[inline]
    pub fn hash_color<H: Hasher>(&self, state: &mut H) {
        for t in self.text {
            let color = t.color;

            let ord_floats: &[OrderedFloat<_>] =
                &[color[0].into(), color[1].into(), color[2].into()];

            ord_floats.hash(state);
        }
    }
}
