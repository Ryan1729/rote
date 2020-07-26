use std::f32;

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
