use super::{
    BuiltInLineBreaker, Color, FontId, FontMap, GlyphPositioner, LineBreaker, PositionedGlyph,
    Rect, SectionGeometry, SectionText,
};
use crate::{characters::Characters, rusttype::{point, Point}, GlyphChange};
use full_rusttype::vector;
use std::{borrow::Cow, mem};

/// Built-in [`GlyphPositioner`](trait.GlyphPositioner.html) implementations.
///
/// Takes generic [`LineBreaker`](trait.LineBreaker.html) to indicate the wrapping style.
/// See [`BuiltInLineBreaker`](enum.BuiltInLineBreaker.html).
///
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Layout<L: LineBreaker> {
    /// Renders a single line from left-to-right according to the inner alignment.
    /// Hard breaking will end the line, partially hitting the width bound will end the line.
    SingleLine {
        line_breaker: L,
        h_align: HorizontalAlign,
        v_align: VerticalAlign,
    },
    /// Renders multiple lines from left-to-right according to the inner alignment.
    /// Hard breaking characters will cause advancement to another line.
    /// A characters hitting the width bound will also cause another line to start.
    Wrap {
        line_breaker: L,
        h_align: HorizontalAlign,
        v_align: VerticalAlign,
    },
}

impl Default for Layout<BuiltInLineBreaker> {
    #[inline]
    fn default() -> Self {
        Layout::default_wrap()
    }
}

impl Layout<BuiltInLineBreaker> {
    #[inline]
    pub fn default_single_line() -> Self {
        Layout::SingleLine {
            line_breaker: BuiltInLineBreaker::default(),
            h_align: HorizontalAlign::Left,
            v_align: VerticalAlign::Top,
        }
    }

    #[inline]
    pub fn default_wrap() -> Self {
        Layout::Wrap {
            line_breaker: BuiltInLineBreaker::default(),
            h_align: HorizontalAlign::Left,
            v_align: VerticalAlign::Top,
        }
    }
}

impl<L: LineBreaker> Layout<L> {
    /// Returns an identical `Layout` but with the input `line_breaker`
    pub fn line_breaker<L2: LineBreaker>(self, line_breaker: L2) -> Layout<L2> {
        use crate::Layout::*;
        match self {
            SingleLine {
                h_align, v_align, ..
            } => SingleLine {
                line_breaker,
                v_align,
                h_align,
            },
            Wrap {
                h_align, v_align, ..
            } => Wrap {
                line_breaker,
                v_align,
                h_align,
            },
        }
    }
}

impl<L: LineBreaker> GlyphPositioner for Layout<L> {
    fn calculate_glyphs<'font, F: FontMap<'font>>(
        &self,
        font_map: &F,
        geometry: &SectionGeometry,
        sections: &[SectionText<'_>],
    ) -> Vec<(PositionedGlyph<'font>, Color, FontId)> {
        use crate::Layout::{SingleLine, Wrap};

        let SectionGeometry {
            screen_position,
            bounds: (bound_w, bound_h),
            ..
        } = *geometry;

        match *self {
            SingleLine {
                h_align,
                v_align,
                line_breaker,
            } => Characters::new(font_map, sections.iter(), line_breaker)
                .words()
                .lines(bound_w)
                .next()
                .map(|line| line.aligned_on_screen(screen_position, h_align, v_align))
                .unwrap_or_default(),

            Wrap {
                h_align,
                v_align,
                line_breaker,
            } => {
                let mut out = vec![];
                let mut caret = screen_position;
                let v_align_top = v_align == VerticalAlign::Top;

                let lines = Characters::new(font_map, sections.iter(), line_breaker)
                    .words()
                    .lines(bound_w);

                for line in lines {
                    // top align can bound check & exit early
                    if v_align_top && caret.1 >= screen_position.1 + bound_h {
                        break;
                    }

                    let line_height = line.line_height();
                    out.extend(line.aligned_on_screen(caret, h_align, VerticalAlign::Top));
                    caret.1 += line_height;
                }

                if !out.is_empty() {
                    match v_align {
                        // already aligned
                        VerticalAlign::Top => {}
                        // convert from top
                        VerticalAlign::Center | VerticalAlign::Bottom => {
                            let shift_up = if v_align == VerticalAlign::Center {
                                (caret.1 - screen_position.1) / 2.0
                            } else {
                                caret.1 - screen_position.1
                            };

                            let (min_x, max_x) = h_align.x_bounds(screen_position.0, bound_w);
                            let (min_y, max_y) = v_align.y_bounds(screen_position.1, bound_h);

                            // y-position and filter out-of-bounds glyphs
                            let shifted: Vec<_> = out
                                .drain(..)
                                .filter_map(|(mut g, color, font)| {
                                    let mut pos = g.position();
                                    pos.y -= shift_up;
                                    g.set_position(pos);
                                    Some((g, color, font)).filter(|(g, ..)| {
                                        g.pixel_bounding_box()
                                            .map(|bb| {
                                                bb.max.y as f32 >= min_y
                                                    && bb.min.y as f32 <= max_y
                                                    && bb.max.x as f32 >= min_x
                                                    && bb.min.x as f32 <= max_x
                                            })
                                            .unwrap_or(false)
                                    })
                                })
                                .collect();
                            mem::replace(&mut out, shifted);
                        }
                    }
                }

                out
            }
        }
    }

    #[allow(clippy::float_cmp)]
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
        match change {
            GlyphChange::Geometry(old) if old.bounds == geometry.bounds => {
                // position change
                let adjustment = vector(
                    geometry.screen_position.0 - old.screen_position.0,
                    geometry.screen_position.1 - old.screen_position.1,
                );

                let mut glyphs = previous.into_owned();
                for (glyph, ..) in &mut glyphs {
                    let new_pos = glyph.position() + adjustment;
                    glyph.set_position(new_pos);
                }

                glyphs
            }
            GlyphChange::Color if !sections.is_empty() && !previous.is_empty() => {
                let new_color = sections[0].color;
                if sections.iter().all(|s| s.color == new_color) {
                    // if only the color changed, but the new section only use a single color
                    // we can simply set all the olds to the new color
                    let mut glyphs = previous.into_owned();
                    for (_, color, ..) in &mut glyphs {
                        *color = new_color;
                    }
                    glyphs
                } else {
                    self.calculate_glyphs(fonts, geometry, sections)
                }
            }
            GlyphChange::Alpha if !sections.is_empty() && !previous.is_empty() => {
                let new_alpha = sections[0].color[3];
                if sections.iter().all(|s| s.color[3] == new_alpha) {
                    // if only the alpha changed, but the new section only uses a single alpha
                    // we can simply set all the olds to the new alpha
                    let mut glyphs = previous.into_owned();
                    for (_, color, ..) in &mut glyphs {
                        color[3] = new_alpha;
                    }
                    glyphs
                } else {
                    self.calculate_glyphs(fonts, geometry, sections)
                }
            }
            _ => self.calculate_glyphs(fonts, geometry, sections),
        }
    }
}

/// Describes horizontal alignment preference for positioning & bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HorizontalAlign {
    /// Leftmost character is immediately to the right of the render position.<br/>
    /// Bounds start from the render position and advance rightwards.
    Left,
    /// Leftmost & rightmost characters are equidistant to the render position.<br/>
    /// Bounds start from the render position and advance equally left & right.
    Center,
    /// Rightmost character is immetiately to the left of the render position.<br/>
    /// Bounds start from the render position and advance leftwards.
    Right,
}

impl HorizontalAlign {
    #[inline]
    pub fn x_bounds(self, screen_x: f32, bound_w: f32) -> (f32, f32) {
        let (min, max) = match self {
            HorizontalAlign::Left => (screen_x, screen_x + bound_w),
            HorizontalAlign::Center => (screen_x - bound_w / 2.0, screen_x + bound_w / 2.0),
            HorizontalAlign::Right => (screen_x - bound_w, screen_x),
        };

        (min.floor(), max.ceil())
    }
}

/// Describes vertical alignment preference for positioning & bounds. Currently a placeholder
/// for future functionality.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VerticalAlign {
    /// Characters/bounds start underneath the render position and progress downwards.
    Top,
    /// Characters/bounds center at the render position and progress outward equally.
    Center,
    /// Characters/bounds start above the render position and progress upward.
    Bottom,
}

impl VerticalAlign {
    #[inline]
    pub fn y_bounds(self, screen_y: f32, bound_h: f32) -> (f32, f32) {
        let (min, max) = match self {
            VerticalAlign::Top => (screen_y, screen_y + bound_h),
            VerticalAlign::Center => (screen_y - bound_h / 2.0, screen_y + bound_h / 2.0),
            VerticalAlign::Bottom => (screen_y - bound_h, screen_y),
        };

        (min.floor(), max.ceil())
    }
}