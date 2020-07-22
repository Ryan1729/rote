use super::{Color, FontId, FontMap};
use crate::{
    linebreak::LineBreaker,
    words::{RelativePositionedGlyph, Words, ZERO_V_METRICS},
};
use full_rusttype::{point, vector, PositionedGlyph, VMetrics};
use std::iter::{FusedIterator, Iterator, Peekable};

/// A line of `Word`s limited to a max width bound.
pub struct Line<'font> {
    pub glyphs: Vec<(RelativePositionedGlyph<'font>, Color, FontId)>,
    pub max_v_metrics: VMetrics,
}

impl<'font> Line<'font> {
    #[inline]
    pub fn line_height(&self) -> f32 {
        self.max_v_metrics.ascent - self.max_v_metrics.descent + self.max_v_metrics.line_gap
    }

    /// Returns line glyphs positioned on the screen and aligned.
    pub fn aligned_on_screen(
        self,
        screen_position: (f32, f32),
    ) -> Vec<(PositionedGlyph<'font>, Color, FontId)> {
        if self.glyphs.is_empty() {
            return Vec::new();
        }

        let screen_pos = point(screen_position.0, screen_position.1);

        self.glyphs
            .into_iter()
            .map(|(glyph, color, font_id)| (glyph.screen_positioned(screen_pos), color, font_id))
            .collect()
    }
}

/// `Line` iterator.
///
/// Will iterator through `Word` until the next word would break the `width_bound`.
///
/// Note: Will always have at least one word, if possible, even if the word itself
/// breaks the `width_bound`.
pub struct Lines<'a, 'b, 'font, L, F>
where
    'font: 'a + 'b,
    L: LineBreaker,
    F: FontMap<'font>,
{
    pub(crate) words: Peekable<Words<'a, 'b, 'font, L, F>>,
    pub(crate) width_bound: f32,
}

impl<'font, L: LineBreaker, F: FontMap<'font>> Iterator for Lines<'_, '_, 'font, L, F> {
    type Item = Line<'font>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut caret = vector(0.0, 0.0);
        let mut line = Line {
            glyphs: Vec::new(),
            max_v_metrics: ZERO_V_METRICS,
        };

        let mut progressed = false;

        while let Some(word) = self.words.peek() {
            let word_in_bounds = {
                let word_x = caret.x + word.layout_width_no_trail;
                // Reduce float errors by using relative "<= width bound" check
                word_x < self.width_bound || approx::relative_eq!(word_x, self.width_bound)
            };

            // only if `progressed` means the first word is allowed to overlap the bounds
            if !word_in_bounds && progressed {
                break;
            }

            let word = self.words.next().unwrap();
            progressed = true;

            if word.max_v_metrics.ascent > line.max_v_metrics.ascent {
                let diff_y = word.max_v_metrics.ascent - caret.y;
                caret.y += diff_y;

                // modify all smaller lined glyphs to occupy the new larger line
                for (glyph, ..) in &mut line.glyphs {
                    glyph.relative.y += diff_y;
                }

                line.max_v_metrics = word.max_v_metrics;
            }

            line.glyphs
                .extend(word.glyphs.into_iter().map(|(mut g, color, font_id)| {
                    g.relative = g.relative + caret;
                    (g, color, font_id)
                }));

            caret.x += word.layout_width;

            if word.hard_break {
                break;
            }
        }

        Some(line).filter(|_| progressed)
    }
}

impl<'font, L: LineBreaker, F: FontMap<'font>> FusedIterator for Lines<'_, '_, 'font, L, F> {}
