//! Text layout for [rusttype](https://gitlab.redox-os.org/redox-os/rusttype).
mod builtin;
mod characters;
mod font;
mod linebreak;
mod lines;
mod section;
mod words;

pub use self::{builtin::*, font::*, linebreak::*, section::*, words::{RelativePositionedGlyph}};
use std::borrow::Cow;

/// Re-exported rusttype types.
pub mod rusttype {
    pub use full_rusttype::{
        point, vector, Error, Font, Glyph, GlyphId, HMetrics, Point, PositionedGlyph,
        Rect, Scale, ScaledGlyph, SharedBytes, VMetrics,
    };
}

//
// Hacked in by Ryan1729 for use in the rote project
//
pub use crate::lines::Lines;
#[perf_viz::record]
pub fn get_lines_iter<'a, 'b, 'font, F>(
    font_map: &'b F,
    sections: &'a [SectionText<'a>],
    bound_w: f32,
) -> Lines<'a, 'b, 'font, BuiltInLineBreaker, F>
where
    'font: 'a + 'b,
    F: FontMap<'font>,
{
    characters::Characters::new(
        font_map,
        sections.iter(),
        BuiltInLineBreaker::UnicodeLineBreaker,
    )
    .words()
    .lines(bound_w)
}

pub type UnboundedLine<'font> = 
    Vec<(RelativePositionedGlyph<'font>, [f32; 4], FontId)>
;

pub const V_METRICS_ZERO: VMetrics = VMetrics {
    ascent: 0.0,
    descent: 0.0,
    line_gap: 0.0,
};

pub const ZERO_V_METRICS: VMetrics = V_METRICS_ZERO;

pub fn get_unbounded_line_glyphs_iter<'a, 'b, 'font, F>(
        fonts: &'b F,
        sections: &'a [SectionText],
    ) -> impl IntoIterator<
        Item = UnboundedLine<'font>,
        IntoIter = std::vec::IntoIter<UnboundedLine<'font>>
    >
    where
        'font: 'a + 'b,
        F: FontMap<'font>, {
    let v: Vec<_> = UnboundedLines {
        words: Characters::new(
            fonts,
            sections.iter(),
            BuiltInLineBreaker::UnicodeLineBreaker,
        )
        .words()
        .peekable(),
    }.collect();

    v
}

use std::iter::Peekable;
struct UnboundedLines<'a, 'b, 'font, L, F>
where
    'font: 'a + 'b,
    L: LineBreaker,
    F: FontMap<'font>,
{
    pub(crate) words: Peekable<Words<'a, 'b, 'font, L, F>>,
}

impl<'font, L: LineBreaker, F: FontMap<'font>> Iterator for UnboundedLines<'_, '_, 'font, L, F> {
    type Item = UnboundedLine<'font>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut caret = vector(0.0, 0.0);
        let mut line: UnboundedLine = 
            Vec::new()
        ;
        let mut max_v_metrics = V_METRICS_ZERO;

        let mut progressed = false;

        while let Some(_) = self.words.peek() {
            let word = self.words.next().unwrap();
            progressed = true;

            
            if word.max_v_metrics.ascent > max_v_metrics.ascent {
                let diff_y = word.max_v_metrics.ascent - caret.y;
                caret.y += diff_y;

                // modify all smaller lined glyphs to occupy the new larger line
                for (glyph, ..) in &mut line {
                    glyph.relative.y += diff_y;
                }

                max_v_metrics = word.max_v_metrics;
            }

            line
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

/*type UnboundedWord<'font> = (
    Vec<(RelativePositionedGlyph<'font>, Color, FontId)>,
    VMetrics
);*/

pub(crate) struct Word<'font> {
    pub glyphs: Vec<(RelativePositionedGlyph<'font>, Color, FontId)>,
    /// pixel advance width of word includes ending spaces/invisibles
    pub layout_width: f32,
    /// pixel advance width of word not including any trailing spaces/invisibles
    pub layout_width_no_trail: f32,
    pub max_v_metrics: VMetrics,
    /// indicates the break after the word is a hard one
    pub hard_break: bool,
}

/// `Word` iterator.
pub(crate) struct Words<'a, 'b, 'font: 'a + 'b, L, F>
where
    L: LineBreaker,
    F: FontMap<'font>,
{
    pub(crate) characters: Characters<'a, 'b, 'font, L, F>,
}

impl<'font, L, F> Iterator for Words<'_, '_, 'font, L, F>
where
    L: LineBreaker,
    F: FontMap<'font>,
{
    type Item = Word<'font>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let mut glyphs = Vec::new();
        let mut caret = 0.0;
        let mut caret_no_trail = caret;
        let mut last_glyph_id = None;
        let mut max_v_metrics = None;
        let mut hard_break = false;
        let mut progress = false;

        for Character {
            glyph,
            color,
            font_id,
            line_break,
            control,
        } in &mut self.characters
        {
            progress = true;
            {
                let font = glyph.font().expect("standalone not supported");
                let v_metrics = font.v_metrics(glyph.scale());
                if max_v_metrics.is_none() || v_metrics > max_v_metrics.unwrap() {
                    max_v_metrics = Some(v_metrics);
                }

                if let Some(id) = last_glyph_id.take() {
                    caret += font.pair_kerning(glyph.scale(), id, glyph.id());
                }
                last_glyph_id = Some(glyph.id());
            }

            let advance_width = glyph.h_metrics().advance_width;

            if !control {
                let positioned = RelativePositionedGlyph {
                    relative: point(caret, 0.0),
                    glyph,
                };

                caret += advance_width;

                if positioned.bounds().is_some() {
                    glyphs.push((positioned, color, font_id));

                    // not an invisible trail
                    caret_no_trail = caret;
                }
            }

            if line_break.is_some() {
                if let Some(LineBreak::Hard(..)) = line_break {
                    hard_break = true
                }
                break;
            }
        }

        if progress {
            return Some(Word {
                glyphs,
                layout_width: caret,
                layout_width_no_trail: caret_no_trail,
                hard_break,
                max_v_metrics: max_v_metrics.unwrap_or(V_METRICS_ZERO),
            });
        }

        None
    }
}

/// `Character` iterator
pub(crate) struct Characters<'a, 'b, 'font, L, F>
where
    'font: 'a + 'b,
    L: LineBreaker,
    F: FontMap<'font>,
{
    font_map: &'b F,
    section_text: slice::Iter<'a, SectionText<'a>>,
    line_breaker: L,
    part_info: Option<PartInfo<'a>>,
    phantom: PhantomData<&'font ()>,
}

struct PartInfo<'a> {
    section: &'a SectionText<'a>,
    info_chars: CharIndices<'a>,
    line_breaks: Box<dyn Iterator<Item = LineBreak> + 'a>,
    next_break: Option<LineBreak>,
}

use std::{
    iter::{Iterator},
    marker::PhantomData,
    mem, slice,
    str::CharIndices,
};

impl<'a, 'b, 'font, L, F> Characters<'a, 'b, 'font, L, F>
where
    L: LineBreaker,
    F: FontMap<'font>,
{
    /// Returns a new `Characters` iterator.
    pub(crate) fn new(
        font_map: &'b F,
        section_text: slice::Iter<'a, SectionText<'a>>,
        line_breaker: L,
    ) -> Self {
        Self {
            font_map,
            section_text,
            line_breaker,
            part_info: None,
            phantom: PhantomData,
        }
    }

    /// Wraps into a `Words` iterator.
    pub(crate) fn words(self) -> Words<'a, 'b, 'font, L, F> {
        Words { characters: self }
    }
}

#[inline]
fn valid_section(s: &SectionText<'_>) -> bool {
    let Scale { x, y } = s.scale;
    x > 0.0 && y > 0.0
}

impl<'font, L, F> Iterator for Characters<'_, '_, 'font, L, F>
where
    L: LineBreaker,
    F: FontMap<'font>,
{
    type Item = Character<'font>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.part_info.is_none() {
            let mut section;
            loop {
                section = self.section_text.next()?;
                if valid_section(&section) {
                    break;
                }
            }
            let line_breaks = self.line_breaker.line_breaks(section.text);
            self.part_info = Some(PartInfo {
                section,
                info_chars: section.text.char_indices(),
                line_breaks,
                next_break: None,
            });
        }

        {
            let PartInfo {
                section:
                    SectionText {
                        scale,
                        color,
                        font_id,
                        text,
                    },
                info_chars,
                line_breaks,
                next_break,
            } = self.part_info.as_mut().unwrap();

            if let Some((byte_index, c)) = info_chars.next() {
                if next_break.is_none() || next_break.unwrap().offset() <= byte_index {
                    loop {
                        let next = line_breaks.next();
                        if next.is_none() || next.unwrap().offset() > byte_index {
                            mem::replace(next_break, next);
                            break;
                        }
                    }
                }

                let glyph = self.font_map.font(*font_id).glyph(c).scaled(*scale);

                let c_len = c.len_utf8();
                let mut line_break = next_break.filter(|b| b.offset() == byte_index + c_len);
                if line_break.is_some() && byte_index + c_len == text.len() {
                    // handle inherent end-of-str hard breaks
                    line_break = line_break.and(c.eol_line_break(&self.line_breaker));
                }

                return Some(Character {
                    glyph,
                    color: *color,
                    font_id: *font_id,
                    line_break,
                    control: c.is_control(),
                });
            }
        }

        self.part_info = None;
        self.next()
    }
}

/// Single character info
pub(crate) struct Character<'font> {
    pub glyph: ScaledGlyph<'font>,
    pub color: Color,
    pub font_id: FontId,
    /// Line break proceeding this character.
    pub line_break: Option<LineBreak>,
    /// Equivalent to `char::is_control()`.
    pub control: bool,
}


//
//
//

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
    /// Only the colors have changed (including alpha)
    Color,
    /// Only the alpha has changed
    Alpha,
    Unknown,
    #[doc(hidden)]
    __Nonexhaustive,
}
