use std::{
    hash::Hash,
    iter::{FusedIterator, Peekable},
    str::{self, CharIndices},
};

/// Indicator that a character is a line break, soft or hard. Includes the offset (byte-index)
/// position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LineBreak {
    /// Soft line break (offset).
    Soft(usize),
    /// Hard line break (offset).
    Hard(usize),
}

impl LineBreak {
    /// Returns the offset of the line break, the index after the breaking character.
    #[inline]
    pub fn offset(&self) -> usize {
        match *self {
            LineBreak::Soft(offset) | LineBreak::Hard(offset) => offset,
        }
    }
}

type LineBreakIterator<'a> = xi_unicode::LineBreakIterator<'a>;
//type LineBreakIterator<'a> = LineBreakIter<'a>;
/*
#[inline]
pub fn line_breaks<'a>(text: &'a str) -> Box<dyn Iterator<Item = LineBreak> + 'a> {
    let mut unicode_breaker = LineBreakIterator::new(text);
    let current_break = unicode_breaker.next();

    Box::new(AnyCharLineBreakerIter {
        chars: text.char_indices(),
        breaks: unicode_breaker,
        current_break,
    })
}
*/
#[inline]
pub fn line_breaks<'a>(text: &'a str) -> Box<dyn Iterator<Item = LineBreak> + 'a> {
    Box::new(LineBreakIter::new(text).map(|(i, is_hard)|
        if is_hard {
            LineBreak::Hard(i)
        } else {
            LineBreak::Soft(i)
        }
    ))
}

// Iterator that indicates all characters are soft line breaks, except hard ones which are hard.
struct AnyCharLineBreakerIter<'a> {
    chars: CharIndices<'a>,
    breaks: LineBreakIterator<'a>,
    current_break: Option<(usize, bool)>,
}

impl Iterator for AnyCharLineBreakerIter<'_> {
    type Item = LineBreak;

    #[inline]
    fn next(&mut self) -> Option<LineBreak> {
        let (b_index, c) = self.chars.next()?;
        let c_len = c.len_utf8();
        while self.current_break.is_some() {
            if self.current_break.as_ref().unwrap().0 < b_index + c_len {
                self.current_break = self.breaks.next();
            } else {
                break;
            }
        }
        if let Some((break_index, true)) = self.current_break {
            if break_index == b_index + c_len {
                return Some(LineBreak::Hard(break_index));
            }
        }
        Some(LineBreak::Soft(b_index + c_len))
    }
}

impl FusedIterator for AnyCharLineBreakerIter<'_> {}

struct LineBreakIter<'a> {
    chars: Peekable<CharIndices<'a>>,
    len: usize,
    already_emitted_end: bool,
}

impl<'a> LineBreakIter<'a> {
    #[allow(dead_code)]
    /// Create a new iterator for the given string slice.
    pub fn new(s: &str) -> LineBreakIter {
        LineBreakIter {
            chars: s.char_indices().peekable(),
            len: s.len(),
            already_emitted_end: false,
        }
    }
}

impl Iterator for LineBreakIter<'_> {
    type Item = (usize, bool);

    #[inline]
    fn next(&mut self) -> Option<(usize, bool)> {
        loop {
            match self.chars.next() {
                None => {
                    return if self.already_emitted_end {
                        None
                    } else {
                        self.already_emitted_end = true;
                        Some((self.len, true))
                    };
                }
                Some((index, ch)) => {
                    if ch == '\r' {
                        let next_char = self.chars
                            .peek()
                            .map(|(_, c)| *c)
                            .unwrap_or('\0');
                        if next_char == '\n' {
                            let nl_i = self.chars.next().unwrap().0;
                            return Some((nl_i + 1, true));
                        } else {
                            return Some((index + 1, true));
                        }
                    } else if is_linebreak_char(ch) {
                        return Some((index + 1, true));
                    } else {
                        return Some((index, false));
                    }
                }
            }
        }
    }
}

impl FusedIterator for LineBreakIter<'_> {}

// TODO pull this out into a tiny crate to reduce duplication with panic_safe_rope
fn is_linebreak_char(c: char) -> bool {
    // ropey treats these as line breaks, so we do too.
    // See also https://www.unicode.org/reports/tr14/tr14-32.html
    (c >= '\u{a}' && c <= '\r')
        || c == '\u{0085}'
        || c == '\u{2028}'
        || c == '\u{2029}'
}