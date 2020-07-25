use std::{
    hash::Hash,
    iter::FusedIterator,
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

// Iterator that indicates all characters are soft line breaks, except hard ones which are hard.
struct AnyCharLineBreakerIter<'a> {
    chars: CharIndices<'a>,
    breaks: xi_unicode::LineBreakIterator<'a>,
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

#[inline]
pub fn line_breaks<'a>(text: &'a str) -> Box<dyn Iterator<Item = LineBreak> + 'a> {
    let mut unicode_breaker = xi_unicode::LineBreakIterator::new(text);
    let current_break = unicode_breaker.next();

    Box::new(AnyCharLineBreakerIter {
        chars: text.char_indices(),
        breaks: unicode_breaker,
        current_break,
    })
}