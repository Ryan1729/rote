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

#[inline]
pub fn line_breaks<'a>(text: &'a str) -> LineBreakIter {
    LineBreakIter {
        chars: text.char_indices().peekable(),
        len: text.len(),
        already_emitted_end: false,
    }
}

pub struct LineBreakIter<'a> {
    chars: Peekable<CharIndices<'a>>,
    len: usize,
    already_emitted_end: bool,
}

impl Iterator for LineBreakIter<'_> {
    type Item = LineBreak;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        use LineBreak::*;
        match self.chars.next() {
            None => if self.already_emitted_end {
                None
            } else {
                self.already_emitted_end = true;
                Some(Hard(self.len))
            },
            Some((index, ch)) => if ch == '\r' {
                let next_char = self.chars
                    .peek()
                    .map(|(_, c)| *c)
                    .unwrap_or('\0');
                if next_char == '\n' {
                    let nl_i = self.chars.next().unwrap().0;
                    Some(Hard(nl_i + 1))
                } else {
                    Some(Hard(index + 1))
                }
            } else if is_linebreak_char(ch) {
                Some(Hard(index + 1))
            } else {
                Some(Soft(index + 1))
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