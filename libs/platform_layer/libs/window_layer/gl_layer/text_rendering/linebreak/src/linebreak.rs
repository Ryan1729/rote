use is_linebreak_char::is_linebreak_char;

use std::{
    hash::Hash,
    iter::{FusedIterator, Peekable},
    str::{self, CharIndices},
};

/// Indicator that a character is a linebreak, soft or hard. Includes the byte index
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Linebreak {
    /// Soft linebreak (offset).
    Soft(usize),
    /// Hard linebreak (offset).
    Hard(usize),
}

impl Linebreak {
    /// Returns the offset of the linebreak, the index after the breaking character.
    #[must_use]
    #[inline]
    pub fn offset(&self) -> usize {
        match *self {
            Linebreak::Soft(offset) | Linebreak::Hard(offset) => offset,
        }
    }
}

#[must_use]
#[inline]
pub fn iter(text: &'_ str) -> LinebreakIter {
    LinebreakIter {
        chars: text.char_indices().peekable(),
        len: text.len(),
        already_emitted_end: false,
    }
}

pub struct LinebreakIter<'a> {
    chars: Peekable<CharIndices<'a>>,
    len: usize,
    already_emitted_end: bool,
}

impl Iterator for LinebreakIter<'_> {
    type Item = Linebreak;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {        
        use Linebreak::*;
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
                    .map_or('\0', |(_, c)| *c);
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

impl FusedIterator for LinebreakIter<'_> {}