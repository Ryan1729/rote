use std::{
    fmt,
    hash::Hash,
    iter::FusedIterator,
    str::{self, CharIndices},
};
use xi_unicode;

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

/// Producer of a [`LineBreak`](enum.LineBreak.html) iterator. Used to allow to the
/// [`Layout`](enum.Layout.html) to be line break aware in a generic way.
pub trait LineBreaker: fmt::Debug + Copy + Hash {
    fn line_breaks<'a>(&self, glyph_info: &'a str) -> Box<dyn Iterator<Item = LineBreak> + 'a>;
}

/// Built-in linebreaking logic.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BuiltInLineBreaker {
    /// LineBreaker that follows Unicode Standard Annex #14. That effectively means it
    /// wraps words in a way that should work for most cases.
    UnicodeLineBreaker,
    /// LineBreaker that soft breaks on any character, and hard breaks similarly to
    /// UnicodeLineBreaker.
    AnyCharLineBreaker,
}

impl Default for BuiltInLineBreaker {
    #[inline]
    fn default() -> Self {
        BuiltInLineBreaker::UnicodeLineBreaker
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
        let (b_index, _) = self.chars.next()?;
        while self.current_break.is_some() {
            if self.current_break.as_ref().unwrap().0 < b_index + 1 {
                self.current_break = self.breaks.next();
            } else {
                break;
            }
        }
        if let Some((break_index, true)) = self.current_break {
            if break_index == b_index + 1 {
                return Some(LineBreak::Hard(break_index));
            }
        }
        Some(LineBreak::Soft(b_index + 1))
    }
}

impl FusedIterator for AnyCharLineBreakerIter<'_> {}

impl LineBreaker for BuiltInLineBreaker {
    #[inline]
    fn line_breaks<'a>(&self, text: &'a str) -> Box<dyn Iterator<Item = LineBreak> + 'a> {
        match *self {
            BuiltInLineBreaker::UnicodeLineBreaker => Box::new(
                xi_unicode::LineBreakIterator::new(text).map(|(offset, hard)| {
                    if hard {
                        LineBreak::Hard(offset)
                    } else {
                        LineBreak::Soft(offset)
                    }
                }),
            ),
            BuiltInLineBreaker::AnyCharLineBreaker => {
                let mut unicode_breaker = xi_unicode::LineBreakIterator::new(text);
                let current_break = unicode_breaker.next();

                Box::new(AnyCharLineBreakerIter {
                    chars: text.char_indices(),
                    breaks: unicode_breaker,
                    current_break,
                })
            }
        }
    }
}

/// Line breakers can't easily tell the difference between the end of a slice being a hard
/// break and the last character being itself a hard or soft break. This trait allows testing
/// of eol characters being "true" eol line breakers.
pub(crate) trait EolLineBreak<B: LineBreaker> {
    fn eol_line_break(&self, line_breaker: &B) -> Option<LineBreak>;

    #[inline]
    fn is_eol_hard_break(&self, line_breaker: &B) -> bool {
        if let Some(LineBreak::Hard(..)) = self.eol_line_break(line_breaker) {
            return true;
        }
        false
    }

    #[inline]
    fn is_eol_soft_break(&self, line_breaker: &B) -> bool {
        if let Some(LineBreak::Soft(..)) = self.eol_line_break(line_breaker) {
            return true;
        }
        false
    }
}

impl<B: LineBreaker> EolLineBreak<B> for char {
    #[inline]
    fn eol_line_break(&self, line_breaker: &B) -> Option<LineBreak> {
        // to check if the previous end char (say '$') should hard break construct
        // a str "$ " an check if the line break logic flags a hard break at index 1
        let mut last_end_bytes: [u8; 5] = [b' '; 5];
        self.encode_utf8(&mut last_end_bytes);
        let len_utf8 = self.len_utf8();
        if let Ok(last_end_padded) = str::from_utf8(&last_end_bytes[0..=len_utf8]) {
            match line_breaker.line_breaks(last_end_padded).next() {
                l @ Some(LineBreak::Soft(1)) | l @ Some(LineBreak::Hard(1)) => return l,
                _ => {}
            }
        }

        // check for soft breaks using str "$a"
        last_end_bytes[len_utf8] = b'a';
        if let Ok(last_end_padded) = str::from_utf8(&last_end_bytes[0..=len_utf8]) {
            match line_breaker.line_breaks(last_end_padded).next() {
                l @ Some(LineBreak::Soft(1)) | l @ Some(LineBreak::Hard(1)) => return l,
                _ => {}
            }
        }

        None
    }
}

#[cfg(test)]
mod eol_line_break {
    use super::*;

    #[test]
    fn hard_break_char() {
        assert_eq!(
            '\n'.eol_line_break(&BuiltInLineBreaker::default()),
            Some(LineBreak::Hard(1))
        );
    }

    #[test]
    fn soft_break_char() {
        assert_eq!(
            ' '.eol_line_break(&BuiltInLineBreaker::default()),
            Some(LineBreak::Soft(1))
        );
    }
}
