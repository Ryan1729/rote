use editor_types::{Cursor, Position};
use platform_types::Move;
use std::borrow::Borrow;
use unicode_segmentation::UnicodeSegmentation;

//TODO replace with a real Gap Buffer
pub struct GapBuffer {
    data: String,
}

impl Default for GapBuffer {
    fn default() -> Self {
        GapBuffer {
            data: include_str!("../../../../../text/slipsum.txt").into(),
        }
    }
}

impl GapBuffer {
    pub fn insert(&mut self, c: char) {
        self.data.push(c);
    }

    pub fn delete(&mut self) {
        self.data.pop();
    }

    pub fn in_bounds(&self, position: &Position) -> bool {
        self.find_offset(position) != None
    }

    /// Maps a position to its raw data index This can be affected by multi-`char` graphemes,
    /// so the result does **not** make sense to be assigned to a `Position`s offset field.
    //TODO make "GraphemeAwareOffset" newtype? Or maybe we should make the position offset a newtype?
    pub fn find_offset<P: Borrow<Position>>(&self, position: P) -> Option<usize> {
        let pos = position.borrow();
        let mut line = 0;
        let mut line_offset = 0;

        for (offset, grapheme) in self.data.grapheme_indices(true) {
            if line == pos.line && line_offset == pos.offset {
                return Some(offset);
            }

            if grapheme == "\n" || grapheme == "\r\n" {
                line += 1;
                line_offset = 0;
            } else {
                line_offset += 1;
            }
        }

        // there's the extra space after the end of the buffer, which *is* a valid insertion point.
        if line == pos.line && line_offset == pos.offset {
            return Some(self.data.len());
        }

        None
    }

    pub fn grapheme_before(&self, c: &Cursor) -> Option<&str> {
        let offset = self.find_offset(c.position);
        offset.and_then(|o| {
            if o == 0 {
                None
            } else {
                self.data.graphemes(true).nth(o - 1)
            }
        })
    }

    pub fn grapheme_after(&self, c: &Cursor) -> Option<&str> {
        let offset = self.find_offset(c.position);
        offset.and_then(|o| self.data.graphemes(true).nth(o))
    }
}

impl<'buffer> GapBuffer {
    pub fn lines(&'buffer self) -> impl Iterator<Item = &str> + 'buffer {
        self.data.lines()
    }

    pub fn chars(&'buffer self) -> impl Iterator<Item = char> + 'buffer {
        self.data.chars()
    }
}
