use editor_types::{Cursor, Position};
use macros::invariant_assert;
use std::borrow::Borrow;
use unicode_segmentation::UnicodeSegmentation;

pub struct GapBuffer {
    data: Vec<u8>, // must always be a valid utf8 string
    gap_start: usize,
    gap_length: usize,
}

impl Default for GapBuffer {
    fn default() -> Self {
        GapBuffer::new(include_str!("../../../../../text/slipsum.txt").into())
    }
}

impl GapBuffer {
    pub fn new(data: String) -> GapBuffer {
        let mut bytes = data.into_bytes();
        let capacity = bytes.capacity();
        let gap_start = bytes.len();
        let gap_length = capacity - gap_start;
        unsafe {
            bytes.set_len(capacity);
        }

        GapBuffer {
            data: bytes,
            gap_start,
            gap_length,
        }
    }

    pub fn insert(&mut self, c: char, position: &Position) -> Option<()> {
        let mut stack_bytes = [0; 4];
        self.insert_str(c.encode_utf8(&mut stack_bytes), position)
    }

    pub fn insert_str(&mut self, data: &str, position: &Position) -> Option<()> {
        // Ensure we have the capacity to insert this data.
        if data.len() > self.gap_length {
            // We're about to add space to the end of the buffer, so move the gap
            // there beforehand so that we're essentially just increasing the
            // gap size, and preventing a split/two-segment gap.
            self.move_gap(self.data.capacity());

            // Re-allocate the gap buffer, increasing its size.
            self.data.reserve(data.len());

            // Update the tracked gap size and tell the vector that
            // we're using all of the new space immediately.
            let capacity = self.data.capacity();
            self.gap_length = capacity - self.gap_start;
            unsafe {
                self.data.set_len(capacity);
            }
        }

        // You might be tempted to move this to the top of this method so we don't allocate if the
        // position is invalid, but if we need to allocate then the offset might be invalidated.
        let offset = self.find_offset(position)?;

        self.move_gap(offset);

        for byte in data.bytes() {
            self.data[self.gap_start] = byte;
            self.gap_start += 1;
            self.gap_length -= 1;
        }

        Some(())
    }

    pub fn delete(&mut self, position: &Position) {
        self.delete_range(backward(self, *position)..=*position)
    }

    pub fn delete_range(&mut self, range: std::ops::RangeInclusive<Position>) {
        let start_offset = match self.find_offset(range.start()) {
            Some(o) => o,
            None => return,
        };

        self.move_gap(start_offset);

        match self.find_offset(range.end()) {
            Some(offset) => {
                // Widen the gap to cover the deleted contents.
                self.gap_length = offset - self.gap_start;
            }
            None => {
                // The end of the range doesn't exist; check
                // if it's on the last line in the file.
                let start_of_next_line = Position {
                    line: range.end().line + 1,
                    offset: 0,
                };

                match self.find_offset(&start_of_next_line) {
                    Some(offset) => {
                        // There are other lines below this range.
                        // Just remove up until the end of the line.
                        self.gap_length = offset - self.gap_start;
                    }
                    None => {
                        // We're on the last line, just get rid of the rest
                        // by extending the gap right to the end of the buffer.
                        self.gap_length = self.data.len() - self.gap_start;
                    }
                }
            }
        };
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

        let first_half = self.get_str(..self.gap_start);

        for (offset, grapheme) in first_half.grapheme_indices(true) {
            // Check to see if we've found the position yet.
            if line == pos.line && line_offset == pos.offset {
                return Some(offset);
            }

            // Advance the line and offset characters.
            if grapheme == "\n" || grapheme == "\r\n" {
                line += 1;
                line_offset = 0;
            } else {
                line_offset += 1;
            }
        }

        // We didn't find the position *within* the first half, but it could
        // be right after it, which means it's right at the start of the gap.
        if line == pos.line && line_offset == pos.offset {
            return Some(self.gap_start + self.gap_length);
        }

        // We haven't reached the position yet, so we'll move on to the other half.
        let second_half = self.get_str(self.gap_start + self.gap_length..);
        for (offset, grapheme) in second_half.grapheme_indices(true) {
            // Check to see if we've found the position yet.
            if line == pos.line && line_offset == pos.offset {
                return Some(self.gap_start + self.gap_length + offset);
            }

            // Advance the line and offset characters.
            if grapheme == "\n" || grapheme == "\r\n" {
                line += 1;
                line_offset = 0;
            } else {
                line_offset += 1;
            }
        }

        // We didn't find the position *within* the second half, but it could
        // be right after it, which means it's at the end of the buffer.
        if line == pos.line && line_offset == pos.offset {
            return Some(self.data.len());
        }

        None
    }

    fn get_str<I>(&self, index: I) -> &str
    where
        I: std::slice::SliceIndex<[u8], Output = [u8]>,
    {
        invariant_assert!(str::from_utf8(&self.data[index]).is_ok());

        let minimize_unsafe = &self.data[index];

        unsafe { std::str::from_utf8_unchecked(minimize_unsafe) }
    }

    pub fn grapheme_before(&self, c: &Cursor) -> Option<&str> {
        let offset = self.find_offset(c.position);
        offset.and_then(|o| {
            if o == 0 {
                None
            } else {
                self.graphemes().nth(o - 1)
            }
        })
    }

    pub fn grapheme_after(&self, c: &Cursor) -> Option<&str> {
        let offset = self.find_offset(c.position);
        offset.and_then(|o| self.graphemes().nth(o))
    }

    fn move_gap(&mut self, offset: usize) {
        // We don't need to move any data if the buffer is at capacity.
        if self.gap_length == 0 {
            self.gap_start = offset;
            return;
        }

        // TODO can we speed this up with `std::ptr::copy`? Seems like the alignment requirements
        // might make it too complicated. We should also do a benchmark beforehand so we can tell
        // if we would really be speeding things up.
        if offset < self.gap_start {
            // Shift the gap to the left one byte at a time.
            for index in (offset..self.gap_start).rev() {
                self.data[index + self.gap_length] = self.data[index];
                self.data[index] = 0;
            }

            self.gap_start = offset;
        } else if offset > self.gap_start {
            // Shift the gap to the right one byte at a time.
            for index in self.gap_start + self.gap_length..offset {
                self.data[index - self.gap_length] = self.data[index];
                self.data[index] = 0;
            }

            // Because the offset was after the gap, its value included the
            // gap length. We must remove it to determine the starting point.
            self.gap_start = offset - self.gap_length;
        }
    }
}

macro_rules! chain_halves {
    ($self:expr=>$method:ident$(($params:tt))?) => {{
        let (first_half, second_half) = $self.get_halves();
        first_half.$method($($params)?).chain(second_half.$method($($params)?))
    }}
}

impl<'buffer> GapBuffer {
    pub fn get_halves(&'buffer self) -> (&'buffer str, &'buffer str) {
        (
            self.get_str(..self.gap_start),
            self.get_str(self.gap_start + self.gap_length..),
        )
    }

    pub fn lines(&'buffer self) -> impl Iterator<Item = &str> + 'buffer {
        chain_halves!(self=>lines)
    }

    pub fn graphemes(&'buffer self) -> impl Iterator<Item = &str> + 'buffer {
        chain_halves!(self=>graphemes(true))
    }

    pub fn grapheme_indices(&'buffer self) -> impl Iterator<Item = (usize, &str)> + 'buffer {
        chain_halves!(self=>grapheme_indices(true))
    }

    pub fn chars(&'buffer self) -> impl Iterator<Item = char> + 'buffer {
        chain_halves!(self=>chars)
    }
}

fn backward(gap_buffer: &GapBuffer, position: Position) -> Position {
    if position.offset == 0 {
        if position.line == 0 {
            return position;
        }
        let line = position.line.saturating_sub(1);
        Position {
            line,
            offset: gap_buffer
                .lines()
                .nth(line)
                .map(|s| s.graphemes(true).count())
                .unwrap_or_default(),
        }
    } else {
        Position {
            offset: position.offset - 1,
            ..position
        }
    }
}

fn forward(gap_buffer: &GapBuffer, position: Position) -> Position {
    let mut new = Position {
        offset: position.offset + 1,
        ..position
    };

    //  we expect the rest of the system to bounds check on positions
    if !gap_buffer.in_bounds(&new) {
        new.line += 1;
        new.offset = 0;
    }

    new
}
