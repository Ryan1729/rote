use editor_types::{ByteIndex, ByteLength, Cursor};
use macros::d;
use macros::invariant_assert;
use platform_types::{CharOffset, Position};
use std::borrow::Borrow;
use unicode_segmentation::UnicodeSegmentation;

#[cfg(test)]
macro_rules! tdbg {
    ($($args:tt)*) => (dbg!($($args)*));
}

#[cfg(not(test))]
macro_rules! tdbg {
    ($($args:tt)*) => {};
}

type Utf8Data = Vec<u8>; // must always be a valid utf8 string

#[derive(Debug)]
pub struct GapBuffer {
    data: Utf8Data,
    gap_start: ByteIndex,
    gap_length: ByteLength,
}

impl Default for GapBuffer {
    fn default() -> Self {
        GapBuffer::new(include_str!("../../../../../text/slipsum.txt").into())
    }
}

impl GapBuffer {
    pub fn new(data: String) -> GapBuffer {
        // If we made a valid buffer then called `insert_str` that would be slow for opening new
        // files. Here, we avoid copying the string.
        let mut bytes = data.into_bytes();
        let gap_start = ByteIndex(bytes.len());
        let mut gap_length = d!();
        Self::match_len_to_capacity(&mut bytes, gap_start, &mut gap_length);

        GapBuffer {
            data: bytes,
            gap_start,
            gap_length,
        }
    }

    // This is the index that a grapheme would be at if it was one past the last slot we have
    // allocated.
    fn capacity(data: &Utf8Data) -> ByteIndex {
        ByteIndex(data.capacity())
    }

    fn gap_end(&self) -> ByteIndex {
        ByteIndex(self.gap_start.0 + self.gap_length.0)
    }

    fn match_len_to_capacity(
        data: &mut Utf8Data,
        gap_start: ByteIndex,
        gap_length: &mut ByteLength,
    ) {
        // Update the tracked gap size and tell the vector that
        // we're using all of the new space immediately.
        let capacity = Self::capacity(data);
        *gap_length = ByteLength(capacity.0 - gap_start.0);
        unsafe {
            data.set_len(capacity.0);
        }
    }

    pub fn insert<P: Borrow<Position>>(&mut self, c: char, position: P) -> Option<()> {
        let mut stack_bytes = [0; 4];
        self.insert_str(c.encode_utf8(&mut stack_bytes), position)
    }

    pub fn insert_str<P: Borrow<Position>>(&mut self, data: &str, position: P) -> Option<()> {
        let position = position.borrow();
        // Ensure we have the capacity to insert this data.
        if data.len() > self.gap_length {
            // We're about to add space to the end of the buffer, so move the gap
            // there beforehand so that we're essentially just increasing the
            // gap size, and preventing a split/two-segment gap.
            self.move_gap(Self::capacity(&self.data));

            // Re-allocate the gap buffer, increasing its size.
            self.data.reserve(data.len());

            Self::match_len_to_capacity(&mut self.data, self.gap_start, &mut self.gap_length)
        }

        // You might be tempted to move this early return to the top of this method so we don't
        // run the lines above which potentially allocate, if the position is invalid. But if we
        // need to allocate then the index might be invalidated.
        let index = self.find_index(position)?;

        self.move_gap(index);

        for byte in data.bytes() {
            self.data[self.gap_start.0] = byte;
            self.gap_start += 1;
            self.gap_length -= 1;
        }

        Some(())
    }

    pub fn delete<P: Borrow<Position>>(&mut self, position: P) {
        let position = position.borrow();
        tdbg!(backward(self, *position)..=*position);
        self.delete_range(backward(self, *position)..=*position)
    }

    pub fn delete_range(&mut self, range: std::ops::RangeInclusive<Position>) {
        let start_index = match self.find_index(range.start()) {
            Some(o) => o,
            None => return,
        };

        self.move_gap(start_index);

        self.gap_length = match self.find_index(range.end()) {
            Some(offset) => {
                // Widen the gap to cover the deleted contents.
                offset - self.gap_start
            }
            None => {
                // The end of the range doesn't exist; check
                // if it's on the last line in the file.
                let start_of_next_line = Position {
                    line: range.end().line + 1,
                    offset: d!(),
                };

                match self.find_index(&start_of_next_line) {
                    Some(offset) => {
                        // There are other lines below this range.
                        // Just remove up until the end of the line.
                        offset - self.gap_start
                    }
                    None => {
                        // We're on the last line, just get rid of the rest
                        // by extending the gap right to the end of the buffer.
                        self.data.len() - self.gap_start
                    }
                }
            }
        }
        .into();
    }
}

macro_rules! return_valid_position_if_available {
    ($line:ident, $offset:ident, $pos:ident, $at_line_break:ident) => {
        if $line == $pos.line {
            if $offset == $pos.offset {
                return Some(*$pos);
            } else if $at_line_break {
                return Some(Position {
                    line: $line,
                    offset: $offset,
                });
            }
        }
    };
}

impl GapBuffer {
    #[perf_viz::record]
    pub fn nearest_valid_position_on_same_line<P: Borrow<Position>>(
        &self,
        position: P,
    ) -> Option<Position> {
        let pos = position.borrow();
        let mut line = 0;
        let mut offset = CharOffset(0);
        let mut at_line_break = false;

        let first_half = self.get_str(..self.gap_start.0);
        for grapheme in first_half.graphemes() {
            at_line_break = grapheme == "\n" || grapheme == "\r\n";

            return_valid_position_if_available!(line, offset, pos, at_line_break);

            // Advance the line and offset characters.
            if at_line_break {
                line += 1;
                offset = d!();
            } else {
                offset += 1;
            }
        }

        // We didn't find the position *within* the first half, but it could
        // be right after it, which means it's right at the start of the gap.
        return_valid_position_if_available!(line, offset, pos, at_line_break);

        // We haven't reached the position yet, so we'll move on to the other half.
        let second_half = self.get_str(self.gap_end().0..);
        for grapheme in second_half.graphemes() {
            at_line_break = grapheme == "\n" || grapheme == "\r\n";

            return_valid_position_if_available!(line, offset, pos, at_line_break);

            // Advance the line and offset characters.
            if at_line_break {
                line += 1;
                offset = d!();
            } else {
                offset += 1;
            }
        }

        // We didn't find the position *within* the second half, but it could
        // be right after it, which means it's at the end of the buffer.
        return_valid_position_if_available!(line, offset, pos, at_line_break);

        None
    }

    #[perf_viz::record]
    pub fn in_bounds<P: Borrow<Position>>(&self, position: P) -> bool {
        self.find_index(position) != None
    }

    /// Maps a position to its raw data index This can be affected by multi-`char` graphemes,
    /// so the result does **not** make sense to be assigned to a `Position`s offset field.
    /// Here's an example of where the distiction matters:
    /// Say someone has typed "ö" into the editor (without the quotes).
    /// "ö" is two characters: "o\u{308}"
    /// ```
    ///     assert_eq!("ö", "o\u{308}");
    /// ```
    /// So there should be now way to get a `GraphemeOffset` for the byte index between
    /// "o" and "\u{308}" from this method, for a given buffer.
    // TODO should we label these offsets with which buffer they are from? Can PhantomData do that?
    /// If the cursor is after the "ö" we want the `Position`'s offset to be `1`., not `2` so we
    /// can delete the "ö" with a single keystroke.
    #[perf_viz::record]
    pub fn find_index<P: Borrow<Position>>(&self, position: P) -> Option<ByteIndex> {
        let pos = position.borrow();
        let mut line = 0;
        let mut line_offset = 0;

        let first_half = self.get_str(..self.gap_start.0);

        for (index, grapheme) in first_half
            .grapheme_indices()
            .map(|(o, g)| (ByteIndex(o), g))
        {
            // Check to see if we've found the position yet.
            if line == pos.line && line_offset == pos.offset {
                return Some(index);
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
            return Some(self.gap_end());
        }

        // We haven't reached the position yet, so we'll move on to the other half.
        let second_half = self.get_str(self.gap_end().0..);
        for (index, grapheme) in second_half
            .grapheme_indices()
            .map(|(o, g)| (ByteIndex(o), g))
        {
            // Check to see if we've found the position yet.
            if line == pos.line && line_offset == pos.offset {
                return Some(self.gap_end() + index);
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
            return Some(ByteIndex(self.data.len()));
        }

        None
    }

    /// The character offset of the given position in the entre buffer. The output is suitable for
    /// passing into `self.graphemes().nth`.
    #[perf_viz::record]
    pub fn find_absolute_offset<P: Borrow<Position>>(&self, position: P) -> Option<CharOffset> {
        let pos = position.borrow();
        let mut line = 0;
        let mut line_offset = 0;
        let mut absolute_offset = CharOffset(0);

        let first_half = self.get_str(..self.gap_start.0);

        for grapheme in first_half.graphemes() {
            // Check to see if we've found the position yet.
            if line == pos.line && line_offset == pos.offset {
                return Some(absolute_offset);
            }

            // Advance the line and offset characters.
            if grapheme == "\n" || grapheme == "\r\n" {
                line += 1;
                line_offset = 0;
            } else {
                line_offset += 1;
            }

            absolute_offset += 1;
        }

        // We haven't reached the position yet, so we'll move on to the other half.
        let second_half = self.get_str(self.gap_end().0..);
        for grapheme in second_half.graphemes() {
            // Check to see if we've found the position yet.
            if line == pos.line && line_offset == pos.offset {
                return Some(absolute_offset);
            }

            // Advance the line and offset characters.
            if grapheme == "\n" || grapheme == "\r\n" {
                line += 1;
                line_offset = 0;
            } else {
                line_offset += 1;
            }

            absolute_offset += 1;
        }

        // We didn't find the position *within* the second half, but it could
        // be right after it, which means it's at the end of the buffer.
        if line == pos.line && line_offset == pos.offset {
            return Some(absolute_offset);
        }

        None
    }

    #[perf_viz::record]
    fn get_str<I>(&self, index: I) -> &str
    where
        I: std::slice::SliceIndex<[u8], Output = [u8]>,
    {
        invariant_assert!(str::from_utf8(&self.data[index]).is_ok());

        let minimize_unsafe = &self.data[index];

        unsafe { std::str::from_utf8_unchecked(minimize_unsafe) }
    }

    #[perf_viz::record]
    fn move_gap(&mut self, index: ByteIndex) {
        // We don't need to move any data if the buffer is at capacity.
        if self.gap_length == 0 {
            self.gap_start = index;
            return;
        }

        // TODO can we speed this up with `std::ptr::copy`? Seems like the alignment requirements
        // might make it too complicated. We should also do a benchmark beforehand so we can tell
        // if we would really be speeding things up.
        if index < self.gap_start.0 {
            // Shift the gap to the left one byte at a time.
            for i in (index.0..self.gap_start.0).rev() {
                self.data[i + self.gap_length.0] = self.data[i];
                self.data[i] = 0;
            }

            self.gap_start = index;
        } else if index > self.gap_start.0 {
            // Shift the gap to the right one byte at a time.
            for i in self.gap_end().0..index.0 {
                self.data[i - self.gap_length.0] = self.data[i];
                self.data[i] = 0;
            }

            // Because the index was after the gap, its value included the
            // gap length. We must remove it to determine the starting point.
            self.gap_start = ByteIndex(index.0 - self.gap_length.0);
        }
    }
}

//
// Probably only useful for debugging
//
#[allow(dead_code)]
impl GapBuffer {
    #[perf_viz::record]
    pub fn grapheme_before(&self, c: &Cursor) -> Option<&str> {
        let offset = self.find_absolute_offset(c);
        offset.and_then(|CharOffset(o)| {
            if o == 0 {
                None
            } else {
                self.graphemes().nth(o - 1)
            }
        })
    }

    #[perf_viz::record]
    pub fn grapheme_after(&self, c: &Cursor) -> Option<&str> {
        let offset = self.find_absolute_offset(c);
        offset.and_then(|CharOffset(o)| self.graphemes().nth(o))
    }

    #[perf_viz::record]
    pub fn grapheme_before_gap(&self) -> Option<&str> {
        let first_half = self.get_str(..self.gap_start.0);
        first_half.graphemes().next_back()
    }

    #[perf_viz::record]
    pub fn grapheme_after_gap(&self) -> Option<&str> {
        let second_half = self.get_str(self.gap_end().0..);
        second_half.graphemes().next()
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
            self.get_str(..self.gap_start.0),
            self.get_str((self.gap_start + self.gap_length).0..),
        )
    }

    pub fn graphemes(&'buffer self) -> impl Iterator<Item = &str> + 'buffer {
        chain_halves!(self=>graphemes)
    }

    pub fn grapheme_indices(&'buffer self) -> impl Iterator<Item = (usize, &str)> + 'buffer {
        chain_halves!(self=>grapheme_indices)
    }

    pub fn chars(&'buffer self) -> impl Iterator<Item = char> + 'buffer {
        chain_halves!(self=>chars)
    }
}

struct GapLines<'buffer> {
    next_of_first: Option<&'buffer str>,
    first_half: Option<std::str::Lines<'buffer>>,
    second_half: std::str::Lines<'buffer>,
}

// enum GapLinesState {
//     NextOfFirst(&'buffer str),
// }

impl<'buffer> Iterator for GapLines<'buffer> {
    type Item = GapLine<'buffer>;

    // TODO: fast nth
    // fn nth(mut n: usize) {
    //
    // }
    fn next(&mut self) -> Option<Self::Item> {
        None
        // match self.next_of_first {
        //     None => {
        //         let next = self.first_half.next();
        //         self.next_of_first = self.first_half.next().or_else(||);
        //         next
        //     },
        //     Some(next_of_first) => {
        //     match self.first_half.next() {
        //         Some(current) => {
        //             let next = self.first_half.next();
        //             if next.is_some() {
        //                 self.next_of_first = next;
        //                 Some(current)
        //             } else {
        //                 if current.ends_with("\n") {
        //                     self.second_half = Some(
        //                         self.buffer
        //                             .get_str((self.buffer.gap_start + self.buffer.gap_length).0..)
        //                             .lines(),
        //                     );
        //                     Some(current)
        //                 } else {
        //                     let mut second_half = self
        //                         .buffer
        //                         .get_str((self.buffer.gap_start + self.buffer.gap_length).0..)
        //                         .lines();
        //                     match second_half.next() {
        //                         None => {
        //                             self.second_half = Some(second_half);
        //                             Some(current)
        //                         }
        //                         Some(after_gap) => {
        //                             for gc in self.gap_cover {
        //                                 gc.extend(current.clone().chars());
        //                             }
        //                             for gc in self.gap_cover {
        //                                 gc.extend(after_gap.clone().chars());
        //                             }
        //                             self.gap_cover.as_ref().tak
        //                         }
        //                     }
        //                 }
        //             }
        //         }
        //         None => self.second_half.next(),
        //     }
        // }
    }
}

#[derive(Debug, PartialEq)]
pub enum GapLine<'buffer> {
    Connected(&'buffer str),
    Gapped(&'buffer str, &'buffer str),
}

impl<'buffer> GapLine<'buffer> {
    fn graphemes(&self) -> Box<Iterator<Item = &str> + 'buffer> {
        match self {
            GapLine::Connected(line) => Box::new(line.graphemes()),
            GapLine::Gapped(first, second) => Box::new(first.graphemes().chain(second.graphemes())),
        }
    }
}

impl<'buffer> GapBuffer {
    pub fn lines(&'buffer self) -> impl Iterator<Item = GapLine<'buffer>> + 'buffer {
        let (first_half, second_half) = self.get_halves();
        GapLines {
            next_of_first: None,
            first_half: Some(first_half.lines()),
            second_half: second_half.lines(),
        }
    }
}

impl GapBuffer {
    pub fn nth_line_count(&self, n: usize) -> Option<usize> {
        let (first_half, second_half) = self.get_halves();
        let mut first_half_lines = first_half.lines();
        match first_half_lines.nth(n) {
            Some(line) => {
                let count = line.graphemes().count();

                if let Some(_) = first_half_lines.next() {
                    Some(count)
                } else if first_half.ends_with("\n") {
                    Some(count)
                } else {
                    Some(
                        count
                            + second_half
                                .lines()
                                .next()
                                .map(|l| l.graphemes().count())
                                .unwrap_or_default(),
                    )
                }
            }
            None => {
                let left_over = n - first_half.lines().count();
                if left_over > 1 || first_half.ends_with("\n") {
                    second_half
                        .lines()
                        .nth(left_over)
                        .map(|l| l.graphemes().count())
                } else {
                    match (
                        first_half.lines().last().map(|l| l.graphemes().count()),
                        second_half.lines().next().map(|l| l.graphemes().count()),
                    ) {
                        (Some(first), Some(second)) => Some(first + second),
                        (Some(count), None) | (None, Some(count)) => Some(count),
                        (None, None) => None,
                    }
                }
            }
        }
    }

    pub fn last_line_index_and_count(&self) -> Option<(usize, usize)> {
        match self.lines().enumerate().last() {
            Some((index, GapLine::Connected(line))) => Some((index, line.graphemes().count())),
            Some((index, GapLine::Gapped(first, second))) => Some((
                index,
                first.graphemes().count() + second.graphemes().count(),
            )),
            None => d!(),
        }
    }
}

pub fn backward<P>(gap_buffer: &GapBuffer, position: P) -> Position
where
    P: Borrow<Position>,
{
    let position = position.borrow();

    if position.offset == 0 {
        if position.line == 0 {
            return *position;
        }
        let line = position.line.saturating_sub(1);
        Position {
            line,
            // TODO write tests to confirm this works correctly
            offset: CharOffset(
                gap_buffer
                    .lines()
                    .nth(line)
                    .map(|s| s.graphemes().count())
                    .unwrap_or_default(),
            ),
        }
    } else {
        Position {
            offset: position.offset - 1,
            ..*position
        }
    }
}

pub fn forward<P>(gap_buffer: &GapBuffer, position: P) -> Position
where
    P: Borrow<Position>,
{
    let position = position.borrow();

    let mut new = Position {
        offset: position.offset + 1,
        ..*position
    };

    //  we expect the rest of the system to bounds check on positions
    if !gap_buffer.in_bounds(&new) {
        new.line += 1;
        new.offset = d!();
    }

    new
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! init {
        ($str:literal) => {
            GapBuffer::new($str.to_string())
        };
    }

    macro_rules! p {
        ($buffer:ident) => {
            println!("{:?}", $buffer);
            println!("{:?}", $buffer.chars().collect::<String>())
        };
    }

    #[test]
    fn newline_bug_is_squished() {
        // "fix bug where if you add a newline in the middle of a line then move down and then to
        // the start of the line, then press delete twice, everything after the place where you
        // pressed enter is deleted"

        let mut buffer = init!("1234567890");

        p!(buffer);

        buffer.insert(
            '\n',
            Position {
                offset: CharOffset(5),
                ..d!()
            },
        );

        p!(buffer);

        // should delete inserted newline
        buffer.delete(Position {
            offset: CharOffset(0),
            line: 1,
        });

        p!(buffer);

        // should delete '0'
        buffer.delete(Position {
            offset: CharOffset(0),
            line: 1,
        });

        p!(buffer);

        assert_eq!(buffer.chars().collect::<String>(), "123456789");
    }

    #[test]
    fn deleting_past_the_end_does_nothing() {
        let mut buffer = init!("1234567890");

        buffer.delete(Position {
            offset: CharOffset(0),
            line: 1,
        });

        assert_eq!(buffer.chars().collect::<String>(), "1234567890");
    }

    #[test]
    fn backward_works_with_an_invalid_postion() {
        let mut buffer = init!("123467890");

        buffer.insert(
            '5',
            Position {
                offset: CharOffset(4),
                ..d!()
            },
        );

        assert_eq!(
            backward(
                &buffer,
                Position {
                    offset: CharOffset(0),
                    line: 1,
                },
            ),
            Position {
                offset: CharOffset(10),
                line: 0,
            }
        );
    }

    //
    //    LINES
    //

    #[test]
    fn lines_works_with_single_line_with_gap_at_start() {
        let mut buffer = init!("1234567890");
        buffer.move_gap(ByteIndex(0));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![GapLine::Connected("1234567890")]
        );
    }
    #[test]
    fn lines_works_with_single_line_with_gap_in_middle() {
        let mut buffer = init!("1234567890");
        buffer.move_gap(ByteIndex(5));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![GapLine::Gapped("12345", "67890")]
        );
    }
    #[test]
    fn lines_works_with_single_line_with_gap_at_end() {
        let mut buffer = init!("1234567890");
        buffer.move_gap(ByteIndex(10));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![GapLine::Connected("1234567890")]
        );
    }

    #[test]
    fn lines_works_with_two_lines_with_gap_at_start_of_first() {
        let mut buffer = init!("12345\n67890");
        buffer.move_gap(ByteIndex(0));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
        );
    }
    #[test]
    fn lines_works_with_two_lines_with_gap_in_midde_of_first() {
        let mut buffer = init!("12345\n67890");
        buffer.move_gap(ByteIndex(2));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![GapLine::Gapped("123", "45"), GapLine::Connected("67890")]
        );
    }
    #[test]
    fn lines_works_with_two_lines_with_gap_at_end_of_first() {
        let mut buffer = init!("12345\n67890");
        buffer.move_gap(ByteIndex(5));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
        );
    }

    #[test]
    fn lines_works_with_two_lines_with_gap_at_start_of_second() {
        let mut buffer = init!("12345\n67890");
        buffer.move_gap(ByteIndex(6));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
        );
    }
    #[test]
    fn lines_works_with_two_lines_with_gap_in_midde_of_second() {
        let mut buffer = init!("12345\n67890");
        buffer.move_gap(ByteIndex(8));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![GapLine::Connected("12345"), GapLine::Gapped("67", "890")]
        );
    }
    #[test]
    fn lines_works_with_two_lines_with_gap_at_end_of_second() {
        let mut buffer = init!("12345\n67890");
        buffer.move_gap(ByteIndex(11));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
        );
    }

    #[test]
    fn lines_works_with_three_lines_with_gap_at_start_of_first() {
        let mut buffer = init!("1234\n567\n890");
        buffer.move_gap(ByteIndex(0));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![
                GapLine::Connected("1234"),
                GapLine::Connected("567"),
                GapLine::Connected("890")
            ]
        );
    }
    #[test]
    fn lines_works_with_three_lines_with_gap_in_midde_of_first() {
        let mut buffer = init!("1234\n567\n890");
        buffer.move_gap(ByteIndex(2));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![
                GapLine::Gapped("12", "34"),
                GapLine::Connected("567"),
                GapLine::Connected("890")
            ]
        );
    }
    #[test]
    fn lines_works_with_three_lines_with_gap_at_end_of_first() {
        let mut buffer = init!("1234\n567\n890");
        buffer.move_gap(ByteIndex(4));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![
                GapLine::Connected("1234"),
                GapLine::Connected("567"),
                GapLine::Connected("890")
            ]
        );
    }

    #[test]
    fn lines_works_with_three_lines_with_gap_at_start_of_second() {
        let mut buffer = init!("1234\n567\n890");
        buffer.move_gap(ByteIndex(5));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![
                GapLine::Connected("1234"),
                GapLine::Connected("567"),
                GapLine::Connected("890")
            ]
        );
    }
    #[test]
    fn lines_works_with_three_lines_with_gap_in_midde_of_second() {
        let mut buffer = init!("1234\n567\n890");
        buffer.move_gap(ByteIndex(6));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![
                GapLine::Connected("1234"),
                GapLine::Gapped("5", "67"),
                GapLine::Connected("890")
            ]
        );
    }
    #[test]
    fn lines_works_with_three_lines_with_gap_at_end_of_second() {
        let mut buffer = init!("1234\n567\n890");
        buffer.move_gap(ByteIndex(8));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![
                GapLine::Connected("1234"),
                GapLine::Connected("567"),
                GapLine::Connected("890")
            ]
        );
    }

    #[test]
    fn lines_works_with_three_lines_with_gap_at_start_of_third() {
        let mut buffer = init!("1234\n567\n890");
        buffer.move_gap(ByteIndex(9));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![
                GapLine::Connected("1234"),
                GapLine::Connected("567"),
                GapLine::Connected("890")
            ]
        );
    }
    #[test]
    fn lines_works_with_three_lines_with_gap_in_midde_of_third() {
        let mut buffer = init!("1234\n567\n890");
        buffer.move_gap(ByteIndex(10));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![
                GapLine::Connected("1234"),
                GapLine::Connected("567"),
                GapLine::Gapped("8", "90")
            ]
        );
    }
    #[test]
    fn lines_works_with_three_lines_with_gap_at_end_of_third() {
        let mut buffer = init!("1234\n567\n890");
        buffer.move_gap(ByteIndex(12));
        assert_eq!(
            buffer.lines().collect::<Vec<_>>(),
            vec![
                GapLine::Connected("1234"),
                GapLine::Connected("567"),
                GapLine::Connected("890")
            ]
        );
    }
}
