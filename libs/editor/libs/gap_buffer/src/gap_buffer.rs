use editor_types::{ByteIndex, ByteLength, Cursor};
use macros::{d, fmt_debug, integer_newtype, invariant_assert, usize_newtype};
use platform_types::{append_positions, unappend_positions, CharOffset, Position};
use sorted::{get_tree_bounds_by, Sorted};
use std::borrow::Borrow;
use std::num::NonZeroUsize;
use std::ops::{Add, Sub};
use std::ops::{Bound, RangeBounds};
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

// An index into the buffer that assumes the gap size is zero.
// In otherwords this is a byte index into the string that the buffer represents
#[derive(Clone, Copy, Debug, Default, Hash)]
pub struct GapObliviousByteIndex(pub usize);

integer_newtype! {
    GapObliviousByteIndex
}

usize_newtype! {
    GapObliviousByteIndex
}

fn inform_of_gap(
    GapObliviousByteIndex(i): GapObliviousByteIndex,
    GapBuffer {
        gap_start,
        gap_length,
        ..
    }: &GapBuffer,
) -> ByteIndex {
    ByteIndex(if i <= gap_start.0 {
        i
    } else {
        i + gap_length.0
    })
}

fn remove_gap_knowledge(
    ByteIndex(i): ByteIndex,
    GapBuffer {
        gap_start,
        gap_length,
        ..
    }: &GapBuffer,
) -> GapObliviousByteIndex {
    GapObliviousByteIndex(if i <= gap_start.0 {
        i
    } else {
        i - gap_length.0
    })
}

#[macro_export]
macro_rules! gap_informed {
    ($index:literal, $buffer:ident) => {
        inform_of_gap(GapObliviousByteIndex($index), &$buffer)
    };
}

#[macro_export]
macro_rules! cached_offset {
    (l $line:literal o $offset:literal i $index:literal) => {
        CachedOffset {
            position: Position {
                line: $line,
                offset: CharOffset($offset),
            },
            index: GapObliviousByteIndex($index),
        }
    };
    (p: $position:expr, i $index:literal) => {
        CachedOffset {
            position: $position,
            index: GapObliviousByteIndex($index),
        }
    };
    () => {
        CachedOffset::default()
    };
}

#[derive(Clone, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CachedOffset {
    position: Position,
    index: GapObliviousByteIndex,
}

fmt_debug! {
    for CachedOffset :
    CachedOffset { position: Position{ line, offset}, index: GapObliviousByteIndex(index) } in
    "cached_offset!{{ l {} o {} i {} }}", line, offset, index
}

/// Semantically this is `append_positions` but with the idexes accounted for too.
pub fn append_offsets(left: CachedOffset, right: CachedOffset) -> CachedOffset {
    CachedOffset {
        position: append_positions(left.position, right.position),
        index: left.index + right.index,
    }
}

/// THe inverse of `append_offsets`. That is,
/// `unappend_offsets(append_offsets(o, p), p) == o`
// TODO proptest this property
pub fn unappend_offsets(left: CachedOffset, right: CachedOffset) -> CachedOffset {
    CachedOffset {
        position: unappend_positions(left.position, right.position),
        index: left.index - right.index,
    }
}

#[derive(Debug, PartialEq)]
struct OffsetCache {
    offsets: Sorted<CachedOffset>,
    block_size: NonZeroUsize,
}

d!(for OffsetCache : OffsetCache {
    offsets: d!(),
    block_size: DEFAULT_BLOCK_SIZE,
});

impl OffsetCache {
    pub fn iter(&self) -> std::slice::Iter<CachedOffset> {
        self.offsets.iter()
    }

    pub fn len(&self) -> usize {
        self.offsets.len()
    }

    pub fn insert(&mut self, index: usize, offset: CachedOffset) {
        //TODO do block_size respecting insertion
        self.offsets.insert(offset);
    }
}

impl std::borrow::Borrow<Sorted<CachedOffset>> for OffsetCache {
    fn borrow(&self) -> &Sorted<CachedOffset> {
        &self.offsets
    }
}

impl std::borrow::Borrow<Sorted<CachedOffset>> for &OffsetCache {
    fn borrow(&self) -> &Sorted<CachedOffset> {
        &self.offsets
    }
}

fn optimal_offset_cache_from_all_cached_offsets(
    all_cached_offsets: Vec<CachedOffset>,
    block_size: NonZeroUsize,
) -> OffsetCache {
    let size = block_size.get();
    let len = all_cached_offsets.len();
    let minimum_len = if len <= 1 {
        len
    } else {
        std::cmp::max(len / size, 1)
    };

    let mut output = Vec::with_capacity(minimum_len);

    for chunk in all_cached_offsets.chunks(size) {
        if let Some(last) = chunk.last() {
            output.push((*last).clone());
        }
    }

    OffsetCache {
        offsets: output.into(),
        block_size,
    }
}

#[derive(Debug)]
pub struct GapBuffer {
    data: Utf8Data,
    gap_start: ByteIndex,
    gap_length: ByteLength,
    offset_cache: OffsetCache,
}

// TODO tune this.
const DEFAULT_BLOCK_SIZE: NonZeroUsize = unsafe { NonZeroUsize::new_unchecked(4096) };

impl Default for GapBuffer {
    fn default() -> Self {
        GapBuffer::new(include_str!("../../../../../text/slipsum.txt").into())
    }
}

macro_rules! advance_position_based_on_grapheme {
    ($current_pos: ident, $grapheme: ident) => {
        if $grapheme == "\n" || $grapheme == "\r\n" {
            $current_pos.line += 1;
            $current_pos.offset = d!();
        } else {
            $current_pos.offset += 1;
        }
    };
}

macro_rules! advance_cached_offset_based_on_grapheme {
    ($cached_offset: ident, $grapheme: ident) => {{
        let position = &mut $cached_offset.position;
        advance_position_based_on_grapheme!(position, $grapheme);
        $cached_offset.index += $grapheme.len();
    }};
}

impl GapBuffer {
    pub fn new(data: String) -> GapBuffer {
        // If we made a valid buffer then called `insert_str` that would be slow for opening new
        // files. Here, we avoid copying the string.
        let mut bytes = data.into_bytes();
        let gap_start = ByteIndex(bytes.len());
        let mut gap_length = d!();
        Self::match_len_to_capacity(&mut bytes, gap_start, &mut gap_length);

        let mut output = GapBuffer {
            data: bytes,
            gap_start,
            gap_length,
            offset_cache: d!(),
        };

        output.offset_cache = output.optimal_offset_cache(DEFAULT_BLOCK_SIZE);

        output
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

        let (index, end_offset) = {
            let index_bounds = get_tree_bounds_by(&self.offset_cache, |o| o.position.cmp(position));

            // You might be tempted to move this early return to the top of this method so we don't
            // run the lines above which potentially allocate, if the position is invalid. But if
            // we need to allocate then the index might be invalidated.
            let index = self.find_index_within_range(position, index_bounds.clone())?;

            let end_offset = if let Bound::Included(end_offset) | Bound::Excluded(end_offset) =
                index_bounds.end_bound()
            {
                Some(end_offset.clone())
            } else {
                // It must be at the end of the cache so we don't need to switch anything
                None
            };

            (index, end_offset)
        };

        if let Some(end_offset) = end_offset {
            let target_index = self.offset_cache.iter().position(|p| *p == end_offset)?;

            let insertion_offset = CachedOffset {
                position: position.clone(),
                index: remove_gap_knowledge(index, self),
            };
            let mut advanced_offset = CachedOffset {
                position: position.clone(),
                index: remove_gap_knowledge(index, self),
            };

            for g in data.graphemes() {
                advance_cached_offset_based_on_grapheme!(advanced_offset, g);
            }
            dbg!((&self.offset_cache, target_index));
            self.offset_cache.insert(
                target_index,
                dbg!(append_offsets(
                    advanced_offset,
                    unappend_offsets(end_offset, insertion_offset),
                )),
            );
        }
        //
        //

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
        let (start_pos, end_pos) = (range.start(), range.end());
        if start_pos > end_pos {
            //range is empty, so nothing to delete
            return;
        }
        let start_index = match self.find_index(start_pos) {
            Some(o) => o,
            None => return,
        };

        self.move_gap(start_index);

        self.gap_length = match self.find_index(end_pos) {
            Some(index) => {
                // Widen the gap to cover the deleted contents.
                index - self.gap_start
            }
            None => {
                // The end of the range doesn't exist; check
                // if it's on the last line in the file.
                let start_of_next_line = Position {
                    line: range.end().line + 1,
                    offset: d!(),
                };

                match self.find_index(&start_of_next_line) {
                    Some(index) => {
                        // There are other lines below this range.
                        // Just remove up until the end of the line.
                        index - self.gap_start
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

    fn get_all_cached_offsets(&self) -> Vec<CachedOffset> {
        let mut all_cached_offsets = Vec::with_capacity(
            // a loose upper bound
            self.data.len() - self.gap_length.0,
        );
        let mut cached_offset: CachedOffset = d!();

        let first_half = self.get_str(..self.gap_start.0);

        for grapheme in first_half.graphemes() {
            all_cached_offsets.push(cached_offset.clone());

            advance_cached_offset_based_on_grapheme!(cached_offset, grapheme);
        }

        let second_half = self.get_str(self.gap_end().0..);

        cached_offset.index += self.gap_length.0;

        for grapheme in second_half.graphemes() {
            all_cached_offsets.push(cached_offset.clone());

            advance_cached_offset_based_on_grapheme!(cached_offset, grapheme);
        }

        all_cached_offsets.push(cached_offset);

        all_cached_offsets
    }

    fn optimal_offset_cache(&self, block_size: NonZeroUsize) -> OffsetCache {
        optimal_offset_cache_from_all_cached_offsets(self.get_all_cached_offsets(), block_size)
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
    /// ```no_run
    ///     assert_eq!("ö", "o\u{308}");
    /// ```
    /// So there should be now way to get a `GraphemeOffset` for the byte index between
    /// "o" and "\u{308}" from this method, for a given buffer.
    // TODO should we label these offsets with which buffer they are from? Can PhantomData do that?
    /// If the cursor is after the "ö" we want the `Position`'s offset to be `1`., not `2` so we
    /// can delete the "ö" with a single keystroke.
    #[perf_viz::record]
    pub fn find_index<P: Borrow<Position>>(&self, position: P) -> Option<ByteIndex> {
        let position = position.borrow();
        self.find_index_within_range(
            position,
            get_tree_bounds_by(&self.offset_cache, |offset| offset.position.cmp(position)),
        )
    }

    fn find_index_within_range<P, R>(&self, position: P, range: R) -> Option<ByteIndex>
    where
        P: Borrow<Position>,
        R: RangeBounds<CachedOffset>,
    {
        let pos = position.borrow();
        let mut current_pos: Position;

        macro_rules! bounded {
            ($lower_bound: expr, $upper_bound_condition: expr) => {{
                let first_half = self.get_str(..self.gap_start.0);
                current_pos = $lower_bound.position;

                if $lower_bound.index < first_half.len() {
                    if let Some(first_half) = dbg!(first_half.get(dbg!($lower_bound.index.0)..)) {
                        for (index, grapheme) in first_half
                            .grapheme_indices()
                            .map(|(o, g)| (ByteIndex(o + $lower_bound.index.0), g))
                        {
                            // Check to see if we've found the position yet.
                            if current_pos == *pos {
                                return Some(index);
                            }

                            advance_position_based_on_grapheme!(current_pos, grapheme);

                            if $upper_bound_condition {
                                return None;
                            }
                        }

                        // We didn't find the position *within* the first half, but it could
                        // be right after it, which means it's right at the start of the gap.
                        if current_pos == *pos {
                            return Some(self.gap_start);
                        }
                    }
                }

                let lower_bound_index = inform_of_gap($lower_bound.index, self);

                if current_pos == *pos {
                    return Some(lower_bound_index);
                }

                // We haven't reached the position yet, so we'll move on to the other half.
                let second_half = self.get_str(self.gap_end().0..);

                if lower_bound_index < self.gap_end() + second_half.len() {
                    let second_half_start = if $lower_bound.index < first_half.len() {
                        0
                    } else {
                        $lower_bound.index.0.saturating_sub(self.gap_start.0)
                    };

                    if let Some(second_half) = second_half.get(second_half_start..) {
                        for (index, grapheme) in second_half
                            .grapheme_indices()
                            .map(|(o, g)| (ByteIndex(o + second_half_start), g))
                        {
                            // Check to see if we've found the position yet.
                            if current_pos == *pos {
                                return Some(self.gap_end() + index);
                            }

                            advance_position_based_on_grapheme!(current_pos, grapheme);

                            if $upper_bound_condition {
                                return None;
                            }
                        }

                        // We didn't find the position *within* the second half, but it could
                        // be right after it, which means it's at the end of the buffer.
                        if current_pos == *pos {
                            return Some(ByteIndex(self.data.len()));
                        }
                    }
                }

                None
            }};
        }

        match (range.start_bound(), range.end_bound()) {
            (Bound::Excluded(lower), upper) => {
                // Since in a string each index is unique the fact that we will stop looking if we
                // find the lower bound, and then discard it will produce the right answer
                // in practice.
                match upper {
                    Bound::Unbounded => bounded!(lower, false),
                    Bound::Included(upper) => bounded!(lower, current_pos > upper.position),
                    Bound::Excluded(upper) => bounded!(lower, current_pos >= upper.position),
                }
                .and_then(|c: ByteIndex| if c.0 == lower.index.0 { None } else { Some(c) })
            }
            (Bound::Unbounded, Bound::Unbounded) => {
                let lower = CachedOffset::default();
                bounded!(lower, false)
            }
            (Bound::Included(lower), Bound::Unbounded) => bounded!(lower, false),
            (Bound::Unbounded, Bound::Included(upper)) => {
                let lower = CachedOffset::default();
                bounded!(lower, current_pos > upper.position)
            }
            (Bound::Included(lower), Bound::Included(upper)) => {
                bounded!(lower, current_pos > upper.position)
            }
            (Bound::Unbounded, Bound::Excluded(upper)) => {
                let lower = CachedOffset::default();
                bounded!(lower, current_pos >= upper.position)
            }
            (Bound::Included(lower), Bound::Excluded(upper)) => {
                bounded!(lower, current_pos >= upper.position)
            }
        }
    }

    /// The character offset of the given position in the entre buffer. The output is suitable for
    /// passing into `self.graphemes().nth`.
    #[perf_viz::record]
    pub fn find_absolute_offset<P: Borrow<Position>>(&self, position: P) -> Option<CharOffset> {
        let pos = position.borrow();
        let mut current_pos: Position = d!();
        let mut absolute_offset = CharOffset(0);

        let first_half = self.get_str(..self.gap_start.0);

        for grapheme in first_half.graphemes() {
            // Check to see if we've found the position yet.
            if current_pos == *pos {
                return Some(absolute_offset);
            }

            advance_position_based_on_grapheme!(current_pos, grapheme);

            absolute_offset += 1;
        }

        // We haven't reached the position yet, so we'll move on to the other half.
        let second_half = self.get_str(self.gap_end().0..);
        for grapheme in second_half.graphemes() {
            // Check to see if we've found the position yet.
            if current_pos == *pos {
                return Some(absolute_offset);
            }

            advance_position_based_on_grapheme!(current_pos, grapheme);

            absolute_offset += 1;
        }

        // We didn't find the position *within* the second half, but it could
        // be right after it, which means it's at the end of the buffer.
        if current_pos == *pos {
            return Some(absolute_offset);
        }

        None
    }

    #[perf_viz::record]
    fn get_str<I>(&self, index: I) -> &str
    where
        I: std::slice::SliceIndex<[u8], Output = [u8]>,
    {
        invariant_assert!(std::str::from_utf8(&self.data[index]).is_ok());

        let minimize_unsafe = &self.data[index];

        unsafe { std::str::from_utf8_unchecked(minimize_unsafe) }
    }
}

//
// Probably only useful for debugging
//
#[allow(dead_code)]
impl GapBuffer {
    fn new_with_block_size(data: String, block_size: NonZeroUsize) -> GapBuffer {
        let mut output = Self::new(data);

        output.offset_cache = output.optimal_offset_cache(block_size);

        output
    }

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
    first_half: std::str::Lines<'buffer>,
    second_half: std::str::Lines<'buffer>,
    gap_is_between_lines: bool,
}

impl<'buffer> Iterator for GapLines<'buffer> {
    type Item = GapLine<'buffer>;

    // TODO: fast nth
    // fn nth(mut n: usize) -> Option<Self::Item> {
    //
    // }

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! handle_gap_edges {
            ($first:expr) => {
                if self.gap_is_between_lines {
                    Some($first).map(GapLine::Connected)
                } else {
                    match self.second_half.next() {
                        Some(from_second) => match ($first, from_second) {
                            ("", line) | (line, "") => Some(line).map(GapLine::Connected),
                            (f, s) => Some(GapLine::Gapped(f, s)),
                        },
                        None => Some($first).map(GapLine::Connected),
                    }
                }
            };
        }

        let next = self.next_of_first;
        self.next_of_first = self.first_half.next();

        match (next, self.next_of_first) {
            (Some(n), Some(_)) => Some(n).map(GapLine::Connected),
            (Some(n), None) => handle_gap_edges!(n),
            (None, Some(nn)) => {
                self.next_of_first = self.first_half.next();
                match self.next_of_first {
                    None => handle_gap_edges!(nn),
                    Some(_) => Some(nn).map(GapLine::Connected),
                }
            }
            (None, None) => self.second_half.next().map(GapLine::Connected),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum GapLine<'buffer> {
    Connected(&'buffer str),
    Gapped(&'buffer str, &'buffer str),
}

impl<'buffer> GapBuffer {
    pub fn lines(&'buffer self) -> impl Iterator<Item = GapLine<'buffer>> + 'buffer {
        let (first_half, second_half) = self.get_halves();
        GapLines {
            next_of_first: None,
            first_half: first_half.lines(),
            second_half: second_half.lines(),
            gap_is_between_lines: first_half
                .chars()
                .last()
                .map(|c| c == '\n')
                // If the gap is at the start then the gap is not inside a line
                .unwrap_or(true),
        }
    }
}

impl GapBuffer {
    pub fn nth_line_count(&self, n: usize) -> Option<usize> {
        // TODO fast version of this. (fast `nth` may be enough).
        self.lines().nth(n).map(|g| match g {
            GapLine::Gapped(first, second) => {
                first.graphemes().count() + second.graphemes().count()
            }
            GapLine::Connected(line) => line.graphemes().count(),
        })
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
            offset: CharOffset(gap_buffer.nth_line_count(line).unwrap_or_default()),
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
mod tests;
