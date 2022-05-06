use editor_types::{Position, Cursor};
use macros::CheckedSub;
use panic_safe_rope::*;

pub use panic_safe_rope::is_linebreak_char;

use std::borrow::Borrow;

mod absolute_char_offset_range {
    use super::*;

    #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
    pub struct AbsoluteCharOffsetRange {
        min: AbsoluteCharOffset,
        max: AbsoluteCharOffset,
    }

    impl AbsoluteCharOffsetRange {
        #[must_use]
        pub fn new(o1: AbsoluteCharOffset, o2: AbsoluteCharOffset) -> Self {
            let min = std::cmp::min(o1, o2);
            let max = std::cmp::max(o1, o2);

            AbsoluteCharOffsetRange { min, max }
        }

        #[must_use]
        pub fn new_usize(o1: usize, o2: usize) -> Self {
            AbsoluteCharOffsetRange::new(
                AbsoluteCharOffset(o1),
                AbsoluteCharOffset(o2)
            )
        }

        #[must_use]
        pub fn range(&self) -> std::ops::Range<AbsoluteCharOffset> {
            self.min..self.max
        }

        #[must_use]
        pub fn min(&self) -> AbsoluteCharOffset {
            self.min
        }

        #[must_use]
        pub fn max(&self) -> AbsoluteCharOffset {
            self.max
        }

        #[must_use]
        #[allow(dead_code)]
        pub fn add_to_min<A>(&self, min: A) -> Self
        where
            AbsoluteCharOffset: std::ops::Add<A, Output = AbsoluteCharOffset>,
        {
            Self::new(self.min + min, self.max)
        }

        #[must_use]
        pub fn add_to_max<A>(&self, max: A) -> Self
        where
            AbsoluteCharOffset: std::ops::Add<A, Output = AbsoluteCharOffset>,
        {
            Self::new(self.min, self.max + max)
        }

        #[must_use]
        #[allow(dead_code)]
        pub fn sub_from_min<A>(&self, min: A) -> Self
        where
            AbsoluteCharOffset: std::ops::Sub<A, Output = AbsoluteCharOffset>,
        {
            Self::new(self.min - min, self.max)
        }

        #[must_use]
        #[allow(dead_code)]
        pub fn sub_from_max<A>(&self, max: A) -> Self
        where
            AbsoluteCharOffset: std::ops::Sub<A, Output = AbsoluteCharOffset>,
        {
            Self::new(self.min, self.max - max)
        }

        #[must_use]
        #[allow(dead_code)]
        pub fn checked_sub_from_min<A>(&self, min: A) -> Option<Self>
        where
            AbsoluteCharOffset: CheckedSub<A, Output = AbsoluteCharOffset>,
        {
            self.min
                .checked_sub(min)
                .map(|min| Self::new(min, self.max))
        }

        #[must_use]
        pub fn checked_sub_from_max<A>(&self, max: A) -> Option<Self>
        where
            AbsoluteCharOffset: CheckedSub<A, Output = AbsoluteCharOffset>,
        {
            self.max
                .checked_sub(max)
                .map(|max| Self::new(self.min, max))
        }
    }
}
pub use absolute_char_offset_range::AbsoluteCharOffsetRange;

#[must_use]
#[perf_viz::record]
pub fn in_cursor_bounds<P: Borrow<Position>>(rope: &Rope, position: P) -> bool {
    let p = position.borrow();
    final_non_newline_offset_for_line(rope, LineIndex(p.line))
        .map_or(false, |l| p.offset <= l)
}

#[must_use]
/// Returns `None` iff the position is not valid for the given rope.
#[perf_viz::record]
pub fn pos_to_char_offset(rope: &Rope, position: &Position) -> Option<AbsoluteCharOffset> {
    let line_index = LineIndex(position.line);

    let line_start = rope.line_to_char(line_index)?;
    let line = rope.line(line_index)?;
    let offset = position.offset;
    if offset == 0 || offset <= line.len_chars() {
        Some(line_start + offset)
    } else {
        None
    }
}

#[must_use]
#[perf_viz::record]
pub fn char_offset_to_pos(rope: &Rope, offset: AbsoluteCharOffset) -> Option<Position> {
    if rope.len_chars() == offset {
        Some(LineIndex(rope.len_lines().0 - 1))
    } else {
        rope.char_to_line(offset)
    }
    .and_then(|line_index| {
        let start_of_line = rope.line_to_char(line_index)?;

        offset
            .checked_sub(start_of_line)
            .map(|offset: CharOffset| Position {
                line: line_index.0,
                offset,
            })
    })
}

#[must_use]
pub fn clamp_position(rope: &Rope, position: Position) -> Position {
    clamp_position_helper(rope, position)
        .unwrap_or_else(|| 
            char_offset_to_pos(rope, rope.len_chars())
                .unwrap_or_default()
        )
}

#[must_use]
fn clamp_position_helper(rope: &Rope, position: Position) -> Option<Position> {
    let line_index = LineIndex(position.line);

    let line_start = rope.line_to_char(line_index)?;
    let line = rope.line(line_index)?;
    let offset = position.offset;

    let abs_offset = line_start + std::cmp::min(offset, final_non_newline_offset_for_rope_line(line));

    let line_index = if rope.len_chars() == abs_offset {
        Some(LineIndex(rope.len_lines().0 - 1))
    } else {
        rope.char_to_line(abs_offset)
    };

    line_index.and_then(|line_index| {
        let start_of_line = rope.line_to_char(line_index)?;

        abs_offset
            .checked_sub(start_of_line)
            .map(|offset: CharOffset| Position {
                line: line_index.0,
                offset,
            })
    })
}

#[must_use]
/// returns `None` if the input position's line does not refer to a line in the `Rope`.
pub fn nearest_valid_position_on_same_line<P: Borrow<Position>>(rope: &Rope, p: P) -> Option<Position> {
    let p = p.borrow();

    final_non_newline_offset_for_line(rope, LineIndex(p.line)).map(|final_offset| Position {
        offset: std::cmp::min(p.offset, final_offset),
        ..*p
    })
}

#[must_use]
/// Returns `None` if that line is not in the `Rope`.
#[perf_viz::record]
fn final_non_newline_offset_for_line(rope: &Rope, line_index: LineIndex) -> Option<CharOffset> {
    rope.line(line_index)
        .map(final_non_newline_offset_for_rope_line)
}

#[must_use]
#[perf_viz::record]
pub fn final_non_newline_offset_for_rope_line(line: RopeLine) -> CharOffset {
    final_non_newline_offset_for_rope_line_(line)
}
#[must_use]
fn final_non_newline_offset_for_rope_line_(line: RopeLine) -> CharOffset {
    let mut len = line.len_chars();

    macro_rules! get_char_before_len {
        () => {
            if let Some(c) = line.char(len - 1) {
                c
            } else {
                // We know that the index we are passing in is less than `len_chars()`
                // so this case should not actually happen. But, we have a reasonable
                // value to return so why no just do that?
                return CharOffset(0);
            }
        };
    }

    macro_rules! return_if_0 {
        () => {
            if len == 0 {
                return CharOffset(0);
            }
        };
    }

    return_if_0!();

    let last = get_char_before_len!();

    if last == '\n' {
        len -= 1;

        return_if_0!();

        let second_last = get_char_before_len!();

        if second_last == '\r' {
            len -= 1;
            return_if_0!();
        }
    } else if is_linebreak_char(last)
    {
        len -= 1;
        return_if_0!();
    }

    len
}

fn get_line_char_iterator<'line, R: std::ops::RangeBounds<CharOffset>>(
    line: RopeLine<'line>,
    range: R,
) -> impl Iterator<Item = (CharOffset, char)> + 'line {
    use std::ops::Bound::*;

    let skip = match range.start_bound() {
        Included(CharOffset(o)) => *o,
        Excluded(CharOffset(o)) => (*o).saturating_add(1),
        Unbounded => 0,
    };

    let take = match range.end_bound() {
        Included(CharOffset(o)) => *o,
        Excluded(CharOffset(o)) => (*o).saturating_sub(1),
        Unbounded => usize::max_value(),
    } - skip;
    
    let final_offset = final_non_newline_offset_for_rope_line(line);

    line
        .chars()
        .enumerate()
        .map(|(i, c)| (CharOffset(i), c))
        .skip(skip)
        .take(std::cmp::min(take, final_offset.0))
}

#[must_use]
pub fn get_first_non_white_space_offset_in_range<R: std::ops::RangeBounds<CharOffset>>(
    line: RopeLine,
    range: R,
) -> Option<CharOffset> {
    for (i, c) in get_line_char_iterator(line, range) {
        if !c.is_whitespace() {
            return Some(i);
        }
    }

    None
}

#[must_use]
pub fn get_last_non_white_space_offset_in_range<R: std::ops::RangeBounds<CharOffset>>(
    line: RopeLine,
    range: R,
) -> Option<CharOffset> {
    // TODO would it be worth it to make a reversed iterator? One that just pulls
    // the chars into memory seems like it wouldn't be worth it.
    let mut output = None;

    for (i, c) in get_line_char_iterator(line, range) {
        if !c.is_whitespace() {
            output = Some(i);
        }
    }

    output
}

pub type OffsetPair = (Option<AbsoluteCharOffset>, Option<AbsoluteCharOffset>);

/// This will return `None` if the offset is one-past the last index.
pub fn offset_pair(rope: &Rope, cursor: &Cursor) -> OffsetPair {
    let filter_out_of_bounds =
        |position: Position| macros::some_if!(in_cursor_bounds(rope, position) => position);

    (
        Some(cursor.get_position())
            .and_then(filter_out_of_bounds)
            .and_then(|p| pos_to_char_offset(rope, &p)),
        cursor
            .get_highlight_position()
            .and_then(filter_out_of_bounds)
            .and_then(|p| pos_to_char_offset(rope, &p)),
    )
}

#[cfg(test)]
mod tests;