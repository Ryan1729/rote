use crate::*;

use std::ops::{Bound, RangeBounds};

/// It is the user of this funtion's responsibility to uphold the invariants of `RopeLine`.
pub(crate) fn to_rope_line(rope_slice: ropey::RopeSlice) -> RopeLine {
    RopeLine(RopeSlice {
        rope_slice
    })
}

pub(crate) fn to_chunk((s, byte_idx, char_index, line_idx): (&str, usize, usize, usize)) -> Chunk {
    (s, ByteIndex(byte_idx), AbsoluteCharOffset(char_index), LineIndex(line_idx))
}

#[inline(always)]
pub fn to_slice_range<O, R>(char_range: R, length: O) -> Option<(Bound<usize>, Bound<usize>)>
where
    O: Into<CharOffset> + Copy,
    R: RangeBounds<O>, {
        let start_bound = to_bound_usize(char_range.start_bound());
        let start = start_bound_to_num(start_bound).unwrap_or(0);

        let end_bound = to_bound_usize(char_range.end_bound());
        let len = length.into().0;
        let end = end_bound_to_num(end_bound).unwrap_or(len);

        some_if!(
            start <= end && end <= len => (start_bound, end_bound)
        )
}

#[inline(always)]
fn to_bound_usize<O: Into<CharOffset> + Copy>(b: Bound<&O>) -> Bound<usize> {
    match b {
        Bound::Included(&o) => {
            let CharOffset(i) = o.into();
            Bound::Included(i)
        },
        Bound::Excluded(&o) => {
            let CharOffset(i) = o.into();
            Bound::Excluded(i)
        },
        Bound::Unbounded => Bound::Unbounded,
    }
}

#[inline(always)]
fn start_bound_to_num(b: Bound<usize>) -> Option<usize> {
    match b {
        Bound::Included(n) => Some(n),
        Bound::Excluded(n) => Some(n + 1),
        Bound::Unbounded => None,
    }
}

#[inline(always)]
fn end_bound_to_num(b: Bound<usize>) -> Option<usize> {
    match b {
        Bound::Included(n) => Some(n + 1),
        Bound::Excluded(n) => Some(n),
        Bound::Unbounded => None,
    }
}
