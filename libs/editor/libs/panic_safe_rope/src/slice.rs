use crate::{start_bound_to_num, end_bound_to_num};
use std::ops::RangeBounds;
use ropey::iter::{Bytes, Chars, Lines, Chunks};

/// A wrapper around `ropey::RopeSlice` that checks the panic conditions at runtime and
/// changes the return type of some methods with the aim of preventing panics.
#[derive(Copy, Clone, Debug)]
pub struct RopeSlice<'a> {
    pub(crate) rope_slice: ropey::RopeSlice<'a>,
}

/// A wrapper around `RopeSlice` that is guarenteed to have at most one newline in it.
/// That newline is also guarenteed to be at the end, if it is present.
/// It is returned from the `line` method on `RopeSlice`
#[derive(Copy, Clone, Debug)]
pub struct RopeLine<'a>(pub(crate) RopeSlice<'a>);

/// This trait enforces that `RopeLine` can do everything a `RopeSlice` can. Yes, it does mean
/// that you have to import the trait too, but on the plus side, the source code now has all
/// the method definintions in one place which cn be read without the distraction of
/// implementations. :|
pub trait RopeSliceTrait<'a> {
    fn len_bytes(&self) -> usize;

    fn len_chars(&self) -> usize;

    fn len_lines(&self) -> usize;

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    fn byte_to_char(&self, byte_idx: usize) -> Option<usize>;

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    fn byte_to_line(&self, byte_idx: usize) -> Option<usize>;

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    fn char_to_byte(&self, char_idx: usize) -> Option<usize>;

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    fn char_to_line(&self, char_idx: usize) -> Option<usize>;

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    fn line_to_byte(&self, line_idx: usize) -> Option<usize>;

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    fn line_to_char(&self, line_idx: usize) -> Option<usize>;

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx >= len_bytes()`).
    fn byte(&self, byte_idx: usize) -> Option<u8>;

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx >= len_chars()`).
    fn char(&self, char_idx: usize) -> Option<char>;

    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    fn chunk_at_byte(&self, byte_idx: usize) -> Option<(&'a str, usize, usize, usize)>;

    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    fn chunk_at_char(&self, char_idx: usize) -> Option<(&'a str, usize, usize, usize)>;

    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    fn chunk_at_line_break(
        &self,
        line_break_idx: usize
    ) -> Option<(&'a str, usize, usize, usize)>;

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx >= len_lines()`).
    fn line(&self, line_idx: usize) -> Option<RopeLine<'a>>;

    /// Returns `None` if the start of the range is greater than the end, or if the
    /// end is out of bounds (i.e. `end > len_chars()`).
    fn slice<R>(&self, char_range: R) -> Option<Self> where
        R: RangeBounds<usize>,
        Self: std::marker::Sized;

    fn bytes(&self) -> Bytes<'a>;

    fn chars(&self) -> Chars<'a>;

    fn lines(&self) -> Lines<'a>;

    fn chunks(&self) -> Chunks<'a>;

    fn as_str(&self) -> Option<&'a str>;
}

// End of public facing portion of this file.

// It is the user of this funtion's responsibility to uphold the invariants of `RopeLine`.
pub(crate) fn to_rope_line(rope_slice: ropey::RopeSlice) -> RopeLine {
    RopeLine(RopeSlice {
        rope_slice
    })
}

impl <'a>RopeSliceTrait<'a> for RopeSlice<'a> {
    #[inline]
    fn len_bytes(&self) -> usize {
        self.rope_slice.len_bytes()
    }

    #[inline]
    fn len_chars(&self) -> usize {
        self.rope_slice.len_chars()
    }

    #[inline]
    fn len_lines(&self) -> usize {
        self.rope_slice.len_lines()
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn byte_to_char(&self, byte_idx: usize) -> Option<usize> {
        macros::some_if!(byte_idx <= self.len_bytes() => self.rope_slice.byte_to_char(byte_idx))
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn byte_to_line(&self, byte_idx: usize) -> Option<usize> {
        macros::some_if!(byte_idx <= self.len_bytes() => self.rope_slice.byte_to_line(byte_idx))
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn char_to_byte(&self, char_idx: usize) -> Option<usize> {
        macros::some_if!(char_idx <= self.len_chars() => self.rope_slice.char_to_byte(char_idx))
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn char_to_line(&self, char_idx: usize) -> Option<usize> {
        macros::some_if!(
            char_idx <= self.len_chars() => self.rope_slice.char_to_line(char_idx)
        )
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    fn line_to_byte(&self, line_idx: usize) -> Option<usize> {
        macros::some_if!(
            line_idx <= self.len_lines() => self.rope_slice.line_to_byte(line_idx)
        )
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        macros::some_if!(
            line_idx <= self.len_lines() => self.rope_slice.line_to_char(line_idx)
        )
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx >= len_bytes()`).
    #[inline]
    fn byte(&self, byte_idx: usize) -> Option<u8> {
        macros::some_if!(
            byte_idx < self.len_bytes() => self.rope_slice.byte(byte_idx)
        )
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx >= len_chars()`).
    #[inline]
    fn char(&self, char_idx: usize) -> Option<char> {
        macros::some_if!(
            char_idx < self.len_chars() => self.rope_slice.char(char_idx)
        )
    }

    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn chunk_at_byte(&self, byte_idx: usize) -> Option<(&'a str, usize, usize, usize)> {
        macros::some_if!(
            byte_idx <= self.len_bytes() => self.rope_slice.chunk_at_byte(byte_idx)
        )
    }

    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn chunk_at_char(&self, char_idx: usize) -> Option<(&'a str, usize, usize, usize)> {
        macros::some_if!(
            char_idx <= self.len_chars() => self.rope_slice.chunk_at_char(char_idx)
        )
    }

    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    #[inline]
    fn chunk_at_line_break(
        &self,
        line_break_idx: usize,
    ) -> Option<(&'a str, usize, usize, usize)> {
        macros::some_if!(
            line_break_idx <= self.len_lines() => self.rope_slice.chunk_at_line_break(line_break_idx)
        )
    }

    #[inline]
    fn bytes(&self) -> ropey::iter::Bytes<'a> {
        self.rope_slice.bytes()
    }

    #[inline]
    fn chars(&self) -> ropey::iter::Chars<'a> {
        self.rope_slice.chars()
    }

    #[inline]
    fn lines(&self) -> ropey::iter::Lines<'a> {
        self.rope_slice.lines()
    }

    #[inline]
    fn chunks(&self) -> ropey::iter::Chunks<'a> {
        self.rope_slice.chunks()
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx >= len_lines()`).
    #[inline]
    fn line(&self, line_idx: usize) -> Option<RopeLine<'a>> {
        macros::some_if!(
            line_idx < self.len_lines() => to_rope_line(self.rope_slice.line(line_idx))
        )
    }

    /// Returns `None` if the start of the range is greater than the end, or if the
    /// end is out of bounds (i.e. `end > len_chars()`).
    #[inline]
    fn slice<R>(&self, char_range: R) -> Option<Self>
    where
        R: RangeBounds<usize>,
    {
        let start = start_bound_to_num(char_range.start_bound()).unwrap_or(0);
        let end = end_bound_to_num(char_range.end_bound()).unwrap_or_else(|| self.len_chars());

        macros::some_if!(
            start <= end && end <= self.len_chars() => RopeSlice {
                rope_slice: self.rope_slice.slice(char_range)
            }
        )
    }

    fn as_str(&self) -> Option<&'a str> {
        self.rope_slice.as_str()
    }
}

impl<'a> From<&'a str> for RopeSlice<'a> {
    #[inline]
    fn from(text: &'a str) -> Self {
        RopeSlice{
            rope_slice: ropey::RopeSlice::from(text)
        }
    }
}

impl<'a> From<RopeSlice<'a>> for String {
    #[inline]
    fn from(slice: RopeSlice<'a>) -> Self {
        Self::from(slice.rope_slice)
    }
}

impl<'a> From<RopeSlice<'a>> for std::borrow::Cow<'a, str> {
    #[inline]
    fn from(slice: RopeSlice<'a>) -> Self {
        Self::from(slice.rope_slice)
    }
}

impl<'slice> std::cmp::Eq for RopeSlice<'slice> {}

impl<'slice> std::cmp::PartialEq<RopeSlice<'slice>> for RopeSlice<'slice> {
    #[inline]
    fn eq(&self, other: &RopeSlice<'slice>) -> bool {
        self.rope_slice.eq(&other.rope_slice)
    }
}

impl<'a,'slice> std::cmp::PartialEq<&'a str> for RopeSlice<'slice> {
    #[inline]
    fn eq(&self, other: &&'a str) -> bool {
        self.rope_slice.eq(other)
    }
}

impl<'a,'slice> std::cmp::PartialEq<RopeSlice<'slice>> for &'a str {
    #[inline]
    fn eq(&self, other: &RopeSlice<'slice>) -> bool {
        self.eq(&other.rope_slice)
    }
}

impl<'slice> std::cmp::PartialEq<str> for RopeSlice<'slice> {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.rope_slice.eq(other)
    }
}

impl<'slice> std::cmp::PartialEq<RopeSlice<'slice>> for str {
    #[inline]
    fn eq(&self, other: &RopeSlice<'slice>) -> bool {
        self.eq(&other.rope_slice)
    }
}

impl<'a, 'slice> std::cmp::PartialEq<String> for RopeSlice<'slice> {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.rope_slice.eq(other)
    }
}

impl<'a,'slice> std::cmp::PartialEq<RopeSlice<'slice>> for String {
    #[inline]
    fn eq(&self, other: &RopeSlice<'slice>) -> bool {
        self.eq(&other.rope_slice)
    }
}

impl<'a,'slice> std::cmp::PartialEq<std::borrow::Cow<'a, str>> for RopeSlice<'slice> {
    #[inline]
    fn eq(&self, other: &std::borrow::Cow<'a, str>) -> bool {
        self.rope_slice.eq(other)
    }
}

impl<'a,'slice> std::cmp::PartialEq<RopeSlice<'slice>> for std::borrow::Cow<'a, str> {
    #[inline]
    fn eq(&self, other: &RopeSlice<'slice>) -> bool {
        self.eq(&other.rope_slice)
    }
}

impl<'slice> std::cmp::Ord for RopeSlice<'slice> {
    #[inline]
    fn cmp(&self, other: &RopeSlice<'slice>) -> std::cmp::Ordering {
        self.rope_slice.cmp(&other.rope_slice)
    }
}

impl<'slice> std::cmp::PartialOrd<RopeSlice<'slice>> for RopeSlice<'slice> {
    #[inline]
    fn partial_cmp(&self, other: &RopeSlice<'slice>) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl <'a>RopeSliceTrait<'a> for RopeLine<'a> {
    // In general in this impl we punch through to the `rope_slice` field if we didn't cahnge the
    // type, and we just call the method on `.0` if we did, so we don't have to reimplement the
    // logic

    #[inline]
    fn len_bytes(&self) -> usize {
        self.0.rope_slice.len_bytes()
    }

    #[inline]
    fn len_chars(&self) -> usize {
        self.0.rope_slice.len_chars()
    }

    #[inline]
    fn len_lines(&self) -> usize {
        self.0.rope_slice.len_lines()
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn byte_to_char(&self, byte_idx: usize) -> Option<usize> {
        self.0.byte_to_char(byte_idx)
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn byte_to_line(&self, byte_idx: usize) -> Option<usize> {
        self.0.byte_to_line(byte_idx)
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn char_to_byte(&self, char_idx: usize) -> Option<usize> {
        self.0.char_to_byte(char_idx)
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn char_to_line(&self, char_idx: usize) -> Option<usize> {
        self.0.char_to_line(char_idx)
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    fn line_to_byte(&self, line_idx: usize) -> Option<usize> {
        self.0.line_to_byte(line_idx)
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        self.0.line_to_char(line_idx)
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx >= len_bytes()`).
    #[inline]
    fn byte(&self, byte_idx: usize) -> Option<u8> {
        self.0.byte(byte_idx)
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx >= len_chars()`).
    #[inline]
    fn char(&self, char_idx: usize) -> Option<char> {
        self.0.char(char_idx)
    }

    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn chunk_at_byte(&self, byte_idx: usize) -> Option<(&'a str, usize, usize, usize)> {
        self.0.chunk_at_byte(byte_idx)
    }

    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn chunk_at_char(&self, char_idx: usize) -> Option<(&'a str, usize, usize, usize)> {
        self.0.chunk_at_char(char_idx)
    }

    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    #[inline]
    fn chunk_at_line_break(
        &self,
        line_break_idx: usize,
    ) -> Option<(&'a str, usize, usize, usize)> {
        self.0.chunk_at_line_break(line_break_idx)
    }

    #[inline]
    fn bytes(&self) -> ropey::iter::Bytes<'a> {
        self.0.rope_slice.bytes()
    }

    #[inline]
    fn chars(&self) -> ropey::iter::Chars<'a> {
        self.0.rope_slice.chars()
    }

    #[inline]
    fn lines(&self) -> ropey::iter::Lines<'a> {
        self.0.rope_slice.lines()
    }

    #[inline]
    fn chunks(&self) -> ropey::iter::Chunks<'a> {
        self.0.rope_slice.chunks()
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx >= len_lines()`).
    #[inline]
    fn line(&self, line_idx: usize) -> Option<RopeLine<'a>> {
        self.0.line(line_idx)
    }

    /// Returns `None` if the start of the range is greater than the end, or if the
    /// end is out of bounds (i.e. `end > len_chars()`).
    #[inline]
    fn slice<R>(&self, char_range: R) -> Option<Self>
    where
        R: RangeBounds<usize>,
    {
        let start = start_bound_to_num(char_range.start_bound()).unwrap_or(0);
        let end = end_bound_to_num(char_range.end_bound()).unwrap_or_else(|| self.len_chars());

        macros::some_if!(
            start <= end && end <= self.len_chars() => to_rope_line(self.0.rope_slice.slice(char_range))
        )
    }

    fn as_str(&self) -> Option<&'a str> {
        self.0.rope_slice.as_str()
    }
}

impl<'a> From<&'a str> for RopeLine<'a> {
    #[inline]
    fn from(text: &'a str) -> Self {
        to_rope_line(ropey::RopeSlice::from(text))
    }
}

impl<'a> From<RopeLine<'a>> for String {
    #[inline]
    fn from(slice: RopeLine<'a>) -> Self {
        Self::from(slice.0.rope_slice)
    }
}

impl<'a> From<RopeLine<'a>> for std::borrow::Cow<'a, str> {
    #[inline]
    fn from(slice: RopeLine<'a>) -> Self {
        Self::from(slice.0.rope_slice)
    }
}

impl<'slice> std::cmp::Eq for RopeLine<'slice> {}

impl<'slice> std::cmp::PartialEq<RopeLine<'slice>> for RopeLine<'slice> {
    #[inline]
    fn eq(&self, other: &RopeLine<'slice>) -> bool {
        self.0.rope_slice.eq(&other.0.rope_slice)
    }
}

impl<'a,'slice> std::cmp::PartialEq<&'a str> for RopeLine<'slice> {
    #[inline]
    fn eq(&self, other: &&'a str) -> bool {
        self.0.rope_slice.eq(other)
    }
}

impl<'a,'slice> std::cmp::PartialEq<RopeLine<'slice>> for &'a str {
    #[inline]
    fn eq(&self, other: &RopeLine<'slice>) -> bool {
        self.eq(&other.0.rope_slice)
    }
}

impl<'slice> std::cmp::PartialEq<str> for RopeLine<'slice> {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.0.rope_slice.eq(other)
    }
}

impl<'slice> std::cmp::PartialEq<RopeLine<'slice>> for str {
    #[inline]
    fn eq(&self, other: &RopeLine<'slice>) -> bool {
        self.eq(&other.0.rope_slice)
    }
}

impl<'a, 'slice> std::cmp::PartialEq<String> for RopeLine<'slice> {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.0.rope_slice.eq(other)
    }
}

impl<'a,'slice> std::cmp::PartialEq<RopeLine<'slice>> for String {
    #[inline]
    fn eq(&self, other: &RopeLine<'slice>) -> bool {
        self.eq(&other.0.rope_slice)
    }
}

impl<'a,'slice> std::cmp::PartialEq<std::borrow::Cow<'a, str>> for RopeLine<'slice> {
    #[inline]
    fn eq(&self, other: &std::borrow::Cow<'a, str>) -> bool {
        self.0.rope_slice.eq(other)
    }
}

impl<'a,'slice> std::cmp::PartialEq<RopeLine<'slice>> for std::borrow::Cow<'a, str> {
    #[inline]
    fn eq(&self, other: &RopeLine<'slice>) -> bool {
        self.eq(&other.0.rope_slice)
    }
}

impl<'slice> std::cmp::Ord for RopeLine<'slice> {
    #[inline]
    fn cmp(&self, other: &RopeLine<'slice>) -> std::cmp::Ordering {
        self.0.rope_slice.cmp(&other.0.rope_slice)
    }
}

impl<'slice> std::cmp::PartialOrd<RopeLine<'slice>> for RopeLine<'slice> {
    #[inline]
    fn partial_cmp(&self, other: &RopeLine<'slice>) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
