use crate::{
    conversion::{to_chunk, to_rope_line, to_slice_range},
    ByteIndex, ByteLength, CharOffset, Chunk, LineIndex, LineLength, Lines,
};
pub use ropey::iter::{Bytes, Chars, Chunks};
use std::{
    ops::RangeBounds,
    borrow::Cow,
};

/// A wrapper around `ropey::RopeSlice` that checks the panic conditions at runtime and
/// changes the return type of some methods with the aim of preventing panics.
#[derive(Copy, Clone, Debug)]
pub struct RopeSlice<'rope> {
    pub(crate) rope_slice: ropey::RopeSlice<'rope>,
}

/// A wrapper around `RopeSlice` that is guarenteed to have at most one newline in it.
/// That newline is also guarenteed to be at the end, if it is present.
/// It is returned from the `line` method on `RopeSlice`
#[derive(Copy, Clone, Debug)]
pub struct RopeLine<'rope>(pub(crate) RopeSlice<'rope>);

/// This trait enforces that `RopeLine` can do everything a `RopeSlice` can. Yes, it does mean
/// that you have to import the trait too, but on the plus side, the source code now has all
/// the method definintions in one place which cn be read without the distraction of
/// implementations. :|
/// Note that this trait has methods that are similar some `Rope` methods, but some of the
/// guarentees are different, so accordingly some of the types are different. For instance since
/// the indexes here are relative to the start of the slice, the methods here return `CharOffset`s
/// instead of `AbsoluteCharOffset`.
pub trait RopeSliceTrait<'rope> {
    fn len_bytes(&self) -> ByteLength;

    fn len_chars(&self) -> CharOffset;

    fn len_lines(&self) -> LineLength;

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    fn byte_to_char(&self, byte_idx: ByteIndex) -> Option<CharOffset>;

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    fn byte_to_line(&self, byte_idx: ByteIndex) -> Option<LineIndex>;

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    fn char_to_byte(&self, char_idx: CharOffset) -> Option<ByteIndex>;

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    fn char_to_line(&self, char_idx: CharOffset) -> Option<LineIndex>;

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    fn line_to_byte(&self, line_idx: LineIndex) -> Option<ByteIndex>;

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    fn line_to_char(&self, line_idx: LineIndex) -> Option<CharOffset>;

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx >= len_bytes()`).
    fn byte(&self, byte_idx: ByteIndex) -> Option<u8>;

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx >= len_chars()`).
    fn char(&self, char_idx: CharOffset) -> Option<char>;

    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    fn chunk_at_byte(&self, byte_idx: ByteIndex) -> Option<Chunk<'rope>>;

    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    fn chunk_at_char(&self, char_idx: CharOffset) -> Option<Chunk<'rope>>;

    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    fn chunk_at_line_break(&self, line_break_idx: usize) -> Option<Chunk<'rope>>;

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx >= len_lines()`).
    fn line(&self, line_idx: LineIndex) -> Option<RopeLine<'rope>>;

    /// Returns `None` if the start of the range is greater than the end, or if the
    /// end is out of bounds (i.e. `end > len_chars()`).
    fn slice<R>(&self, char_range: R) -> Option<Self>
    where
        R: RangeBounds<CharOffset>,
        Self: std::marker::Sized;

    fn bytes(&self) -> Bytes<'rope>;

    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    fn bytes_at(&self, byte_idx: ByteIndex) -> Option<Bytes>;

    fn chars(&self) -> Chars<'rope>;

    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    fn chars_at(&self, char_idx: CharOffset) -> Option<Chars>;

    fn lines(&self) -> Lines<'rope>;

    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    fn lines_at(&self, line_break_idx: usize) -> Option<Lines>;

    fn chunks(&self) -> Chunks<'rope>;

    // Returns the sliced chars as a contiguous string if possible. This allows 
    // optimizations in those cases. Note that a valid implemention of this method
    // can always return `None`, so it should not be relied upon to ever return a 
    // `&str`
    fn as_str_if_no_allocation_needed(&self) -> Option<&'rope str>;

    fn as_cow_str(&self) -> Cow<'rope, str>;
}

// End of public facing portion of this file.

impl<'rope> RopeSliceTrait<'rope> for RopeSlice<'rope> {
    #[inline]
    fn len_bytes(&self) -> ByteLength {
        ByteLength(self.rope_slice.len_bytes())
    }

    #[inline]
    fn len_chars(&self) -> CharOffset {
        CharOffset(self.rope_slice.len_chars())
    }

    #[inline]
    fn len_lines(&self) -> LineLength {
        LineLength(self.rope_slice.len_lines())
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn byte_to_char(&self, byte_idx: ByteIndex) -> Option<CharOffset> {
        macros::some_if!(byte_idx <= self.len_bytes().0 => CharOffset(self.rope_slice.byte_to_char(byte_idx.0)))
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn byte_to_line(&self, byte_idx: ByteIndex) -> Option<LineIndex> {
        macros::some_if!(byte_idx <= self.len_bytes().0 => LineIndex(self.rope_slice.byte_to_line(byte_idx.0)))
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn char_to_byte(&self, char_idx: CharOffset) -> Option<ByteIndex> {
        macros::some_if!(char_idx <= self.len_chars() => ByteIndex(self.rope_slice.char_to_byte(char_idx.0)))
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn char_to_line(&self, char_idx: CharOffset) -> Option<LineIndex> {
        macros::some_if!(
            char_idx <= self.len_chars() => LineIndex(self.rope_slice.char_to_line(char_idx.0))
        )
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    fn line_to_byte(&self, line_idx: LineIndex) -> Option<ByteIndex> {
        macros::some_if!(
            line_idx <= self.len_lines().0 => ByteIndex(self.rope_slice.line_to_byte(line_idx.0))
        )
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    fn line_to_char(&self, line_idx: LineIndex) -> Option<CharOffset> {
        macros::some_if!(
            line_idx <= self.len_lines().0 => CharOffset(self.rope_slice.line_to_char(line_idx.0))
        )
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx >= len_bytes()`).
    #[inline]
    fn byte(&self, byte_idx: ByteIndex) -> Option<u8> {
        macros::some_if!(
            byte_idx < self.len_bytes().0 => self.rope_slice.byte(byte_idx.0)
        )
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx >= len_chars()`).
    #[inline]
    fn char(&self, char_idx: CharOffset) -> Option<char> {
        macros::some_if!(
            char_idx < self.len_chars() => self.rope_slice.char(char_idx.0)
        )
    }

    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn chunk_at_byte(&self, byte_idx: ByteIndex) -> Option<Chunk<'rope>> {
        macros::some_if!(
            byte_idx <= self.len_bytes().0 => self.rope_slice.chunk_at_byte(byte_idx.0)
        )
        .map(to_chunk)
    }

    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn chunk_at_char(&self, char_idx: CharOffset) -> Option<Chunk<'rope>> {
        macros::some_if!(
            char_idx <= self.len_chars() => self.rope_slice.chunk_at_char(char_idx.0)
        )
        .map(to_chunk)
    }

    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    #[inline]
    fn chunk_at_line_break(&self, line_break_idx: usize) -> Option<Chunk<'rope>> {
        macros::some_if!(
            line_break_idx <= self.len_lines() => self.rope_slice.chunk_at_line_break(line_break_idx)
        ).map(to_chunk)
    }

    #[inline]
    fn bytes(&self) -> ropey::iter::Bytes<'rope> {
        self.rope_slice.bytes()
    }

    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    fn bytes_at(&self, byte_idx: ByteIndex) -> Option<ropey::iter::Bytes> {
        macros::some_if!(
            byte_idx.0 <= self.len_bytes().0 => self.rope_slice.bytes_at(byte_idx.0)
        )
    }

    #[inline]
    fn chars(&self) -> ropey::iter::Chars<'rope> {
        self.rope_slice.chars()
    }
    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    fn chars_at(&self, char_idx: CharOffset) -> Option<ropey::iter::Chars> {
        macros::some_if!(
            char_idx <= self.len_chars() => self.rope_slice.chars_at(char_idx.0)
        )
    }

    #[inline]
    fn lines(&self) -> Lines<'rope> {
        self.rope_slice.lines().map(to_rope_line)
    }

    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    fn lines_at(&self, line_break_idx: usize) -> Option<Lines> {
        macros::some_if!(
            line_break_idx <= self.len_lines() => self.rope_slice.lines_at(line_break_idx).map(to_rope_line)
        )
    }

    #[inline]
    fn chunks(&self) -> ropey::iter::Chunks<'rope> {
        self.rope_slice.chunks()
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx >= len_lines()`).
    #[inline]
    fn line(&self, line_idx: LineIndex) -> Option<RopeLine<'rope>> {
        macros::some_if!(
            line_idx < self.len_lines().0 => to_rope_line(self.rope_slice.line(line_idx.0))
        )
    }

    /// Returns `None` if the start of the range is greater than the end, or if the
    /// end is out of bounds (i.e. `end > len_chars()`).
    #[inline]
    fn slice<R>(&self, char_range: R) -> Option<Self>
    where
        R: RangeBounds<CharOffset>,
    {
        to_slice_range(char_range, self.len_chars()).map(|r| RopeSlice {
            rope_slice: self.rope_slice.slice(r),
        })
    }

    // Returns a Some only if the str can be safely sliced without a memory allocation.
    fn as_str_if_no_allocation_needed(&self) -> Option<&'rope str> {
        self.rope_slice.as_str()
    }

    fn as_cow_str(&self) -> Cow<'rope, str> {
        self.rope_slice.into()
    }
}

impl<'rope> From<&'rope str> for RopeSlice<'rope> {
    #[inline]
    fn from(text: &'rope str) -> Self {
        RopeSlice {
            rope_slice: ropey::RopeSlice::from(text),
        }
    }
}

impl<'rope> From<RopeSlice<'rope>> for String {
    #[inline]
    fn from(slice: RopeSlice<'rope>) -> Self {
        Self::from(slice.rope_slice)
    }
}

impl<'rope> From<RopeSlice<'rope>> for std::borrow::Cow<'rope, str> {
    #[inline]
    fn from(slice: RopeSlice<'rope>) -> Self {
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

impl<'rope, 'slice> std::cmp::PartialEq<&'rope str> for RopeSlice<'slice> {
    #[inline]
    fn eq(&self, other: &&'rope str) -> bool {
        self.rope_slice.eq(other)
    }
}

impl<'rope, 'slice> std::cmp::PartialEq<RopeSlice<'slice>> for &'rope str {
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

impl<'rope, 'slice> std::cmp::PartialEq<String> for RopeSlice<'slice> {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.rope_slice.eq(other)
    }
}

impl<'rope, 'slice> std::cmp::PartialEq<RopeSlice<'slice>> for String {
    #[inline]
    fn eq(&self, other: &RopeSlice<'slice>) -> bool {
        self.eq(&other.rope_slice)
    }
}

impl<'rope, 'slice> std::cmp::PartialEq<std::borrow::Cow<'rope, str>> for RopeSlice<'slice> {
    #[inline]
    fn eq(&self, other: &std::borrow::Cow<'rope, str>) -> bool {
        self.rope_slice.eq(other)
    }
}

impl<'rope, 'slice> std::cmp::PartialEq<RopeSlice<'slice>> for std::borrow::Cow<'rope, str> {
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

impl<'rope> RopeSliceTrait<'rope> for RopeLine<'rope> {
    // In general in this impl we punch through to the `rope_slice` field if we didn't cahnge the
    // type, and we just call the method on `.0` if we did, so we don't have to reimplement the
    // logic

    #[inline]
    fn len_bytes(&self) -> ByteLength {
        self.0.len_bytes()
    }

    #[inline]
    fn len_chars(&self) -> CharOffset {
        self.0.len_chars()
    }

    #[inline]
    fn len_lines(&self) -> LineLength {
        self.0.len_lines()
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn byte_to_char(&self, byte_idx: ByteIndex) -> Option<CharOffset> {
        self.0.byte_to_char(byte_idx)
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn byte_to_line(&self, byte_idx: ByteIndex) -> Option<LineIndex> {
        self.0.byte_to_line(byte_idx)
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn char_to_byte(&self, char_idx: CharOffset) -> Option<ByteIndex> {
        self.0.char_to_byte(char_idx)
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn char_to_line(&self, char_idx: CharOffset) -> Option<LineIndex> {
        self.0.char_to_line(char_idx)
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    fn line_to_byte(&self, line_idx: LineIndex) -> Option<ByteIndex> {
        self.0.line_to_byte(line_idx)
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    fn line_to_char(&self, line_idx: LineIndex) -> Option<CharOffset> {
        self.0.line_to_char(line_idx)
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx >= len_bytes()`).
    #[inline]
    fn byte(&self, byte_idx: ByteIndex) -> Option<u8> {
        self.0.byte(byte_idx)
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx >= len_chars()`).
    #[inline]
    fn char(&self, char_idx: CharOffset) -> Option<char> {
        self.0.char(char_idx)
    }

    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    fn chunk_at_byte(&self, byte_idx: ByteIndex) -> Option<Chunk<'rope>> {
        self.0.chunk_at_byte(byte_idx)
    }

    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    fn chunk_at_char(&self, char_idx: CharOffset) -> Option<Chunk<'rope>> {
        self.0.chunk_at_char(char_idx)
    }

    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    #[inline]
    fn chunk_at_line_break(&self, line_break_idx: usize) -> Option<Chunk<'rope>> {
        self.0.chunk_at_line_break(line_break_idx)
    }

    #[inline]
    fn bytes(&self) -> ropey::iter::Bytes<'rope> {
        self.0.rope_slice.bytes()
    }

    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    fn bytes_at(&self, byte_idx: ByteIndex) -> Option<ropey::iter::Bytes> {
        self.0.bytes_at(byte_idx)
    }

    #[inline]
    fn chars(&self) -> ropey::iter::Chars<'rope> {
        self.0.rope_slice.chars()
    }

    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    fn chars_at(&self, char_idx: CharOffset) -> Option<ropey::iter::Chars> {
        self.0.chars_at(char_idx)
    }

    #[inline]
    fn lines(&self) -> Lines<'rope> {
        self.0.lines()
    }

    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    fn lines_at(&self, line_break_idx: usize) -> Option<Lines> {
        self.0.lines_at(line_break_idx)
    }

    #[inline]
    fn chunks(&self) -> Chunks<'rope> {
        self.0.chunks()
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx >= len_lines()`).
    #[inline]
    fn line(&self, line_idx: LineIndex) -> Option<RopeLine<'rope>> {
        self.0.line(line_idx)
    }

    /// Returns `None` if the start of the range is greater than the end, or if the
    /// end is out of bounds (i.e. `end > len_chars()`).
    #[inline]
    fn slice<R>(&self, char_range: R) -> Option<Self>
    where
        R: RangeBounds<CharOffset>,
    {
        self.0.slice(char_range).map(RopeLine)
    }

    // Returns a Some only if the str can be safely sliced without a memory allocation.
    fn as_str_if_no_allocation_needed(&self) -> Option<&'rope str> {
        self.0.rope_slice.as_str()
    }

    fn as_cow_str(&self) -> Cow<'rope, str> {
        self.0.rope_slice.into()
    }
}

impl<'rope> From<&'rope str> for RopeLine<'rope> {
    #[inline]
    fn from(text: &'rope str) -> Self {
        to_rope_line(ropey::RopeSlice::from(text))
    }
}

impl<'rope> From<RopeLine<'rope>> for String {
    #[inline]
    fn from(slice: RopeLine<'rope>) -> Self {
        Self::from(slice.0.rope_slice)
    }
}

impl<'rope> From<RopeLine<'rope>> for std::borrow::Cow<'rope, str> {
    #[inline]
    fn from(slice: RopeLine<'rope>) -> Self {
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

impl<'rope, 'slice> std::cmp::PartialEq<&'rope str> for RopeLine<'slice> {
    #[inline]
    fn eq(&self, other: &&'rope str) -> bool {
        self.0.rope_slice.eq(other)
    }
}

impl<'rope, 'slice> std::cmp::PartialEq<RopeLine<'slice>> for &'rope str {
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

impl<'rope, 'slice> std::cmp::PartialEq<String> for RopeLine<'slice> {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.0.rope_slice.eq(other)
    }
}

impl<'rope, 'slice> std::cmp::PartialEq<RopeLine<'slice>> for String {
    #[inline]
    fn eq(&self, other: &RopeLine<'slice>) -> bool {
        self.eq(&other.0.rope_slice)
    }
}

impl<'rope, 'slice> std::cmp::PartialEq<std::borrow::Cow<'rope, str>> for RopeLine<'slice> {
    #[inline]
    fn eq(&self, other: &std::borrow::Cow<'rope, str>) -> bool {
        self.0.rope_slice.eq(other)
    }
}

impl<'rope, 'slice> std::cmp::PartialEq<RopeLine<'slice>> for std::borrow::Cow<'rope, str> {
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
