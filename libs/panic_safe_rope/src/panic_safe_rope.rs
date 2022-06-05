// While I haven't done rigorous benchmarks, some things in this module
// are on hot paths, so I think inline annotations are warranted.
#![allow(clippy::inline_always)]
// It does not seem worth it to change to `slice::Rope`, etc. at this time.
#![allow(clippy::module_name_repetitions)]

mod conversion;
mod slice;

use conversion::{to_chunk, to_rope_line, to_slice_range};
pub use slice::{RopeLine, RopeSlice, RopeSliceTrait, Bytes, Chars, Chunks};

use macros::{fmt_debug, fmt_display, integer_newtype, some_if, usize_newtype};
pub use text_pos::{AbsoluteCharOffset, CharOffset};
pub use is_linebreak_char::is_linebreak_char;

use std::io;
use std::iter::FromIterator;
use std::ops::RangeBounds;

///! A wrapper around `ropey::Rope` that checks the panic conditions at runtime and
///! changes the return type of some methods with the aim of preventing panics.

#[derive(Default, Clone)]
pub struct Rope {
    rope: ropey::Rope,
}

pub type Lines<'rope> =
    std::iter::Map<ropey::iter::Lines<'rope>, fn(ropey::RopeSlice<'rope>) -> RopeLine<'rope>>;

pub type Chunk<'rope> = (&'rope str, ByteIndex, AbsoluteCharOffset, LineIndex);

// TODO add either an `AbsoluteByteIndex` or a `RelativeByteIndex` if we ever start really using
// these again, and the distiction comes up.
/// A zero-based index into the buffer's underlying bytes.
#[derive(Clone, Copy, Debug, Default)]
pub struct ByteIndex(pub usize);

integer_newtype! {
    ByteIndex
}

usize_newtype! {
    ByteIndex
}

fmt_display! {for ByteIndex : ByteIndex(index) in "{}", index}

/// The length of the buffer in bytes. Unless the buffer has grown in the meantime, not a valid
/// byte index
#[derive(Clone, Copy, Debug, Default)]
pub struct ByteLength(pub usize);

integer_newtype! {
    ByteLength
}

usize_newtype! {
    ByteLength
}

fmt_display! {for ByteLength : ByteLength(len) in "{}", len}

impl std::ops::Add<ByteLength> for ByteIndex {
    type Output = ByteIndex;

    fn add(self, other: ByteLength) -> ByteIndex {
        ByteIndex(self.0 + other.0)
    }
}

impl From<ByteIndex> for ByteLength {
    fn from(index: ByteIndex) -> ByteLength {
        ByteLength(index.0)
    }
}

// TODO add either an `AbsoluteLineIndex` or a `RelativeLineIndex` if the distiction comes up.
/// An zero-based index that can be used to refer to the (n -1)th line of the buffer.
#[derive(Clone, Copy, Debug, Default)]
pub struct LineIndex(pub usize);

integer_newtype! {
    LineIndex
}

usize_newtype! {
    LineIndex
}

// TODO should this use `std::num::NonZeroUsize` or would that just be extra hassle?
/// The total umber of lines in the buffer, that is, one plus the number of line breaks.
/// Unless the buffer has grown in the meantime, not a valid line index.
#[derive(Clone, Copy, Debug, Default)]
pub struct LineLength(pub usize);

integer_newtype! {
    LineLength
}

usize_newtype! {
    LineLength
}

impl std::ops::Add<LineLength> for LineIndex {
    type Output = LineIndex;

    fn add(self, other: LineLength) -> LineIndex {
        LineIndex(self.0 + other.0)
    }
}

impl From<LineIndex> for LineLength {
    fn from(index: LineIndex) -> LineLength {
        LineLength(index.0)
    }
}

//use check_or_no_panic::check_or_no_panic;

impl Rope {
    //#[check_or_no_panic]
    #[must_use]
    #[inline]
    #[perf_viz::record]
    pub fn new() -> Self {
        Rope {
            rope: ropey::Rope::new(),
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    // our from_str cannot fail so the FromStr trait's type doesn't match.
    #[inline]
    pub fn from_str(text: &str) -> Self {
        Rope {
            rope: ropey::Rope::from_str(text),
        }
    }

    /// # Errors
    /// Returns an `Err` if and only if the reader returns one.
    pub fn from_reader<T: io::Read>(reader: T) -> io::Result<Self> {
        ropey::Rope::from_reader(reader).map(|rope| Rope { rope })
    }

    /// # Errors
    /// Returns an `Err` if and only if the writer returns one.
    pub fn write_to<T: io::Write>(&self, writer: T) -> io::Result<()> {
        self.rope.write_to(writer)
    }

    #[must_use]
    pub fn capacity(&self) -> usize {
        self.rope.capacity()
    }

    pub fn shrink_to_fit(&mut self) {
        self.rope.shrink_to_fit();
    }

    #[must_use]
    /// Returns `None` and does not mutate if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    pub fn insert(&mut self, char_idx: AbsoluteCharOffset, text: &str) -> Option<()> {
        some_if!(char_idx <= self.len_chars() => self.rope.insert(char_idx.0, text))
    }

    #[must_use]
    /// Returns `None` and does not mutate if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    pub fn insert_char(&mut self, char_idx: AbsoluteCharOffset, ch: char) -> Option<()> {
        some_if!(
            char_idx <= self.len_chars() => self.rope.insert_char(char_idx.0, ch)
        )
    }

    #[must_use]
    /// Returns `None` and does not mutate if the start of the range is greater than the end, or if the
    /// end is out of bounds (i.e. `end > len_chars()`).
    //#[check_or_no_panic]
    pub fn remove<R>(&mut self, char_range: R) -> Option<()>
    where
        R: RangeBounds<AbsoluteCharOffset>,
    {
        to_slice_range(char_range, self.len_chars()).map(|r| self.rope.remove(r))
    }

    #[must_use]
    /// Returns `None` and does not mutate if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    pub fn split_off(&mut self, char_idx: AbsoluteCharOffset) -> Option<Self> {
        some_if!(
            char_idx <= self.len_chars() => self.rope.split_off(char_idx.0).into()
        )
    }

    /// Appends a `Rope` to the end of this one, consuming the other `Rope`.
    pub fn append(&mut self, other: Self) {
        self.rope.append(other.rope);
    }

    #[must_use]
    // Begin methods in common with `RopeSlice`
    #[inline]
    #[perf_viz::record]
    pub fn len_bytes(&self) -> ByteLength {
        ByteLength(self.rope.len_bytes())
    }

    #[must_use]
    #[inline]
    #[perf_viz::record]
    pub fn len_chars(&self) -> AbsoluteCharOffset {
        AbsoluteCharOffset(self.rope.len_chars())
    }

    #[must_use]
    #[inline]
    #[perf_viz::record]
    pub fn len_lines(&self) -> LineLength {
        LineLength(self.rope.len_lines())
    }

    #[must_use]
    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    #[perf_viz::record]
    pub fn byte_to_char(&self, byte_idx: ByteIndex) -> Option<AbsoluteCharOffset> {
        macros::some_if!(byte_idx <= self.len_bytes().0 => AbsoluteCharOffset(self.rope.byte_to_char(byte_idx.0)))
    }

    #[must_use]
    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    #[perf_viz::record]
    pub fn byte_to_line(&self, byte_idx: ByteIndex) -> Option<LineIndex> {
        macros::some_if!(byte_idx <= self.len_bytes().0 => LineIndex(self.rope.byte_to_line(byte_idx.0)))
    }

    #[must_use]
    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    #[perf_viz::record]
    pub fn char_to_byte(&self, char_idx: AbsoluteCharOffset) -> Option<ByteIndex> {
        macros::some_if!(char_idx <= self.len_chars() => ByteIndex(self.rope.char_to_byte(char_idx.0)))
    }

    #[must_use]
    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    #[perf_viz::record]
    pub fn char_to_line(&self, char_idx: AbsoluteCharOffset) -> Option<LineIndex> {
        macros::some_if!(
            char_idx <= self.len_chars().0 => LineIndex(self.rope.char_to_line(char_idx.0))
        )
    }

    #[must_use]
    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    #[perf_viz::record]
    pub fn line_to_byte(&self, line_idx: LineIndex) -> Option<ByteIndex> {
        macros::some_if!(
            line_idx <= self.len_lines().0 => ByteIndex(self.rope.line_to_byte(line_idx.0))
        )
    }

    #[must_use]
    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    #[perf_viz::record]
    pub fn line_to_char(&self, line_idx: LineIndex) -> Option<AbsoluteCharOffset> {
        macros::some_if!(
            line_idx <= self.len_lines().0 => AbsoluteCharOffset(self.rope.line_to_char(line_idx.0))
        )
    }

    #[must_use]
    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx >= len_bytes()`).
    #[inline]
    #[perf_viz::record]
    pub fn byte(&self, byte_idx: ByteIndex) -> Option<u8> {
        macros::some_if!(
            byte_idx < self.len_bytes().0 => self.rope.byte(byte_idx.0)
        )
    }

    #[must_use]
    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx >= len_chars()`).
    #[inline]
    #[perf_viz::record]
    pub fn char(&self, char_idx: AbsoluteCharOffset) -> Option<char> {
        macros::some_if!(
            char_idx < self.len_chars().0 => self.rope.char(char_idx.0)
        )
    }

    #[must_use]
    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    #[perf_viz::record]
    pub fn chunk_at_byte(&self, byte_idx: ByteIndex) -> Option<Chunk> {
        macros::some_if!(
            byte_idx <= self.len_bytes().0 => self.rope.chunk_at_byte(byte_idx.0)
        )
        .map(to_chunk)
    }

    #[must_use]
    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    #[perf_viz::record]
    pub fn chunk_at_char(&self, char_idx: AbsoluteCharOffset) -> Option<Chunk> {
        macros::some_if!(
            char_idx <= self.len_chars().0 => self.rope.chunk_at_char(char_idx.0)
        )
        .map(to_chunk)
    }

    #[must_use]
    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    #[inline]
    #[perf_viz::record]
    pub fn chunk_at_line_break(&self, line_break_idx: usize) -> Option<Chunk> {
        macros::some_if!(
            line_break_idx <= self.len_lines().0 => self.rope.chunk_at_line_break(line_break_idx)
        )
        .map(to_chunk)
    }

    #[must_use]
    #[inline]
    #[perf_viz::record]
    pub fn bytes(&self) -> ropey::iter::Bytes {
        self.rope.bytes()
    }

    #[must_use]
    #[inline]
    #[perf_viz::record]
    pub fn chars(&self) -> ropey::iter::Chars {
        self.rope.chars()
    }

    #[must_use]
    #[inline]
    #[perf_viz::record]
    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    pub fn chars_at(&self, char_idx: AbsoluteCharOffset) -> Option<ropey::iter::Chars> {
        macros::some_if!(
            char_idx <= self.len_chars() => self.rope.chars_at(char_idx.0)
        )
    }

    #[must_use]
    /// Equivalent to `r.chars_at(r.len_chars()).unwrap()`
    #[inline]
    #[perf_viz::record]
    pub fn chars_at_end(&self) -> ropey::iter::Chars {
        self.rope.chars_at(self.rope.len_chars())
    }

    #[inline]
    #[perf_viz::record]
    pub fn lines(&self) -> Lines {
        self.rope.lines().map(to_rope_line)
    }

    #[must_use]
    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    #[inline]
    #[perf_viz::record]
    pub fn lines_at(&self, line_break_idx: usize) -> Option<Lines> {
        macros::some_if!(
            line_break_idx <= self.len_lines() => self.rope.lines_at(line_break_idx).map(to_rope_line)
        )
    }

    #[must_use]
    #[inline]
    #[perf_viz::record]
    pub fn chunks(&self) -> ropey::iter::Chunks {
        self.rope.chunks()
    }

    #[must_use]
    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx >= len_lines()`).
    #[inline]
    #[perf_viz::record]
    pub fn line(&self, line_idx: LineIndex) -> Option<RopeLine> {
        macros::some_if!(
            line_idx < self.len_lines().0 => to_rope_line(self.rope.line(line_idx.0))
        )
    }

    #[must_use]
    /// Returns `None` if the start of the range is greater than the end, or if the
    /// end is out of bounds (i.e. `end > len_chars()`).
    #[inline]
    #[perf_viz::record]
    pub fn slice<R>(&self, char_range: R) -> Option<RopeSlice>
    where
        R: RangeBounds<AbsoluteCharOffset>,
    {
        to_slice_range(char_range, self.len_chars()).map(|r| RopeSlice {
            rope_slice: self.rope.slice(r),
        })
    }

    #[must_use]
    /// Equivalent to `slice(..0)` except it always returns a `RopeSlice`
    #[inline]
    #[perf_viz::record]
    pub fn empty_slice(&self) -> RopeSlice {
        RopeSlice {
            rope_slice: self.rope.slice(..0),
        }
    }

    #[must_use]
    /// Equivalent to `slice(..)` except it always returns a `RopeSlice`
    #[inline]
    #[perf_viz::record]
    pub fn full_slice(&self) -> RopeSlice {
        RopeSlice {
            rope_slice: self.rope.slice(0..self.len_chars().0),
        }
    }
}

fmt_debug!(for Rope : Rope {rope, ..} in "{:?}", rope);
fmt_display!(for Rope : Rope {rope, ..} in "{}", rope);

impl<'a> From<&'a str> for Rope {
    #[inline]
    #[perf_viz::record]
    fn from(text: &'a str) -> Self {
        Rope::from_str(text)
    }
}

impl<'a> From<std::borrow::Cow<'a, str>> for Rope {
    #[inline]
    #[perf_viz::record]
    fn from(text: std::borrow::Cow<'a, str>) -> Self {
        Rope::from_str(&text)
    }
}

impl From<String> for Rope {
    #[inline]
    #[perf_viz::record]
    fn from(text: String) -> Self {
        Rope::from_str(&text)
    }
}

impl From<Rope> for ropey::Rope {
    #[inline]
    #[perf_viz::record]
    fn from(r: Rope) -> Self {
        r.rope
    }
}

impl From<ropey::Rope> for Rope {
    #[inline]
    #[perf_viz::record]
    fn from(rope: ropey::Rope) -> Self {
        Rope { rope }
    }
}

impl From<Rope> for String {
    #[inline]
    #[perf_viz::record]
    fn from(r: Rope) -> Self {
        From::from(r.rope)
    }
}

impl<'a> From<&'a Rope> for String {
    #[inline]
    #[perf_viz::record]
    fn from(r: &'a Rope) -> Self {
        From::from(&r.rope)
    }
}

impl<'a> From<Rope> for std::borrow::Cow<'a, str> {
    #[inline]
    #[perf_viz::record]
    fn from(r: Rope) -> Self {
        From::from(r.rope)
    }
}

impl<'a> From<&'a Rope> for std::borrow::Cow<'a, str> {
    #[inline]
    #[perf_viz::record]
    fn from(r: &'a Rope) -> Self {
        From::from(&r.rope)
    }
}

impl<'a> FromIterator<&'a str> for Rope {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = &'a str>,
    {
        ropey::Rope::from_iter(iter).into()
    }
}

impl<'a> FromIterator<std::borrow::Cow<'a, str>> for Rope {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = std::borrow::Cow<'a, str>>,
    {
        ropey::Rope::from_iter(iter).into()
    }
}

impl FromIterator<String> for Rope {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = String>,
    {
        ropey::Rope::from_iter(iter).into()
    }
}

impl std::cmp::Eq for Rope {}

impl std::cmp::PartialEq<Rope> for Rope {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &Rope) -> bool {
        self.rope.eq(&other.rope)
    }
}

impl std::cmp::PartialEq<&Rope> for Rope {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &&Rope) -> bool {
        self.rope.eq(&other.rope)
    }
}

impl std::cmp::PartialEq<Rope> for &Rope {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &Rope) -> bool {
        self.rope.eq(&other.rope)
    }
}

impl<'a> std::cmp::PartialEq<&'a str> for Rope {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &&'a str) -> bool {
        self.rope.eq(other)
    }
}

impl<'a> std::cmp::PartialEq<Rope> for &'a str {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &Rope) -> bool {
        self.eq(&other.rope)
    }
}

impl std::cmp::PartialEq<str> for Rope {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &str) -> bool {
        self.rope.eq(other)
    }
}

impl std::cmp::PartialEq<Rope> for str {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &Rope) -> bool {
        self.eq(&other.rope)
    }
}

impl<'a> std::cmp::PartialEq<String> for Rope {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &String) -> bool {
        self.rope.eq(other)
    }
}

impl<'a> std::cmp::PartialEq<Rope> for String {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &Rope) -> bool {
        self.eq(&other.rope)
    }
}

impl<'a> std::cmp::PartialEq<std::borrow::Cow<'a, str>> for Rope {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &std::borrow::Cow<'a, str>) -> bool {
        self.rope.eq(other)
    }
}

impl<'a> std::cmp::PartialEq<Rope> for std::borrow::Cow<'a, str> {
    #[inline]
    #[perf_viz::record]
    fn eq(&self, other: &Rope) -> bool {
        self.eq(&other.rope)
    }
}

impl std::cmp::Ord for Rope {
    #[inline]
    #[perf_viz::record]
    fn cmp(&self, other: &Rope) -> std::cmp::Ordering {
        self.rope.cmp(&other.rope)
    }
}

impl std::cmp::PartialOrd<Rope> for Rope {
    #[inline]
    #[perf_viz::record]
    fn partial_cmp(&self, other: &Rope) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests;