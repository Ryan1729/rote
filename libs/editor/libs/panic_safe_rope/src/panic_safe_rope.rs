///! A wrapper around `ropey::Rope` that checks the panic conditions at runtime and
///! changes the return type of some methods with the aim of preventing panics.

#[derive(Default)]
pub struct Rope {
    rope: ropey::Rope,
}

use macros::{fmt_debug, fmt_display};
use std::io;
use std::iter::FromIterator;
use std::ops::{Bound, RangeBounds};

macro_rules! some_if {
    ($condition: expr => $output: expr) => {{
        if $condition {
            Some($output)
        } else {
            None
        }
    }};
}

impl Rope {
    #[inline]
    pub fn new() -> Self {
        Rope {
            rope: ropey::Rope::new(),
        }
    }

    #[inline]
    pub fn from_str(text: &str) -> Self {
        Rope {
            rope: ropey::Rope::from_str(text),
        }
    }

    pub fn from_reader<T: io::Read>(reader: T) -> io::Result<Self> {
        ropey::Rope::from_reader(reader).map(|rope| Rope { rope })
    }

    pub fn write_to<T: io::Write>(&self, writer: T) -> io::Result<()> {
        self.rope.write_to(writer)
    }

    #[inline]
    pub fn len_bytes(&self) -> usize {
        self.rope.len_bytes()
    }

    #[inline]
    pub fn len_chars(&self) -> usize {
        self.rope.len_chars()
    }

    #[inline]
    pub fn len_lines(&self) -> usize {
        self.rope.len_lines()
    }

    pub fn capacity(&self) -> usize {
        self.rope.capacity()
    }

    pub fn shrink_to_fit(&mut self) {
        self.rope.shrink_to_fit()
    }

    /// Returns `None` and does not mutate if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    pub fn insert(&mut self, char_idx: usize, text: &str) -> Option<()> {
        some_if!(char_idx <= self.len_chars() => self.rope.insert(char_idx, text))
    }

    /// Returns `None` and does not mutate if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    pub fn insert_char(&mut self, char_idx: usize, ch: char) -> Option<()> {
        some_if!(
            char_idx <= self.len_chars() => self.rope.insert_char(char_idx, ch)
        )
    }

    /// Returns `None` and does not mutate if the start of the range is greater than the end, or if the
    /// end is out of bounds (i.e. `end > len_chars()`).
    pub fn remove<R>(&mut self, char_range: R) -> Option<()>
    where
        R: RangeBounds<usize>,
    {
        let start = start_bound_to_num(char_range.start_bound()).unwrap_or(0);
        let end = end_bound_to_num(char_range.end_bound()).unwrap_or_else(|| self.len_chars());

        some_if!(
            start <= end || end <= self.len_chars() => self.rope.remove(char_range)
        )
    }

    /// Returns `None` and does not mutate if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    pub fn split_off(&mut self, char_idx: usize) -> Option<Self> {
        some_if!(
            char_idx <= self.len_chars() => self.rope.split_off(char_idx).into()
        )
    }

    /// Appends a `Rope` to the end of this one, consuming the other `Rope`.
    pub fn append(&mut self, other: Self) {
        self.rope.append(other.rope)
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    pub fn byte_to_char(&self, byte_idx: usize) -> Option<usize> {
        some_if!(byte_idx <= self.len_bytes() => self.rope.byte_to_char(byte_idx))
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    pub fn byte_to_line(&self, byte_idx: usize) -> Option<usize> {
        some_if!(byte_idx <= self.len_bytes() => self.rope.byte_to_line(byte_idx))
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    pub fn char_to_byte(&self, char_idx: usize) -> Option<usize> {
        some_if!(char_idx <= self.len_chars() => self.rope.char_to_byte(char_idx))
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    pub fn char_to_line(&self, char_idx: usize) -> Option<usize> {
        some_if!(
            char_idx < self.len_lines() => self.rope.char_to_line(char_idx)
        )
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    pub fn line_to_byte(&self, line_idx: usize) -> Option<usize> {
        some_if!(
            line_idx < self.len_lines() => self.rope.line_to_byte(line_idx)
        )
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx > len_lines()`).
    #[inline]
    pub fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        some_if!(
            line_idx < self.len_lines() => self.rope.line_to_char(line_idx)
        )
    }

    /// Returns `None`  if `byte_idx` is out of bounds (i.e. `byte_idx >= len_bytes()`).
    #[inline]
    pub fn byte(&self, byte_idx: usize) -> Option<u8> {
        some_if!(
            byte_idx < self.len_bytes() => self.rope.byte(byte_idx)
        )
    }

    /// Returns `None`  if `char_idx` is out of bounds (i.e. `char_idx >= len_chars()`).
    #[inline]
    pub fn char(&self, char_idx: usize) -> Option<char> {
        some_if!(
            char_idx < self.len_chars() => self.rope.char(char_idx)
        )
    }

    /// Returns `None`  if `line_idx` is out of bounds (i.e. `line_idx >= len_lines()`).
    #[inline]
    pub fn line(&self, line_idx: usize) -> Option<ropey::RopeSlice> {
        some_if!(
            line_idx < self.len_lines() => self.rope.line(line_idx)
        )
    }

    /// Returns `None` if `byte_idx` is out of bounds (i.e. `byte_idx > len_bytes()`).
    #[inline]
    pub fn chunk_at_byte(&self, byte_idx: usize) -> Option<(&str, usize, usize, usize)> {
        some_if!(
            byte_idx <= self.len_bytes() => self.rope.chunk_at_byte(byte_idx)
        )
    }

    /// Returns `None` if `char_idx` is out of bounds (i.e. `char_idx > len_chars()`).
    #[inline]
    pub fn chunk_at_char(&self, char_idx: usize) -> Option<(&str, usize, usize, usize)> {
        some_if!(
            char_idx <= self.len_chars() => self.rope.chunk_at_char(char_idx)
        )
    }

    /// Returns `None` if `line_break_idx` is out of bounds (i.e. `line_break_idx > len_lines()`).
    #[inline]
    pub fn chunk_at_line_break(
        &self,
        line_break_idx: usize,
    ) -> Option<(&str, usize, usize, usize)> {
        some_if!(
            line_break_idx <= self.len_lines() => self.rope.chunk_at_line_break(line_break_idx)
        )
    }

    /// Returns `None` if the start of the range is greater than the end, or if the
    /// end is out of bounds (i.e. `end > len_chars()`).
    #[inline]
    pub fn slice<R>(&self, char_range: R) -> Option<ropey::RopeSlice>
    where
        R: RangeBounds<usize>,
    {
        let start = start_bound_to_num(char_range.start_bound()).unwrap_or(0);
        let end = end_bound_to_num(char_range.end_bound()).unwrap_or_else(|| self.len_chars());

        some_if!(
            start <= end || end <= self.len_chars() => self.rope.slice(char_range)
        )
    }

    #[inline]
    pub fn bytes(&self) -> ropey::iter::Bytes {
        self.rope.bytes()
    }

    #[inline]
    pub fn chars(&self) -> ropey::iter::Chars {
        self.rope.chars()
    }

    #[inline]
    pub fn lines(&self) -> ropey::iter::Lines {
        self.rope.lines()
    }

    #[inline]
    pub fn chunks(&self) -> ropey::iter::Chunks {
        self.rope.chunks()
    }
}

impl<'a> From<&'a str> for Rope {
    #[inline]
    fn from(text: &'a str) -> Self {
        Rope::from_str(text)
    }
}

impl<'a> From<std::borrow::Cow<'a, str>> for Rope {
    #[inline]
    fn from(text: std::borrow::Cow<'a, str>) -> Self {
        Rope::from_str(&text)
    }
}

impl From<String> for Rope {
    #[inline]
    fn from(text: String) -> Self {
        Rope::from_str(&text)
    }
}

impl From<Rope> for ropey::Rope {
    #[inline]
    fn from(r: Rope) -> Self {
        r.rope
    }
}

impl From<ropey::Rope> for Rope {
    #[inline]
    fn from(rope: ropey::Rope) -> Self {
        Rope { rope }
    }
}

impl From<Rope> for String {
    #[inline]
    fn from(r: Rope) -> Self {
        From::from(r.rope)
    }
}

impl<'a> From<&'a Rope> for String {
    #[inline]
    fn from(r: &'a Rope) -> Self {
        From::from(&r.rope)
    }
}

impl<'a> From<Rope> for std::borrow::Cow<'a, str> {
    #[inline]
    fn from(r: Rope) -> Self {
        From::from(r.rope)
    }
}

impl<'a> From<&'a Rope> for std::borrow::Cow<'a, str> {
    #[inline]
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

fmt_debug!(for Rope : Rope {rope, ..} in "{:?}", rope);
fmt_display!(for Rope : Rope {rope, ..} in "{}", rope);

impl std::cmp::Eq for Rope {}

impl std::cmp::PartialEq<Rope> for Rope {
    #[inline]
    fn eq(&self, other: &Rope) -> bool {
        self.rope.eq(&other.rope)
    }
}

impl<'a> std::cmp::PartialEq<&'a str> for Rope {
    #[inline]
    fn eq(&self, other: &&'a str) -> bool {
        self.rope.eq(other)
    }
}

impl<'a> std::cmp::PartialEq<Rope> for &'a str {
    #[inline]
    fn eq(&self, other: &Rope) -> bool {
        self.eq(&other.rope)
    }
}

impl std::cmp::PartialEq<str> for Rope {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.rope.eq(other)
    }
}

impl std::cmp::PartialEq<Rope> for str {
    #[inline]
    fn eq(&self, other: &Rope) -> bool {
        self.eq(&other.rope)
    }
}

impl<'a> std::cmp::PartialEq<String> for Rope {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.rope.eq(other)
    }
}

impl<'a> std::cmp::PartialEq<Rope> for String {
    #[inline]
    fn eq(&self, other: &Rope) -> bool {
        self.eq(&other.rope)
    }
}

impl<'a> std::cmp::PartialEq<std::borrow::Cow<'a, str>> for Rope {
    #[inline]
    fn eq(&self, other: &std::borrow::Cow<'a, str>) -> bool {
        self.rope.eq(other)
    }
}

impl<'a> std::cmp::PartialEq<Rope> for std::borrow::Cow<'a, str> {
    #[inline]
    fn eq(&self, other: &Rope) -> bool {
        self.eq(&other.rope)
    }
}

impl std::cmp::Ord for Rope {
    #[inline]
    fn cmp(&self, other: &Rope) -> std::cmp::Ordering {
        self.rope.cmp(&other.rope)
    }
}

impl std::cmp::PartialOrd<Rope> for Rope {
    #[inline]
    fn partial_cmp(&self, other: &Rope) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[inline(always)]
fn start_bound_to_num(b: Bound<&usize>) -> Option<usize> {
    match b {
        Bound::Included(n) => Some(*n),
        Bound::Excluded(n) => Some(*n + 1),
        Bound::Unbounded => None,
    }
}

#[inline(always)]
fn end_bound_to_num(b: Bound<&usize>) -> Option<usize> {
    match b {
        Bound::Included(n) => Some(*n + 1),
        Bound::Excluded(n) => Some(*n),
        Bound::Unbounded => None,
    }
}
