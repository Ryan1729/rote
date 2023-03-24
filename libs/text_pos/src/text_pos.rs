#![deny(clippy::float_arithmetic)]
use macros::{
    add_assign, d, fmt_debug, fmt_display, integer_newtype, ord, sub_assign, usize_newtype,
};

/// The nth space between utf8 characters. So in the string "aöc" there are
/// five possibe `CharOffset`s. (Note that "ö" is two characters: "o\u{308}".)
/// Here they are represented as vertical bars: "|a|o|̈|c|"
#[derive(Clone, Copy, Debug, Default)]
pub struct CharOffset(pub usize);

usize_newtype! {
    CharOffset
}

integer_newtype! {
    CharOffset
}

fmt_display! {for CharOffset : CharOffset(offset) in "{}", offset}

/// A `CharOffset` that is counting from the start of the buffer
#[derive(Clone, Copy, Debug, Default)]
pub struct AbsoluteCharOffset(pub usize);

usize_newtype! {
    AbsoluteCharOffset
}

impl From<AbsoluteCharOffset> for CharOffset {
    fn from(index: AbsoluteCharOffset) -> CharOffset {
        CharOffset(index.0)
    }
}

impl std::ops::Add<CharOffset> for AbsoluteCharOffset {
    type Output = AbsoluteCharOffset;

    fn add(self, other: CharOffset) -> AbsoluteCharOffset {
        AbsoluteCharOffset(self.0 + other.0)
    }
}

add_assign!(<CharOffset> for AbsoluteCharOffset);

impl macros::CheckedAdd for AbsoluteCharOffset {
    type Output = CharOffset;
    fn checked_add(self, other: Self) -> Option<CharOffset> {
        self.0.checked_add(other.0).map(CharOffset)
    }
}

impl macros::SaturatingAdd<CharOffset> for AbsoluteCharOffset {
    type Output = AbsoluteCharOffset;

    #[must_use]
    fn saturating_add(self, other: CharOffset) -> AbsoluteCharOffset {
        AbsoluteCharOffset(self.0.saturating_add(other.0))
    }
}

impl AbsoluteCharOffset {
    /// Seems like 99% of the time we want to do a `checked_add` it's with one
    pub fn checked_add_one(self) -> Option<Self> {
        self.0.checked_add(1).map(AbsoluteCharOffset)
    }
}

impl std::ops::Sub<CharOffset> for AbsoluteCharOffset {
    type Output = AbsoluteCharOffset;

    fn sub(self, other: CharOffset) -> AbsoluteCharOffset {
        AbsoluteCharOffset(self.0 - other.0)
    }
}

sub_assign!(<CharOffset> for AbsoluteCharOffset);

impl std::ops::Sub<AbsoluteCharOffset> for AbsoluteCharOffset {
    type Output = CharOffset;

    fn sub(self, other: AbsoluteCharOffset) -> CharOffset {
        CharOffset(self.0 - other.0)
    }
}

/// If two `AbsoluteCharOffset`s are subtracted the result is the relative diffrence, hence
/// `Output = CharOffset`
impl macros::CheckedSub for AbsoluteCharOffset {
    type Output = CharOffset;
    fn checked_sub(self, other: Self) -> Option<CharOffset> {
        self.0.checked_sub(other.0).map(CharOffset)
    }
}

/// If an `AbsoluteCharOffset` has a `CharOffset` subtracted from it, the result is an adjustment
/// of the original offset. hence `Output = AbsoluteCharOffset`
impl macros::CheckedSub<CharOffset> for AbsoluteCharOffset {
    type Output = AbsoluteCharOffset;
    fn checked_sub(self, other: CharOffset) -> Option<AbsoluteCharOffset> {
        self.0.checked_sub(other.0).map(AbsoluteCharOffset)
    }
}

impl macros::SaturatingSub<CharOffset> for AbsoluteCharOffset {
    type Output = AbsoluteCharOffset;

    #[must_use]
    fn saturating_sub(self, other: CharOffset) -> AbsoluteCharOffset {
        AbsoluteCharOffset(self.0.saturating_sub(other.0))
    }
}

impl AbsoluteCharOffset {
    /// Seems like 99% of the time we want to do a `checked_sub` it's with one
    pub fn checked_sub_one(self) -> Option<Self> {
        self.0.checked_sub(1).map(AbsoluteCharOffset)
    }
}

ord!(and friends for AbsoluteCharOffset: s, other in s.0.cmp(&other.0));

fmt_display! {for AbsoluteCharOffset : AbsoluteCharOffset(offset) in "{}(abs.)", offset}

/// `offset` indicates a location before or after characters, not at the charaters.
#[derive(Copy, Clone, Default, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: usize,
    pub offset: CharOffset,
}

ord!(for Position: p, other in {
    p.line
        .cmp(&other.line)
        .then_with(|| p.offset.cmp(&other.offset))
});

#[macro_export]
macro_rules! pos {
    (l $line:literal o $offset:literal) => {
        pos!(l $line, o $offset)
    };
    (l $line:expr, o $offset:expr) => {
        Position {
            line: $line,
            offset: CharOffset($offset),
        }
    };
    () => {
        Position::default()
    };
}

macro_rules! display_max {
    ($n: expr) => {
        if $n == !0 {
            "max".to_string()
        } else {
            $n.to_string()
        }
    };
}

fmt_debug! {
    for Position :
    Position{ line, offset } in "pos!{{l {} o {}}}", line, display_max!(offset.0)
}

fmt_display! {
   for Position :
   Position{ line, offset } in "{}:{}", line, display_max!(offset.0)
}

impl std::str::FromStr for Position {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let chunks: Vec<&str> = s.trim_matches(char::is_whitespace)
                                 .split(':')
                                 .collect();

        let line = chunks.get(0)
            .copied()
            .unwrap_or_default().parse::<usize>()?;
        let offset = chunks.get(1)
            .copied()
            .unwrap_or("0").parse::<usize>()
            .map(CharOffset).unwrap_or_default();

        Ok(Position { line, offset })
    }
}

/// Semantically this is concatenate strings with these final positions together and take the final
/// position. That is, if a string that has as its final position, the position on the left hand
/// side, is concatenated at the beginning of a string with a final position of the position on the
/// right hand side, the resulting string will have the position that results applying this
/// function.
#[must_use]
pub fn append_positions(left: Position, right: Position) -> Position {
    Position {
        line: left.line + right.line,
        offset: right.offset + if right.line == 0 { left.offset } else { d!() },
    }
}

/// The inverse of `append_positions`. That is,
/// `unappend_positions(append_positions(p, q), q) == p`
// TODO proptest this property
#[must_use]
pub fn unappend_positions(left: Position, right: Position) -> Position {
    Position {
        line: left.line - right.line,
        offset: CharOffset(left.offset.0.saturating_sub(if left.line == right.line {
            right.offset.0
        } else {
            0
        })),
    }
}

#[cfg(any(tests, feature = "pub_arb"))]
pub mod tests {
    use super::*;
    pub mod arb {
        use super::*;
        use proptest::{
            prop_compose
        };

        prop_compose! {
            pub fn char_offset(max_len: usize)(offset in 0..=max_len) -> CharOffset {
                CharOffset(offset)
            }
        }

        prop_compose! {
            pub fn pos(max_line: usize, max_offset: usize)
            (line in 0..=max_line, offset in 0..=max_offset) -> Position {
                Position{ line, offset: CharOffset(offset) }
            }
        }
    }
}