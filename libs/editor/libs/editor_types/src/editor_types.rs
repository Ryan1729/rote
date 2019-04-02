use macros::display;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::ops::Add;
use std::ops::AddAssign;
use std::ops::Sub;
use std::ops::SubAssign;

macro_rules! usize_newtype {
    ($name: ident) => {
        impl Add<usize> for $name {
            type Output = $name;

            fn add(self, other: usize) -> $name {
                $name(self.0 + other)
            }
        }

        impl AddAssign<usize> for $name {
            fn add_assign(&mut self, other: usize) {
                *self = self.add(other);
            }
        }

        impl Sub<usize> for $name {
            type Output = $name;

            fn sub(self, other: usize) -> $name {
                //Should this be saturating?
                $name(self.0 - other)
            }
        }

        impl SubAssign<usize> for $name {
            fn sub_assign(&mut self, other: usize) {
                *self = self.sub(other);
            }
        }

        impl Sub<$name> for usize {
            type Output = $name;

            fn sub(self, other: $name) -> $name {
                //Should this be saturating?
                $name(self - other.0)
            }
        }

        impl PartialOrd<$name> for usize {
            fn partial_cmp(&self, other: &$name) -> Option<Ordering> {
                Some(self.cmp(&other.0))
            }
        }

        impl PartialEq<$name> for usize {
            fn eq(&self, other: &$name) -> bool {
                *self == other.0
            }
        }

        impl PartialOrd<usize> for $name {
            fn partial_cmp(&self, other: &usize) -> Option<Ordering> {
                Some(self.0.cmp(&other))
            }
        }

        impl PartialEq<usize> for $name {
            fn eq(&self, other: &usize) -> bool {
                self.0 == *other
            }
        }
    };
}

macro_rules! number_newtype {
    ($name: ident) => {
        impl Add for $name {
            type Output = $name;

            fn add(self, other: $name) -> $name {
                $name(self.0 + other.0)
            }
        }
        impl Sub for $name {
            type Output = $name;

            fn sub(self, other: $name) -> $name {
                $name(self.0 - other.0)
            }
        }
    };
}

/// The nth space between utf8 characters, not including the gap. So in the string "aöc" there are
/// four possibe `CharOffset`s. (Note that "ö" is two characters: "o\u{308}".)
/// Here they are represented as vertical bars: "|a|ö|c|"
/// Whatever the state of the gap is, that is how `CharOffset`s are meant to be interpreted.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct CharOffset(pub usize);

usize_newtype! {
    CharOffset
}

number_newtype! {
    CharOffset
}

display! {for CharOffset : CharOffset(offset) in "{}", offset}

/// In index into thebuffer's underlying bytes. Indexes into the gap are possible but usually
/// (always?) undesired.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct ByteIndex(pub usize);

number_newtype! {
    ByteIndex
}

usize_newtype! {
    ByteIndex
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct ByteLength(pub usize);

number_newtype! {
    ByteLength
}

usize_newtype! {
    ByteLength
}

impl Add<ByteLength> for ByteIndex {
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

/// `offset` indicates a location before or after characters, not at the charaters.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Position {
    pub line: usize,
    pub offset: CharOffset,
}

display! {for Position : Position{ line, offset } in "{}:{}", line, offset}

#[derive(Clone, Debug, Default)]
pub struct Cursor {
    pub position: Position,
    pub sticky_offset: CharOffset,
}

display! {for Cursor : Cursor { position, sticky_offset, } in "{}({})", position, sticky_offset}

impl Cursor {
    fn new(position: Position) -> Self {
        Cursor {
            position,
            sticky_offset: position.offset,
        }
    }
}

impl Borrow<Position> for Cursor {
    fn borrow(&self) -> &Position {
        &self.position
    }
}
impl Borrow<Position> for &Cursor {
    fn borrow(&self) -> &Position {
        &self.position
    }
}
