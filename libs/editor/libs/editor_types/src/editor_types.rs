use macros::{display, integer_newtype, usize_newtype};
use platform_types::{CharOffset, Position};
use std::borrow::Borrow;
use std::ops::{Add, Sub};

/// In index into thebuffer's underlying bytes. Indexes into the gap are possible but usually
/// (always?) undesired.
#[derive(Clone, Copy, Debug, Default)]
pub struct ByteIndex(pub usize);

integer_newtype! {
    ByteIndex
}

usize_newtype! {
    ByteIndex
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ByteLength(pub usize);

integer_newtype! {
    ByteLength
}

usize_newtype! {
    ByteLength
}

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

#[derive(Clone, Debug, Default)]
pub struct Cursor {
    pub position: Position,
    pub sticky_offset: CharOffset,
}

display! {for Cursor : Cursor { position, sticky_offset, } in "{}({})", position, sticky_offset}

impl Cursor {
    pub fn new(position: Position) -> Self {
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
