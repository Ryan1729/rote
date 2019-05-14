use macros::{fmt_display, integer_newtype, usize_newtype};
use platform_types::{CharOffset, Move, Position};
use std::borrow::Borrow;
use std::ops::{Add, Sub};
use vec1::Vec1;

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

fmt_display! {for Cursor : Cursor { position, sticky_offset, } in "{}({})", position, sticky_offset}

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

pub trait MultiCursorBuffer {
    fn insert(&mut self, ch: char);

    fn delete(&mut self);

    fn move_all_cursors(&mut self, r#move: Move);

    fn move_cursor(&mut self, index: usize, r#move: Move);

    fn in_bounds<P: Borrow<Position>>(&self, position: P) -> bool {
        self.find_index(position) != None
    }

    fn find_index<P: Borrow<Position>>(&self, position: P) -> Option<ByteIndex>;

    fn nearest_valid_position_on_same_line<P: Borrow<Position>>(&self, p: P) -> Option<Position>;

    fn cursors(&self) -> &Vec1<Cursor>;
}
