use macros::{d, fmt_display, integer_newtype, usize_newtype};
use platform_types::{CharOffset, Move, Position};
use std::borrow::{Borrow, BorrowMut};
use std::ops::{Add, Sub};
pub use vec1::Vec1;

/// An index into the buffer's underlying bytes.
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

#[derive(Clone, Debug, PartialEq)]
pub enum CursorState {
    None,
    PressedAgainstWall,
}
d!(for CursorState: CursorState::None);

#[derive(Clone, Debug)]
pub struct Cursor {
    pub position: Position,
    pub sticky_offset: CharOffset,
    pub highlight_position: Option<Position>,
    pub state: CursorState,
}

fmt_display! {
    for Cursor : Cursor {
            position,
            sticky_offset,
            highlight_position,
            ..
        } in "{}({}){}",
        position,
        sticky_offset,
        highlight_position.map(|h| format!("h:{}", h)).unwrap_or_default()
}

impl Cursor {
    pub fn new(position: Position) -> Self {
        Cursor {
            position,
            sticky_offset: position.offset,
            highlight_position: d!(),
            state: d!(),
        }
    }
}

d!(for Cursor: Cursor::new(d!()));

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

pub trait MultiCursorBuffer: Borrow<Vec1<Cursor>> + BorrowMut<Vec1<Cursor>> {
    fn insert(&mut self, ch: char);

    fn delete(&mut self);

    fn move_all_cursors(&mut self, r#move: Move) {
        for i in 0..self.cursors().len() {
            self.move_cursor(i, r#move)
        }
    }

    fn move_cursor(&mut self, index: usize, r#move: Move);

    fn extend_selection_for_all_cursors(&mut self, r#move: Move) {
        for i in 0..self.cursors().len() {
            self.extend_selection(i, r#move)
        }
    }

    fn extend_selection(&mut self, index: usize, r#move: Move);

    fn in_bounds<P: Borrow<Position>>(&self, position: P) -> bool {
        self.find_index(position) != None
    }

    fn find_index<P: Borrow<Position>>(&self, position: P) -> Option<ByteIndex>;

    fn nearest_valid_position_on_same_line<P: Borrow<Position>>(&self, p: P) -> Option<Position>;

    fn cursors(&self) -> &Vec1<Cursor> {
        self.borrow()
    }

    fn cursors_mut(&mut self) -> &mut Vec1<Cursor> {
        self.borrow_mut()
    }
}
