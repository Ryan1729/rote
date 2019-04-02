use macros::display;

/// `offset` indicates a location before or after characters, not at the charaters.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Position {
    pub line: usize,
    pub offset: usize,
}

display! {for Position : Position{ line, offset } in "{}:{}", line, offset}

#[derive(Clone, Debug, Default)]
pub struct Cursor {
    pub position: Position,
    pub sticky_offset: usize,
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
