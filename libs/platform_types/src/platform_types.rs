use macros::{d, display, number_newtype, usize_newtype};
use std::ops::{Add, Sub};

#[derive(Clone, Copy, Debug)]
pub enum Move {
    Up,
    Down,
    Left,
    Right,
    ToLineStart,
    ToLineEnd,
    ToBufferStart,
    ToBufferEnd,
}

#[derive(Clone, Copy, Debug)]
pub enum Input {
    None,
    Insert(char),
    Delete,
    ResetScroll,
    ScrollVertically(f32),
    ScrollHorizontally(f32),
    SetSizes(Sizes),
    SetMousePos((f32, f32)),
    MoveAllCursors(Move),
    ReplaceCursors(Position),
}

d!(for Input : Input::None);

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

/// `offset` indicates a location before or after characters, not at the charaters.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Position {
    pub line: usize,
    pub offset: CharOffset,
}

display! {for Position : Position{ line, offset } in "{}:{}", line, offset}

#[derive(Clone, Copy, Debug)]
pub struct Sizes {
    pub screen_w: Option<f32>,
    pub screen_h: Option<f32>,
    pub char_w: Option<f32>,
    pub line_h: Option<f32>,
}

#[macro_export]
macro_rules! Sizes {
    {
        screen_w: $screen_w:expr,
        screen_h: $screen_h:expr,
        char_w: $char_w:expr,
        line_h: $line_h:expr $(,)?
    } => (
        Sizes {
            screen_w: $screen_w.into(),
            screen_h: $screen_h.into(),
            char_w: $char_w.into(),
            line_h: $line_h.into(),
        }
    );
}

#[derive(Default)]
pub struct View {
    pub buffers: Vec<BufferView>,
}

#[derive(Copy, Clone, Debug)]
pub enum BufferViewKind {
    Edit,
    StatusLine,
    Cursor,
}

pub struct BufferView {
    pub kind: BufferViewKind,
    pub screen_position: (f32, f32),
    pub bounds: (f32, f32),
    pub color: [f32; 4],
    //TODO make this a &str or a char iterator
    pub chars: String,
}

pub enum Cmd {
    NoCmd, //The plan is to communicate things like saving to the platform layer with this
}

d!(for Cmd : Cmd::NoCmd);

pub type UpdateAndRender = fn(Input) -> (View, Cmd);
