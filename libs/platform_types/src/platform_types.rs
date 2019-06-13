use macros::{d, fmt_debug, fmt_display, integer_newtype, usize_newtype};
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
pub struct ScreenSpaceXY {
    pub x: f32,
    pub y: f32,
}

impl From<ScreenSpaceXY> for (f32, f32) {
    fn from(ScreenSpaceXY { x, y }: ScreenSpaceXY) -> Self {
        (x, y)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Input {
    None,
    Quit,
    Insert(char),
    Delete,
    ResetScroll,
    ScrollVertically(f32),
    ScrollHorizontally(f32),
    SetSizes(Sizes),
    SetMousePos(ScreenSpaceXY),
    MoveAllCursors(Move),
    ExtendSelectionForAllCursors(Move),
    ReplaceCursors(ScreenSpaceXY),
}

#[derive(Clone, Copy, Debug, Default)]
///We are currently assuming the font is monospace!
pub struct CharDim {
    pub w: f32,
    pub h: f32,
}

impl From<CharDim> for (f32, f32) {
    fn from(CharDim { w, h }: CharDim) -> Self {
        (w, h)
    }
}

pub fn screen_space_to_position(
    ScreenSpaceXY { x, y }: ScreenSpaceXY,
    CharDim { w, h }: CharDim,
    (scroll_x, scroll_y): (f32, f32),
) -> Position {
    // This is made much more conveinient by the monospace assumption!
    Position {
        offset: CharOffset(
            //conveniently `(x / 0.0) as usize` is `0` rather than a panic.
            (((x - scroll_x) / w)
                // The right half of a character should correspond to the position to the right of
                // the character.
                + 0.5) as usize,
        ),
        line: ((y - scroll_y) / h) as usize,
    }
}

pub fn position_to_screen_space(
    Position { offset, line }: Position,
    CharDim { w, h }: CharDim,
    (scroll_x, scroll_y): (f32, f32),
) -> ScreenSpaceXY {
    // This is made much more conveinient by the monospace assumption!

    // Weird *graphical-only* stuff given a >2^24 long line and/or >2^24
    // lines seems better than an error box or something like that.
    #[allow(clippy::cast_precision_loss)]
    ScreenSpaceXY {
        x: offset.0 as f32 * w + scroll_x,
        y: line as f32 * h + scroll_y,
    }
}

d!(for Input : Input::None);

/// The nth space between utf8 characters. So in the string "aöc" there are
/// four possibe `CharOffset`s. (Note that "ö" is two characters: "o\u{308}".)
/// Here they are represented as vertical bars: "|a|ö|c|"
#[derive(Clone, Copy, Debug, Default, Hash)]
pub struct CharOffset(pub usize);

usize_newtype! {
    CharOffset
}

integer_newtype! {
    CharOffset
}

fmt_display! {for CharOffset : CharOffset(offset) in "{}", offset}

/// A `CharOffset` that is counting from the start of the buffer
#[derive(Clone, Copy, Debug, Default, Hash)]
pub struct AbsoluteCharOffset(pub usize);

usize_newtype! {
    AbsoluteCharOffset
}

integer_newtype! {
    AbsoluteCharOffset
}

fmt_display! {for AbsoluteCharOffset : AbsoluteCharOffset(offset) in "{}(abs.)", offset}

impl std::ops::Add<CharOffset> for AbsoluteCharOffset {
    type Output = AbsoluteCharOffset;

    fn add(self, other: CharOffset) -> AbsoluteCharOffset {
        AbsoluteCharOffset(self.0 + other.0)
    }
}

impl From<AbsoluteCharOffset> for CharOffset {
    fn from(index: AbsoluteCharOffset) -> CharOffset {
        CharOffset(index.0)
    }
}

/// `offset` indicates a location before or after characters, not at the charaters.
#[derive(Copy, Clone, Default, Eq, Hash)]
pub struct Position {
    pub line: usize,
    pub offset: CharOffset,
}

#[macro_export]
macro_rules! pos {
    (l $line:literal o $offset:literal) => {
        Position {
            line: $line,
            offset: CharOffset($offset),
        }
    };
    () => {
        Position::default()
    };
}

fmt_debug! {for Position : Position{ line, offset } in "pos!{{l {} o {}}}", line, offset}

fmt_display! {for Position : Position{ line, offset } in "{}:{}", line, offset}

/// Semantically this is concatenate strings with these final positions together and take the final
/// postion. That is, if a string that has as its final position, the position on the lef- hand
/// side, is concatenated at the beginning of a string with a final position of the position on the
/// right hand side, the resulting string will have the position that results applying this
/// function.
pub fn append_positions(left: Position, right: Position) -> Position {
    Position {
        line: left.line + right.line,
        offset: right.offset + if right.line == 0 { left.offset } else { d!() },
    }
}

/// THe inverse of `append_positions`. That is,
/// `unappend_positions(append_positions(p, q), q) == p`
// TODO proptest this property
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

use std::cmp::Ordering;
impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Position) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Position) -> Ordering {
        self.line
            .cmp(&other.line)
            .then_with(|| self.offset.cmp(&other.offset))
    }
}

impl PartialEq for Position {
    fn eq(&self, other: &Position) -> bool {
        self.line == other.line && self.offset == other.offset
    }
}

#[derive(Default, Debug)]
pub struct View {
    pub buffers: Vec<BufferView>,
}

#[derive(Copy, Clone, Debug)]
pub enum BufferViewKind {
    Edit,
    StatusLine,
    Cursor,
}

d!(for BufferViewKind: BufferViewKind::Cursor);

#[derive(Debug)]
pub struct Highlight {
    min: Position,
    max: Position,
}

impl Highlight {
    pub fn new((p1, p2): (Position, Position)) -> Self {
        Highlight {
            min: std::cmp::min(p1, p2),
            max: std::cmp::max(p1, p2),
        }
    }

    pub fn get(&self) -> (Position, Position) {
        (self.min, self.max)
    }
}

#[derive(Default, Debug)]
pub struct BufferView {
    pub kind: BufferViewKind,
    pub screen_position: (f32, f32),
    pub bounds: (f32, f32),
    pub color: [f32; 4],
    //TODO make this a &str or a char iterator
    pub chars: String,
    pub highlights: Vec<Highlight>,
}

#[derive(Debug, Clone, Copy)]
pub enum Cmd {
    NoCmd, //The plan is to communicate things like saving to the platform layer with this
}

d!(for Cmd : Cmd::NoCmd);

pub type UpdateAndRenderOutput = (View, Cmd);
pub type UpdateAndRender = fn(Input) -> UpdateAndRenderOutput;

#[derive(Clone, Copy, Debug)]
pub struct Sizes {
    pub screen_w: Option<f32>,
    pub screen_h: Option<f32>,
    pub text_char_dim: Option<CharDim>,
    pub status_char_dim: Option<CharDim>,
}

#[macro_export]
macro_rules! Sizes {
    {
        screen_w: $screen_w:expr,
        screen_h: $screen_h:expr,
        text_char_dim: $text_char_dim:expr,
        status_char_dim: $status_char_dim:expr $(,)?
    } => (
        Sizes {
            screen_w: $screen_w.into(),
            screen_h: $screen_h.into(),
            text_char_dim: $text_char_dim.into(),
            status_char_dim: $status_char_dim.into(),
        }
    );
}
