use macros::{d, fmt_debug, fmt_display, integer_newtype, usize_newtype, ord};
use std::ops::{Add, Sub};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Move {
    Up,
    Down,
    Left,
    Right,
    ToLineStart,
    ToLineEnd,
    ToBufferStart,
    ToBufferEnd,
    ToPreviousLikelyEditLocation,
    ToNextLikelyEditLocation,
}

impl std::ops::Not for Move {
    type Output = Move;

    fn not(self) -> Self::Output {
        use Move::*;
        match self {
            Up => Down,
            Down => Up,
            Left => Right,
            Right => Left,
            ToLineStart => ToLineEnd,
            ToLineEnd => ToLineStart,
            ToBufferStart => ToBufferEnd,
            ToBufferEnd => ToBufferStart,
            ToPreviousLikelyEditLocation => ToNextLikelyEditLocation,
            ToNextLikelyEditLocation => ToPreviousLikelyEditLocation,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ScreenSpaceXY {
    pub x: f32,
    pub y: f32,
}

impl From<ScreenSpaceXY> for (f32, f32) {
    fn from(ScreenSpaceXY { x, y }: ScreenSpaceXY) -> Self {
        (x, y)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReplaceOrAdd {
    Replace,
    Add
}

#[derive(Clone, Debug, PartialEq)]
pub enum Input {
    None,
    Quit,
    Insert(char),
    Delete,
    ResetScroll,
    ScrollVertically(f32),
    ScrollHorizontally(f32),
    SetSizes(Sizes),
    MoveAllCursors(Move),
    ExtendSelectionForAllCursors(Move),
    SetCursor(ScreenSpaceXY, ReplaceOrAdd),
    DragCursors(ScreenSpaceXY),
    SelectCharTypeGrouping(ScreenSpaceXY, ReplaceOrAdd),
    Undo,
    Redo,
    Cut,
    Copy,
    Paste(Option<String>)
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
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

pub enum PositionRound {
    Up,
    TowardsZero,
}

fn normal_or_zero(x: f32) -> f32 {
    if x.is_normal() { x } else { 0.0 }
}

pub fn screen_space_to_position(
    ScreenSpaceXY { x, y }: ScreenSpaceXY,
    CharDim { w, h }: CharDim,
    (scroll_x, scroll_y): (f32, f32),
    round: PositionRound
) -> Position {
    // This is made much more conveinient by the monospace assumption!

    let pre_rounded = (x - scroll_x) / w;

    // if the value would not fit in a `usize` then the `as usize` is undefiend behaviour.
    // https://github.com/rust-lang/rust/issues/10184
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=21e5f8c502c8e6e16a685449ccc9db82
    let offset = normal_or_zero(
        match round {
            PositionRound::TowardsZero => pre_rounded,
            PositionRound::Up => {
                // The right half of a character should correspond to the position to the
                // right of the character.
                pre_rounded + 0.5
            },
        }
    ) as usize;
    let line = normal_or_zero((y - scroll_y) / h) as usize;

    Position {
        offset: CharOffset(offset),
        line,
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
/// five possibe `CharOffset`s. (Note that "ö" is two characters: "o\u{308}".)
/// Here they are represented as vertical bars: "|a|o|̈|c|"
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
#[derive(Copy, Clone, Default, Hash)]
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

/// Semantically this is concatenate strings with these final positions together and take the final
/// position. That is, if a string that has as its final position, the position on the lef- hand
/// side, is concatenated at the beginning of a string with a final position of the position on the
/// right hand side, the resulting string will have the position that results applying this
/// function.
pub fn append_positions(left: Position, right: Position) -> Position {
    Position {
        line: left.line + right.line,
        offset: right.offset + if right.line == 0 { left.offset } else { d!() },
    }
}

/// The inverse of `append_positions`. That is,
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

ord!(and friends for Position: p, other in {
    p.line
        .cmp(&other.line)
        .then_with(|| p.offset.cmp(&other.offset))
});

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

pub const DEFAULT_HIGHLIGHT_COLOUR: [f32; 4] = [0.0, 0.0, 0.0, 0.6];

#[derive(Debug)]
pub struct Highlight {
    pub min: Position,
    pub max: Position,
    pub color: [f32; 4],
}

impl Highlight {
    pub fn new((p1, p2): (Position, Position)) -> Self {
        Highlight {
            min: std::cmp::min(p1, p2),
            max: std::cmp::max(p1, p2),
            color: DEFAULT_HIGHLIGHT_COLOUR,
        }
    }

    pub fn get(&self) -> (Position, Position) {
        (self.min, self.max)
    }
}

#[macro_export]
macro_rules! highlight {
    (l $min_line:literal o $min_offset:literal l $max_line:literal o $max_offset:literal ) => {
        Highlight::new(
            (
                Position {
                    line: $min_line,
                    offset: CharOffset($min_offset),
                },
                Position {
                    line: $max_line,
                    offset: CharOffset($max_offset),
                },
            )
        )
    };
    (l $min_line:literal o $min_offset:literal l $max_line:literal o max ) => {
        highlight!(l $min_line o $min_offset l $max_line o 0xFFFF_FFFF__FFFF_FFFF)
    };
}

pub fn push_highlights<O: Into<Option<Position>>>(
    highlights: &mut Vec<Highlight>,
    position: Position,
    highlight_position: O,
) {
    match highlight_position.into() {
        Some(h) if h != position => {
            let min = std::cmp::min(position, h);
            let max = std::cmp::max(position, h);

            if min.line == max.line {
                highlights.push(Highlight::new((min, max)));
                return;
            }

            //This early return is merely an optimization from three rectangles to two.
            // TODO Is this optimization actually worth it? The sticky cursor offset does make this
            // more likely that it would otherwise be.
            if min.offset != 0 && min.offset == max.offset {
                let min_middle = min.line + if min.offset == 0 { 0 } else { 1 };
                // Since We know the lines must be different, we know `max.line > 0`
                let max_middle = max.line - 1;

                let offset = min.offset;
                highlights.push(Highlight::new((
                    Position {
                        offset,
                        line: min.line,
                    },
                    Position {
                        offset: CharOffset(0xFFFF_FFFF__FFFF_FFFF),
                        line: max_middle,
                    },
                )));

                highlights.push(Highlight::new((
                    Position {
                        offset: CharOffset(0),
                        line: min_middle,
                    },
                    Position {
                        offset,
                        line: max.line,
                    },
                )));

                return;
            }

            if min.offset != 0 {
                highlights.push(Highlight::new((
                    min,
                    Position {
                        offset: CharOffset(0xFFFF_FFFF__FFFF_FFFF),
                        ..min
                    },
                )));
            }

            let min_middle = min.line + if min.offset == 0 { 0 } else { 1 };
            // Since We know the lines must be different, we know `max.line > 0`
            let max_middle = max.line - 1;
            if min_middle <= max_middle {
                highlights.push(Highlight::new((
                    Position {
                        offset: CharOffset(0),
                        line: min_middle,
                    },
                    Position {
                        offset: CharOffset(0xFFFF_FFFF__FFFF_FFFF),
                        line: max_middle,
                    },
                )));
            }

            if max.offset != 0 {
                highlights.push(Highlight::new((
                    Position {
                        offset: CharOffset(0),
                        ..max
                    },
                    max,
                )));
            }
        }
        _ => {}
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

 // Short form "Command".
 // This is for telling the platform layer that it should do things something in addition to
 // rendering the view.
#[derive(Debug, Clone)]
pub enum Cmd {
    NoCmd,
    SetClipboard(String)
}

d!(for Cmd : Cmd::NoCmd);

impl Cmd {
    pub fn take(&mut self) -> Cmd {
        std::mem::replace(self, d!())
    }
}

pub type UpdateAndRenderOutput = (View, Cmd);
pub type UpdateAndRender = fn(Input) -> UpdateAndRenderOutput;

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[cfg(test)]
mod tests;
