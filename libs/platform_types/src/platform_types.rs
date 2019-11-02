use macros::{
    add_assign, d, fmt_debug, fmt_display, integer_newtype, ord, sub_assign, usize_newtype,
};
use std::path::PathBuf;

pub mod floating_point;

pub mod screen_positioning;
pub use screen_positioning::*;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReplaceOrAdd {
    Replace,
    Add,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Input {
    None,
    Quit,
    CloseMenuIfAny,
    Insert(char),
    Delete,
    ResetScroll,
    ScrollVertically(f32),
    ScrollHorizontally(f32),
    SetSizeDependents(SizeDependents),
    MoveAllCursors(Move),
    ExtendSelectionForAllCursors(Move),
    SelectAll,
    SetCursor(TextBoxSpaceXY, ReplaceOrAdd),
    DragCursors(TextBoxSpaceXY),
    SelectCharTypeGrouping(TextBoxSpaceXY, ReplaceOrAdd),
    SetBufferPath(usize, PathBuf),
    Undo,
    Redo,
    Cut,
    Copy,
    Paste(Option<String>),
    InsertNumbersAtCursors,
    LoadedFile(PathBuf, String),
    NewScratchBuffer,
    TabIn,
    TabOut,
    NextBuffer,
    PreviousBuffer,
    SelectBuffer(BufferId),
    SetFindReplaceMode(FindReplaceMode),
    SubmitForm,
}
d!(for Input : Input::None);

#[derive(Clone, Copy, Debug)]
pub enum BufferId {
    Text(usize),
    Find(usize),
    Replace(usize),
}
d!(for BufferId: BufferId::Text(0));
ord!(and friends for BufferId: id, other in {
    use BufferId::*;
    use std::cmp::Ordering::*;
    match (id, other) {
        (Text(i1), Text(i2))|(Find(i1), Find(i2))|(Replace(i1), Replace(i2)) => i1.cmp(&i2),
        (Text(_), _) => Less,
        (_, Text(_)) => Greater,
        (Find(_), Replace(_)) => Less,
        (Replace(_), Find(_)) => Greater,
    }
});

impl BufferId {
    pub fn get_index(&self) -> usize {
        match self {
            BufferId::Text(i) | BufferId::Find(i) | BufferId::Replace(i) => *i,
        }
    }

    pub fn is_text(&self) -> bool {
        match self {
            BufferId::Text(_) => true,
            _ => false,
        }
    }

    pub fn is_form(&self) -> bool {
        !self.is_text()
    }
}

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

impl AbsoluteCharOffset {
    pub fn saturating_add(self, other: CharOffset) -> Self {
        AbsoluteCharOffset(self.0.saturating_add(other.0))
    }

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

impl AbsoluteCharOffset {
    pub fn saturating_sub(self, other: CharOffset) -> Self {
        AbsoluteCharOffset(self.0.saturating_sub(other.0))
    }

    /// Seems like 99% of the time we want to do a `checked_sub` it's with one
    pub fn checked_sub_one(self) -> Option<Self> {
        self.0.checked_sub(1).map(AbsoluteCharOffset)
    }
}

ord!(and friends for AbsoluteCharOffset: s, other in s.0.cmp(&other.0));

fmt_display! {for AbsoluteCharOffset : AbsoluteCharOffset(offset) in "{}(abs.)", offset}

/// `offset` indicates a location before or after characters, not at the charaters.
#[derive(Copy, Clone, Default, Hash)]
pub struct Position {
    pub line: usize,
    pub offset: CharOffset,
}

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

#[derive(Clone, Copy, Debug)]
pub enum HighlightKind {
    User,
    Result,
    CurrentResult,
}
d!(for HighlightKind: HighlightKind::User);

#[derive(Debug)]
pub struct Highlight {
    pub min: Position,
    pub max: Position,
    pub kind: HighlightKind,
}

impl Highlight {
    pub fn new((p1, p2): (Position, Position), kind: HighlightKind) -> Self {
        Highlight {
            min: std::cmp::min(p1, p2),
            max: std::cmp::max(p1, p2),
            kind,
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
            ),
            d!()
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
    kind: HighlightKind,
) {
    match highlight_position.into() {
        Some(h) if h != position => {
            let min = std::cmp::min(position, h);
            let max = std::cmp::max(position, h);

            if min.line == max.line {
                highlights.push(Highlight::new((min, max), kind));
                return;
            }

            // This early return is merely an optimization from three rectangles to two.
            // TODO Is this optimization actually worth it? The sticky cursor offset does make this
            // more likely than it would otherwise be.
            if min.offset != 0 && min.offset == max.offset {
                let min_middle = min.line + if min.offset == 0 { 0 } else { 1 };
                // Since We know the lines must be different, we know `max.line > 0`
                let max_middle = max.line - 1;

                let offset = min.offset;
                highlights.push(Highlight::new(
                    (
                        Position {
                            offset,
                            line: min.line,
                        },
                        Position {
                            offset: CharOffset(0xFFFF_FFFF__FFFF_FFFF),
                            line: max_middle,
                        },
                    ),
                    kind,
                ));

                highlights.push(Highlight::new(
                    (
                        Position {
                            offset: CharOffset(0),
                            line: min_middle,
                        },
                        Position {
                            offset,
                            line: max.line,
                        },
                    ),
                    kind,
                ));

                return;
            }

            if min.offset != 0 {
                highlights.push(Highlight::new(
                    (
                        min,
                        Position {
                            offset: CharOffset(0xFFFF_FFFF__FFFF_FFFF),
                            ..min
                        },
                    ),
                    kind,
                ));
            }

            let min_middle = min.line + if min.offset == 0 { 0 } else { 1 };
            // Since We know the lines must be different, we know `max.line > 0`
            let max_middle = max.line - 1;
            if min_middle <= max_middle {
                highlights.push(Highlight::new(
                    (
                        Position {
                            offset: CharOffset(0),
                            line: min_middle,
                        },
                        Position {
                            offset: CharOffset(0xFFFF_FFFF__FFFF_FFFF),
                            line: max_middle,
                        },
                    ),
                    kind,
                ));
            }

            if max.offset != 0 {
                highlights.push(Highlight::new(
                    (
                        Position {
                            offset: CharOffset(0),
                            ..max
                        },
                        max,
                    ),
                    kind,
                ));
            }
        }
        _ => {}
    }
}

#[derive(Clone, Debug)]
pub enum BufferName {
    Path(PathBuf),
    Scratch(u32),
}
d!(for BufferName: BufferName::Scratch(d!()));
fmt_display!(for BufferName: name in "{}",
    match name {
        BufferName::Path(p) => p
            .file_name()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| "?Unknown Path?".to_string()),
        BufferName::Scratch(n) => format!("*scratch {}*", n),
    }
 );

#[derive(Clone, Copy)]
pub enum CursorState {
    None,
    PressedAgainstWall,
}
d!(for CursorState: CursorState::None);

fmt_debug!(for CursorState: s in "{}", match s {
    CursorState::None => "_",
    CursorState::PressedAgainstWall => "->|"
});

ord!(and friends for CursorState: state, other in {
    use std::cmp::Ordering::*;
    match (state, other) {
        (CursorState::None, CursorState::None) | (CursorState::PressedAgainstWall, CursorState::PressedAgainstWall) => Equal,
        (CursorState::None, CursorState::PressedAgainstWall) => Less,
        (CursorState::PressedAgainstWall, CursorState::None) => Greater
    }
});

#[derive(Debug)]
pub struct CursorView {
    pub position: Position,
    pub state: CursorState,
}

#[derive(Debug)]
pub struct StatusLineView {
    pub chars: String,
}

pub const DEFAULT_STATUS_LINE_CHARS: &'static str = "No buffer selected.";
d!(for StatusLineView: StatusLineView {chars: DEFAULT_STATUS_LINE_CHARS.to_owned()});

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FindReplaceMode {
    Hidden,
    CurrentFile,
}
d!(for FindReplaceMode: FindReplaceMode::Hidden);

#[derive(Default, Debug)]
pub struct FindReplaceView {
    pub mode: FindReplaceMode,
    pub find: BufferViewData,
    pub replace: BufferViewData,
}

pub type VisibleBuffers = [Option<usize>; 2];

#[derive(Default, Debug)]
pub struct View {
    pub current_buffer_id: BufferId,
    pub visible_buffers: VisibleBuffers,
    pub buffers: Vec<BufferView>,
    pub find_replace: FindReplaceView,
    pub status_line: StatusLineView,
}

#[derive(Default, Debug)]
pub struct BufferView {
    pub name: BufferName,
    // TODO this could be truncated to a fixed length/on the stack
    pub name_string: String,
    pub data: BufferViewData,
}

#[derive(Default, Debug)]
pub struct BufferViewData {
    //TODO make this a &str or a char iterator
    pub chars: String,
    pub scroll: ScrollXY,
    pub cursors: Vec<CursorView>,
    pub highlights: Vec<Highlight>,
}

// Short form "Command".
// This is for telling the platform layer that it should do something in addition to
// rendering the view.
#[derive(Debug, Clone)]
pub enum Cmd {
    NoCmd,
    SetClipboard(String),
}

d!(for Cmd : Cmd::NoCmd);

impl Cmd {
    pub fn take(&mut self) -> Cmd {
        std::mem::replace(self, d!())
    }
}

pub type UpdateAndRenderOutput = (View, Cmd);
pub type UpdateAndRender = fn(Input) -> UpdateAndRenderOutput;

#[cfg(test)]
mod tests;
