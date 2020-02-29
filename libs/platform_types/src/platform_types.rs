use macros::{
    add_assign, d, fmt_debug, fmt_display, integer_newtype, ord, sub_assign, usize_newtype,
};
use std::path::PathBuf;

pub use vec1::{vec1, Vec1};

pub mod floating_point;

pub mod screen_positioning;
pub use screen_positioning::*;

mod move_mod;
pub use move_mod::Move;

pub mod g_i;
pub use g_i::{SelectionAdjustment, SelectionMove};

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
    DeleteLines,
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
    ExtendSelectionWithSearch,
    SetBufferPath(g_i::Index, PathBuf),
    Undo,
    Redo,
    Cut,
    Copy,
    Paste(Option<String>),
    InsertNumbersAtCursors,
    AddOrSelectBuffer(BufferName, String),
    NewScratchBuffer(Option<String>),
    TabIn,
    TabOut,
    AdjustBufferSelection(SelectionAdjustment),
    NextLanguage,
    SelectBuffer(BufferId),
    OpenOrSelectBuffer(PathBuf),
    CloseBuffer(g_i::Index),
    SetMenuMode(MenuMode),
    SubmitForm,
}
d!(for Input : Input::None);


#[derive(Clone, Copy, Default, Debug)]
pub struct BufferId {
    pub kind: BufferIdKind,
    pub index: g_i::Index,
}
ord!(and friends for BufferId: id, other in {
    id.kind.cmp(&other.kind).then_with(|| id.index.cmp(&other.index))
});

#[macro_export]
macro_rules! b_id {
    //
    // Creation
    //
    ($kind: expr) => {
        BufferId {
            kind: $kind,
            index: d!(),
        }
    };
    ($kind: expr, $index: expr) => {
        BufferId {
            kind: $kind,
            index: $index,
        }
    };
}

#[derive(Clone, Copy, Debug)]
pub enum BufferIdKind {
    /// Used to indicate that the keyboard is focused on a non-buffer.
    None,
    /// Indicates A buffer repesenting an open file or an imemory scratch file.
    /// Almost all buffers are `Text` buffers.
    Text,
    Find,
    Replace,
    FileSwitcher,
    GoToPosition,
}
d!(for BufferIdKind: BufferIdKind::Text);

impl From<&BufferIdKind> for u8 {
    fn from(kind: &BufferIdKind) -> Self {
        use BufferIdKind::*;
        match kind {
            None => 0,
            Text => 1,
            Find => 2,
            Replace => 3,
            FileSwitcher => 4,
            GoToPosition => 5,
        }
    }
}

ord!(and friends for BufferIdKind: kind, other in {
    let k: u8 = kind.into();
    let o: u8 = other.into();
    k.cmp(&o)
});

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

impl std::str::FromStr for Position {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let chunks: Vec<&str> = s.trim_matches(char::is_whitespace)
                                 .split(':')
                                 .collect();

        let line = chunks.get(0)
            .map(|&s| s).unwrap_or_default().parse::<usize>()?;
        let offset = chunks.get(1)
            .map(|&s| s).unwrap_or_else(|| "0").parse::<usize>()
            .map(CharOffset).unwrap_or_default();

        Ok(Position { line, offset })
    }
}

/// Semantically this is concatenate strings with these final positions together and take the final
/// position. That is, if a string that has as its final position, the position on the left hand
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug, Hash)]
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
ord!(and friends for BufferName: name, other in {
     use BufferName::*;
     use std::cmp::Ordering::*;
     match (name, other) {
         (Path(p1), Path(p2)) => {
             match (p1.canonicalize(), p2.canonicalize() ) {
                 (Ok(ref cp1), Ok(ref cp2)) if cp1 == cp2 => {
                     Equal
                 }
                 _ => {
                     p1.cmp(&p2)
                 }
             }
         }
         (Path(_), Scratch(_)) => {
             Less
         }
         (Scratch(_), Path(_)) => {
             Greater
         }
         (Scratch(n1), Scratch(n2)) => {
             n1.cmp(&n2)
         }
     }
 });

impl BufferName {
    pub fn get_extension_or_empty(&self) -> &str {
        use BufferName::*;
        match self {
            Path(p) => {
                p.extension()
                    .and_then(|os_str| os_str.to_str())
                    .unwrap_or("")
            },
            _ => "",
        }
    }
}

#[derive(Clone, Copy)]
pub enum CursorState {
    None,
    PressedAgainstWall(Move),
}
d!(for CursorState: CursorState::None);

fmt_debug!(for CursorState: s in "{}", match s {
    CursorState::None => std::borrow::Cow::Borrowed("_"),
    CursorState::PressedAgainstWall(r#move) => std::borrow::Cow::Owned(format!("->|({})", r#move))
});

ord!(and friends for CursorState: state, other in {
    use std::cmp::Ordering::*;
    match (state, other) {
        (CursorState::None, CursorState::None) => Equal,
        (CursorState::None, CursorState::PressedAgainstWall(_)) => Less,
        (CursorState::PressedAgainstWall(_), CursorState::None) => Greater,
        (CursorState::PressedAgainstWall(m1), CursorState::PressedAgainstWall(m2)) => {
            m1.cmp(&m2)
        }
    }
});

#[derive(Clone, Debug)]
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
pub enum MenuMode {
    Hidden,
    FileSwitcher,
    FindReplace(FindReplaceMode),
    GoToPosition,
}
d!(for MenuMode: MenuMode::Hidden);

#[derive(Debug)]
pub enum MenuView {
    None,
    FileSwitcher(FileSwitcherView),
    FindReplace(FindReplaceView),
    GoToPosition(GoToPositionView)
}
d!(for MenuView: MenuView::None);

impl MenuView {
    pub fn get_mode(&self) -> MenuMode {
        match self {
            Self::None => MenuMode::Hidden,
            Self::FileSwitcher(_) => MenuMode::FileSwitcher,
            Self::FindReplace(v) => MenuMode::FindReplace(v.mode),
            Self::GoToPosition(_) => MenuMode::GoToPosition,
        }
    }
}

pub type FileSwitcherResults = Vec<PathBuf>;

#[derive(Default, Debug)]
pub struct FileSwitcherView {
    pub search: BufferViewData,
    pub results: FileSwitcherResults,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FindReplaceMode {
    CurrentFile,
}
d!(for FindReplaceMode: FindReplaceMode::CurrentFile);

#[derive(Default, Debug)]
pub struct FindReplaceView {
    pub mode: FindReplaceMode,
    pub find: BufferViewData,
    pub replace: BufferViewData,
}

#[derive(Default, Debug)]
pub struct GoToPositionView {
    pub go_to_position: BufferViewData,
}

pub type VisibleBuffer = Option<g_i::Index>;

#[derive(Default, Debug)]
pub struct View {
    pub current_buffer_id: BufferId,
    pub index_state: g_i::State,
    pub visible_buffer: VisibleBuffer,
    pub edited_buffer_index: Option<g_i::Index>,
    pub buffers: Vec<BufferView>,
    pub menu: MenuView,
    pub status_line: StatusLineView,
}

impl View {
    pub fn get_visible_index_or_max(&self) -> usize {
        self.visible_buffer
            .and_then(|index| index.get(self.index_state))
            .unwrap_or(usize::max_value())
    }

    pub fn get_visible_index_and_buffer(&self) -> Option<(g_i::Index, &BufferView)> {
        self.visible_buffer.and_then(|index| {
            index
                .get(self.index_state)
                .and_then(|i| self.buffers.get(i).map(|b| (index, b)))
        })
    }

    pub fn get_current_buffer_view_data(&self) -> Option<&BufferViewData> {
        use BufferIdKind::*;
        match self.current_buffer_id.kind {
            None => Option::None,
            Text => {
                for (index, buffer) in self.buffers.iter().enumerate() {
                    // This seems like it should be correct given that the `current_buffer_id` is
                    // of the current generation
                    let i: usize = self.current_buffer_id.index.into();
                    if index == i {
                        return Some(&buffer.data);
                    }
                }
                Option::None
            }
            Find => match &self.menu {
                MenuView::FindReplace(ref fr) => Some(&fr.find),
                _ => Option::None,
            },
            Replace => match &self.menu {
                MenuView::FindReplace(ref fr) => Some(&fr.replace),
                _ => Option::None,
            },
            FileSwitcher => match &self.menu {
                MenuView::FileSwitcher(ref fs) => Some(&fs.search),
                _ => Option::None,
            },
            GoToPosition => match &self.menu {
                MenuView::GoToPosition(ref gtp) => Some(&gtp.go_to_position),
                _ => Option::None,
            },
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct BufferView {
    pub name: BufferName,
    // TODO this could be truncated to a fixed length/on the stack
    pub name_string: String,
    pub data: BufferViewData,
}

/// We might change this later, but it will always be an integer of some sort.
type SpanKindRaw = u8;

/// We want to allow different kinds of span classifiers to have 
/// different sets of span kinds, and to be able to invent new ones
/// without needing to list them all here. Additionally we want 
/// deciding what to do when presented with values of this type to 
/// be up to individual clients of the `editor` crate, while also 
/// allowing at least some form of backward compatibility. For 
/// example, a client should be allowed to conflate different
/// SpanKinds up to and including ones that were not known at the 
/// time that client was written. All that leads us to allowing all
/// values of the structs size as possible values, rather than an
/// enum where only certain values are allowed.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SpanKind(SpanKindRaw);

#[macro_export]
macro_rules! sk {
    (PLAIN) => {
        sk!(0)
    };
    (COMMENT) => {
        sk!(1)
    };
    (STRING) => {
        sk!(2)
    };
    // When adding a new one of these, increment 
    // the value below for each new one.
    (FIRST_UNASSIGNED_RAW) => {
        3
    };
    ($kind_val: expr) => {
        SpanKind::new($kind_val)
    }
}

impl SpanKind {
    pub const fn new(byte: u8) -> Self { 
        SpanKind(byte)
    }

    /// The justification for using all values of a given size
    /// notwithstanding, it is still useful to have clear 
    /// conventions, (which can be ignored as necessary,)
    /// hence these constants.
    pub const PLAIN: SpanKind = sk!(PLAIN);
    pub const COMMENT: SpanKind = sk!(COMMENT);
    pub const STRING: SpanKind = sk!(STRING);

    /// Given we have conventions, we want to be able to 
    /// conform with them, but also allow new conventions
    /// to be created. This value represents the smallest 
    /// value that does not have a conventional meaning.
    /// all the valued of a SpanKindRaw will not have a
    /// conventional meaning, so different span 
    /// classifiers can assign those values whatever 
    /// meaning they wish.
    pub const FIRST_UNASSIGNED_RAW: SpanKindRaw = sk!(FIRST_UNASSIGNED_RAW);

    pub fn get_byte(&self) -> u8 {
        self.0
    }
}
d!(for SpanKind: SpanKind::PLAIN);

#[derive(Copy, Clone, Default, Debug, PartialEq)]
pub struct SpanView {
    pub end_byte_index: usize,
    pub kind: SpanKind,
}

#[derive(Clone, Default, Debug)]
pub struct BufferViewData {
    //TODO make this a &str or a char iterator
    pub chars: String,
    pub scroll: ScrollXY,
    pub cursors: Vec<CursorView>,
    pub highlights: Vec<Highlight>,
    pub spans: Vec<SpanView>
}

/// Short form of "Command".
/// This is for telling the platform layer that it should do something in addition to
/// rendering the view.
#[derive(Debug, Clone)]
pub enum Cmd {
    NoCmd,
    SetClipboard(String),
    LoadFile(PathBuf),
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
pub mod tests;
