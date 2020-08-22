#![deny(unused)]
use macros::{
    d, fmt_debug, fmt_display, ord, u,
};
use std::{
    time::Duration,
    path::PathBuf
};

pub use vec1::{vec1, Vec1};
pub use panic_safe_rope::Rope;
pub use text_pos::*;

pub mod floating_point;

pub mod screen_positioning;
pub use screen_positioning::{
    CharDim,
    ScreenSpaceRect,
    ScreenSpaceWH,
    ScreenSpaceXY,
    ScrollXY,
    SizeDependents,
    TextBoxXY,
    TextBoxSpaceXY,
    TextSpaceXY,
    TextSpaceXYWH,
    ssr,
    sswh,
    ssxy,
};

pub use abs;
pub use f32_0_1::{F32_0_1, f32_0_1};
pub use pos_f32::{PosF32, pos_f32};
pub use pos_f32_trunc::{PosF32Trunc, pos_f32_trunc};
pub use non_neg_f32::{NonNegF32, non_neg_f32};

pub use move_mod::Move;

pub use g_i;
pub use g_i::{SelectionAdjustment, SelectionMove, SelectableVec1};

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
    SetSizeDependents(Box<SizeDependents>),
    MoveAllCursors(Move),
    ExtendSelectionForAllCursors(Move),
    SelectAll,
    SetCursor(TextBoxSpaceXY, ReplaceOrAdd),
    DragCursors(TextBoxSpaceXY),
    SelectCharTypeGrouping(TextBoxSpaceXY, ReplaceOrAdd),
    ExtendSelectionWithSearch,
    SavedAs(g_i::Index, PathBuf),
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


#[derive(Clone, Copy, Default, Debug, Hash, PartialEq, Eq)]
pub struct BufferId {
    pub kind: BufferIdKind,
    pub index: g_i::Index,
}
ord!(for BufferId: id, other in {
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum BufferIdKind {
    /// Used to indicate that the keyboard is focused on a non-buffer.
    None,
    /// Indicates a buffer repesenting an open file or an in memory scratch file.
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

ord!(for BufferIdKind: kind, other in {
    let k: u8 = kind.into();
    let o: u8 = other.into();
    k.cmp(&o)
});


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HighlightKind {
    User,
    Result,
    CurrentResult,
}
d!(for HighlightKind: HighlightKind::User);

#[derive(Clone, Debug, PartialEq, Eq)]
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
                // [|_______________________|]
                //  ^min_middle   max_middle^
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
                            offset: CharOffset::max_value(),
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
                            offset: CharOffset::max_value(),
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
                            offset: CharOffset::max_value(),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
ord!(for BufferName: name, other in {
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CursorState {
    None,
    PressedAgainstWall(Move),
}
d!(for CursorState: CursorState::None);

fmt_debug!(for CursorState: s in "{}", match s {
    CursorState::None => std::borrow::Cow::Borrowed("_"),
    CursorState::PressedAgainstWall(r#move) => std::borrow::Cow::Owned(format!("->|({})", r#move))
});

ord!(for CursorState: state, other in {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CursorView {
    pub position: Position,
    pub state: CursorState,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StatusLineView {
    pub chars: String,
}

pub const DEFAULT_STATUS_LINE_CHARS: &str = "No buffer selected.";
d!(for StatusLineView: StatusLineView {chars: DEFAULT_STATUS_LINE_CHARS.to_owned()});

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MenuMode {
    Hidden,
    FileSwitcher,
    FindReplace(FindReplaceMode),
    GoToPosition,
}
d!(for MenuMode: MenuMode::Hidden);

#[derive(Clone, Debug, PartialEq)]
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

pub fn kind_editable_during_mode(kind: BufferIdKind, menu_mode: MenuMode) -> bool {
    u!{MenuMode}
    match (kind, menu_mode) {
        (BufferIdKind::None, _) => false,
        // We want this to be true always since it would be completely reasonable 
        // behaviour for a different client to always show the text buffers.
        (BufferIdKind::Text, _) => true, 
        (BufferIdKind::Find, FindReplace(_)) => {
            true
        },
        (BufferIdKind::Replace, FindReplace(_)) => {
            true
        },
        (BufferIdKind::FileSwitcher, MenuMode::FileSwitcher) => {
            true
        },
        (BufferIdKind::GoToPosition, MenuMode::GoToPosition) => {
            true
        },
        _ => {
            false
        },
    }
}

pub type FileSwitcherResults = Vec<PathBuf>;

#[derive(Clone, Default, Debug, PartialEq)]
pub struct FileSwitcherView {
    pub search: BufferViewData,
    pub results: FileSwitcherResults,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FindReplaceMode {
    CurrentFile,
}
d!(for FindReplaceMode: FindReplaceMode::CurrentFile);

#[derive(Clone, Default, Debug, PartialEq)]
pub struct FindReplaceView {
    pub mode: FindReplaceMode,
    pub find: BufferViewData,
    pub replace: BufferViewData,
    pub result_count: usize,
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct GoToPositionView {
    pub go_to_position: BufferViewData,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum EditedTransition {
    ToEdited,
    ToUnedited,
}

pub type IndexedEditedTransition = (g_i::Index, EditedTransition);

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct EditedTransitions(Vec<IndexedEditedTransition>);

impl EditedTransitions {
    pub fn push(&mut self, iet: IndexedEditedTransition) {
        self.0.push(iet);
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &IndexedEditedTransition> {
        self.0.iter()
    }
}

impl IntoIterator for EditedTransitions {
    type Item = IndexedEditedTransition;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct View {    pub buffers: SelectableVec1<BufferView>,
    pub menu: MenuView,
    pub status_line: StatusLineView,
    pub current_buffer_kind: BufferIdKind,
    pub edited_transitions: EditedTransitions,
    pub stats: ViewStats,
}

impl View {
    /// returns the currently visible editor buffer index.
    pub fn current_text_index(&self) -> g_i::Index {
        self.buffers.current_index()
    }

    /// returns the currently visible editor buffer view and its index.
    pub fn current_text_index_and_buffer(&self) -> (g_i::Index, &BufferView) {
        (
            self.buffers.current_index(),
            self.buffers.get_current_element()
        )
    }

    pub fn get_buffer(&self, index: g_i::Index) -> Option<&BufferView> {
        self.buffers.get(index)
    }

    pub fn current_buffer_id(&self) -> BufferId {
        b_id!(
            self.current_buffer_kind,
            self.buffers.current_index()
        )
    }

    /// returns the selected menu buffer view data if there is a menu containing a buffer
    /// currently visible, or the current text buffer view data if not.
    pub fn get_selected_buffer_view_data(&self) -> Option<&BufferViewData> {
        use BufferIdKind::*;
        match self.current_buffer_kind {
            None => Option::None,
            Text => {
                Some(&self.buffers.get_current_element().data)            }
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

    /// returns the currently visible editor buffer path if it has one.
    pub fn current_path(&self) -> Option<PathBuf> {
        u!{BufferName}
        match self.buffers.get_current_element().name {
            Path(ref p) => Some(p.to_owned()),
            Scratch(_) => None,
        }
    }
}

#[derive(Clone, Default, PartialEq)]
pub struct BufferView {
    pub name: BufferName,
    // TODO this could be truncated to a fixed length/on the stack
    pub name_string: String,
    pub data: BufferViewData,
}

fmt_debug!(collapse default for BufferView: me {
    blank_if_default!(name);
    blank_if_default!(name_string, me.name_string.is_empty());
    blank_if_default!(data);
});

/// We might change this later, but it will always be an integer of some sort.
pub type SpanKindRaw = u8;

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
    /// all the values of a SpanKindRaw will not have a
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

#[derive(Clone, Default, PartialEq)]
pub struct BufferViewData {
    pub chars: Rope,
    pub scroll: ScrollXY,
    pub cursors: Vec<CursorView>,
    pub highlights: Vec<Highlight>,
    pub spans: Vec<SpanView>
}

fmt_debug!(collapse default for BufferViewData: me {
    blank_if_default!(chars, me.chars == Rope::default());
    blank_if_default!(scroll);
    blank_if_default!(cursors, me.cursors.is_empty());
    blank_if_default!(highlights, me.highlights.is_empty());
    blank_if_default!(spans, me.spans.is_empty());
});

#[macro_export]
macro_rules! bvd {
    ($chars: expr) => {{
        let mut data: BufferViewData = d!();
        data.chars = $chars.into();
        data
    }}
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

pub type UpdateAndRenderOutput = (View, Cmd);
pub type UpdateAndRender = fn(Input) -> UpdateAndRenderOutput;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ViewStats {
    pub latest_render_duration: Duration
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;
