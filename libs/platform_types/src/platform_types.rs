use macros::{
    add_assign, d, fmt_debug, fmt_display, integer_newtype, ord, sub_assign, usize_newtype,
};
use std::path::PathBuf;

pub mod floating_point;

pub mod screen_positioning;
pub use screen_positioning::*;

mod move_mod {
    use super::*;
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
        ToPreviousLikelyEditLocation,
        ToNextLikelyEditLocation,
    }
    use Move::*;

    fmt_display!(for Move: r#move in "{}", match r#move {
        Up => ">",
        Down => "v",
        Left => "<",
        Right => ">",
        ToLineStart => "Line<",
        ToLineEnd => "Line>",
        ToBufferStart => "Buffer<",
        ToBufferEnd => "Buffer>",
        ToPreviousLikelyEditLocation => "Edit<",
        ToNextLikelyEditLocation => "Edit>",
    });

    macro_rules! to_num {
        ($m: expr) => {
            match $m {
                Up => 0,
                Down => 1,
                Left => 2,
                Right => 3,
                ToLineStart => 4,
                ToLineEnd => 5,
                ToBufferStart => 6,
                ToBufferEnd => 7,
                ToPreviousLikelyEditLocation => 8,
                ToNextLikelyEditLocation => 9,
            }
        };
    }

    ord!(and friends for Move: r#move, other in to_num!(r#move).cmp(&to_num!(other)));

    impl std::ops::Not for Move {
        type Output = Move;

        fn not(self) -> Self::Output {
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
}
pub use move_mod::Move;

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
    ExtendSelectionWithSearch,
    SetBufferPath(g_i::Index, PathBuf),
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
    OpenOrSelectBuffer(PathBuf),
    CloseBuffer(g_i::Index),
    SetMenuMode(MenuMode),
    SetFindReplaceMode(FindReplaceMode),
    SubmitForm,
}
d!(for Input : Input::None);

/// A module containg a Generational Index implementation
pub mod g_i {
    use macros::{d, fmt_debug, fmt_display, ord};
    pub type Generation = u32;
    type LengthSize = u32;

    // The amount of elements in the collection using generational indexes. Not a valid index.
    #[derive(Clone, Copy, Default)]
    pub struct Length(LengthSize);
    fmt_debug!(for Length: Length(l) in "{}", l);
    fmt_display!(for Length: Length(l) in "{}", l);
    ord!(and friends for Length: length, other in {
        length.0.cmp(&other.0)
    });

    impl Length {
        /// This returns a `usize` to make comparing to usize lengths conveinient.
        pub const fn max_value() -> usize {
            LengthSize::max_value() as usize
        }

        /// This takes a `usize` to make creation from usize lengths, where we don't care about
        /// the maximum case, conveinient.
        pub fn or_max(len: usize) -> Self {
            Self({
                let max = Self::max_value();
                if len > max {
                    max as LengthSize
                } else {
                    len as LengthSize
                }
            })
        }
    }

    impl From<Length> for usize {
        fn from(length: Length) -> Self {
            length.0 as usize
        }
    }

    /// The part of `Index` which does not have to do with generations. That is, the part which
    /// denotes which element in the collection is desired, in the usual 0-based way.
    #[derive(Clone, Copy, Default)]
    pub struct IndexPart(LengthSize);
    fmt_debug!(for IndexPart: IndexPart(l) in "{}", l);
    fmt_display!(for IndexPart: IndexPart(l) in "{}", l);
    ord!(and friends for IndexPart: index_part, other in {
        index_part.0.cmp(&other.0)
    });

    impl IndexPart {
        /// This returns a `usize` to make comparing to usize lengths conveinient.
        pub const fn max_value() -> usize {
            (LengthSize::max_value() - 1) as usize
        }

        /// This takes a `usize` to make creation from usize lengths, where we don't care about
        /// the maximum case, conveinient.
        pub fn or_max(i: usize) -> Self {
            Self({
                let max = Self::max_value();
                if i > max {
                    max as LengthSize
                } else {
                    i as LengthSize
                }
            })
        }
    }

    impl macros::SaturatingAdd<usize> for IndexPart {
        type Output = Self;

        fn saturating_add(self, rhs: usize) -> Self::Output {
            let sum = (self.0 as usize).saturating_add(rhs);

            Self::or_max(sum)
        }
    }

    impl macros::SaturatingSub<usize> for IndexPart {
        type Output = Self;

        fn saturating_sub(self, rhs: usize) -> Self::Output {
            // assumes `LengthSize` is an unsigned type.
            Self((self.0 as usize).saturating_sub(rhs) as LengthSize)
        }
    }

    impl From<IndexPart> for usize {
        fn from(part: IndexPart) -> Self {
            part.0 as usize
        }
    }

    impl From<IndexPart> for Length {
        fn from(part: IndexPart) -> Self {
            Length(part.0)
        }
    }

    impl std::ops::Rem<Length> for IndexPart {
        type Output = Self;

        // I guess this operation should be doing generation checking, which would imply that
        // `Length` should store the generation, and more importantly, this operation could fail.
        // Let's see if that actually becomes a problem though I guess? If it does we could avoid
        // that by making this a function that takes the container so it is known that the `Length`
        // is the correct genetration.
        fn rem(self, modulus: Length) -> Self::Output {
            Self(self.0 % modulus.0)
        }
    }

    impl std::ops::RemAssign<Length> for IndexPart {
        fn rem_assign(&mut self, modulus: Length) {
            *self = *self % modulus;
        }
    }

    // It could be argued that this should do generation checking, but it could also be agued that
    // you should be allowed to compare to old lengths assuming you know what yoa are doing. We'll
    // see if it ecomes an issue I guess.
    impl std::cmp::PartialOrd<Length> for IndexPart {
        fn partial_cmp(&self, other: &Length) -> Option<std::cmp::Ordering> {
            Some(self.0.cmp(&other.0))
        }
    }

    impl std::cmp::PartialEq<Length> for IndexPart {
        fn eq(&self, other: &Length) -> bool {
            self.partial_cmp(other)
                .map(|o| o == std::cmp::Ordering::Equal)
                .unwrap_or(false)
        }
    }

    /// The type of invalidation that caused the index to need another generation. We keep track
    /// of this so that we can auto-fix the indexes from one generation ago, when possible.
    /// `RemovedAt(d!())` is a reasonable default because it results is a fixup of no change at all
    /// which is correct for the first instance.
    #[derive(Clone, Copy, Debug)]
    enum Invalidation {
        RemovedAt(IndexPart),
    }

    d!(for Invalidation: Invalidation::RemovedAt(d!()));

    #[derive(Clone, Copy, Default, Debug)]
    pub struct State {
        current: Generation,
        invalidation: Invalidation,
    }

    impl State {
        fn advance(&mut self, invalidation: Invalidation) {
            *self = State {
                current: self.current.wrapping_add(1),
                invalidation,
            };
        }
        pub fn removed_at(&mut self, index: Index) {
            self.advance(Invalidation::RemovedAt(index.index));
        }

        /// Attempt to convert an index from a given gerneation to the current generation.
        pub fn migrate(self, index: Index) -> Option<Index> {
            index.get_index_part(self).map(|i| self.new_index(i))
        }
        pub fn new_index(&self, index: IndexPart) -> Index {
            Index {
                generation: self.current,
                index,
            }
        }
    }

    #[derive(Clone, Copy, Default, Debug)]
    /// A generational index
    pub struct Index {
        generation: Generation,
        /// 4 billion what-zits ought to be enough for anybody!
        index: IndexPart,
    }
    ord!(and friends for Index: index, other in {
        index.generation.cmp(&other.generation).then_with(|| index.index.cmp(&other.index))
    });

    impl std::cmp::PartialOrd<IndexPart> for Index {
        fn partial_cmp(&self, other: &IndexPart) -> Option<std::cmp::Ordering> {
            Some(self.index.cmp(&other))
        }
    }

    impl std::cmp::PartialEq<IndexPart> for Index {
        fn eq(&self, other: &IndexPart) -> bool {
            self.partial_cmp(other)
                .map(|o| o == std::cmp::Ordering::Equal)
                .unwrap_or(false)
        }
    }

    impl Index {
        pub fn get(self, state: State) -> Option<usize> {
            self.get_index_part(state).map(|IndexPart(i)| i as usize)
        }
        fn get_index_part(self, state: State) -> Option<IndexPart> {
            if self.generation == state.current {
                Some(self.index)
            } else if self.generation == state.current.wrapping_sub(1) {
                match state.invalidation {
                    Invalidation::RemovedAt(i) => {
                        use std::cmp::Ordering::*;
                        // Imagine the vec looks like this:
                        // `vec![10, 11, 12, 13, 14]`.
                        // and that we called `v.remove(2)` so now it looks like:
                        // `vec![10, 11, 13, 14]`.
                        // If you wanted `12` you can't have it, but if you wanted `10` or `11`
                        // your index is valid as is. Finally, if you wanted `13` or `14` then your
                        // index needs to be shifted down by one.
                        match self.index.cmp(&i) {
                            Equal => None,
                            Less => Some(self.index),
                            Greater => (self.index.0).checked_sub(1).map(IndexPart),
                        }
                    }
                }
            } else {
                // insert your own joke about people > 40 years older than yourself here.
                None
            }
        }
    }

    impl macros::SaturatingAdd<usize> for Index {
        type Output = Self;

        fn saturating_add(mut self, rhs: usize) -> Self::Output {
            self.index = self.index.saturating_add(rhs);

            self
        }
    }

    impl macros::SaturatingSub<usize> for Index {
        type Output = Self;

        fn saturating_sub(mut self, rhs: usize) -> Self::Output {
            self.index = self.index.saturating_sub(rhs);

            self
        }
    }

    impl From<Index> for usize {
        fn from(index: Index) -> Self {
            usize::from(index.index)
        }
    }

    impl From<Index> for Length {
        fn from(index: Index) -> Self {
            Length::from(index.index)
        }
    }

    impl std::ops::Rem<Length> for Index {
        type Output = Self;

        fn rem(mut self, modulus: Length) -> Self::Output {
            self.index %= modulus;
            self
        }
    }

    impl std::ops::RemAssign<Length> for Index {
        fn rem_assign(&mut self, modulus: Length) {
            *self = *self % modulus;
        }
    }

    impl std::cmp::PartialOrd<Length> for Index {
        fn partial_cmp(&self, other: &Length) -> Option<std::cmp::Ordering> {
            Some(self.index.0.cmp(&other.0))
        }
    }

    impl std::cmp::PartialEq<Length> for Index {
        fn eq(&self, other: &Length) -> bool {
            self.partial_cmp(other)
                .map(|o| o == std::cmp::Ordering::Equal)
                .unwrap_or(false)
        }
    }
}

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
    /// Used to indicate that the kayboard is focussed on a non-buffer.
    None,
    /// Indicates A buffer repesenting an open file or an imemory scratch file.
    /// Almost all buffers are `Text` buffers.
    Text,
    Find,
    Replace,
    FileSwitcher,
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
        }
    }
}

ord!(and friends for BufferIdKind: kind, other in {
    let k: u8 = kind.into();
    let o: u8 = other.into();
    k.cmp(&o)
});

impl BufferId {
    pub fn is_text(&self) -> bool {
        match self.kind {
            BufferIdKind::Text => true,
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
pub enum MenuMode {
    Hidden,
    FileSwitcher,
    FindReplace,
}
d!(for MenuMode: MenuMode::Hidden);

#[derive(Debug)]
pub enum MenuView {
    None,
    FileSwitcher(FileSwitcherView),
    FindReplace(FindReplaceView),
}
d!(for MenuView: MenuView::None);

impl MenuView {
    pub fn get_mode(&self) -> MenuMode {
        match self {
            Self::None => MenuMode::Hidden,
            Self::FileSwitcher(_) => MenuMode::FileSwitcher,
            Self::FindReplace(_) => MenuMode::FindReplace,
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

pub type VisibleBuffer = Option<g_i::Index>;

#[derive(Default, Debug)]
pub struct View {
    pub current_buffer_id: BufferId,
    pub index_state: g_i::State,
    pub visible_buffer: VisibleBuffer,
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
}

#[derive(Default, Debug)]
pub struct BufferView {
    pub name: BufferName,
    // TODO this could be truncated to a fixed length/on the stack
    pub name_string: String,
    pub data: BufferViewData,
}

#[derive(Clone, Copy, Debug)]
pub enum Navigation {
    None,
    Up,
    Down,
}
d!(for Navigation: Navigation::None);

#[derive(Default, Debug)]
pub struct BufferViewData {
    //TODO make this a &str or a char iterator
    pub chars: String,
    pub scroll: ScrollXY,
    pub cursors: Vec<CursorView>,
    pub highlights: Vec<Highlight>,
    pub navigation: Navigation,
}

// Short form "Command".
// This is for telling the platform layer that it should do something in addition to
// rendering the view.
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
mod tests;
