use macros::{
    add_assign, d, fmt_debug, fmt_display, integer_newtype, ord, sub_assign, usize_newtype,
};
use std::path::PathBuf;

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
    Insert(char),
    Delete,
    ResetScroll,
    ScrollVertically(f32),
    ScrollHorizontally(f32),
    ScrollTabs(f32),
    SetSizes(Sizes),
    MoveAllCursors(Move),
    ExtendSelectionForAllCursors(Move),
    SelectAll,
    SetCursor(ScreenSpaceXY, ReplaceOrAdd),
    DragCursors(ScreenSpaceXY),
    SelectCharTypeGrouping(ScreenSpaceXY, ReplaceOrAdd),
    Undo,
    Redo,
    Cut,
    Copy,
    Paste(Option<String>),
    InsertNumbersAtCursors,
    LoadedFile(PathBuf, String),
    TabIn,
    TabOut,
    NextBuffer,
    PreviousBuffer,
}

d!(for Input : Input::None);

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// The top left corner of the text is `(0.0, 0.0)1, top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down.
pub struct TextSpaceXY {
    pub x: f32,
    pub y: f32,
}

fmt_display!(for TextSpaceXY: TextSpaceXY {x, y} in "{:?}", (x, y));

impl From<TextSpaceXY> for (f32, f32) {
    fn from(TextSpaceXY { x, y }: TextSpaceXY) -> Self {
        (x, y)
    }
}

impl std::ops::Add for TextSpaceXY {
    type Output = TextSpaceXY;

    fn add(self, other: TextSpaceXY) -> TextSpaceXY {
        TextSpaceXY {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// The top left corner of the text is `(0.0, 0.0)1, top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down.
pub struct ScrollXY {
    pub x: f32,
    pub y: f32,
}

fmt_display!(for ScrollXY: ScrollXY {x, y} in "{:?}", (x, y));

impl From<ScrollXY> for (f32, f32) {
    fn from(ScrollXY { x, y }: ScrollXY) -> Self {
        (x, y)
    }
}

impl std::ops::Add for ScrollXY {
    type Output = ScrollXY;

    fn add(self, other: ScrollXY) -> ScrollXY {
        ScrollXY {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl std::ops::Add<ScrollXY> for ScreenSpaceXY {
    type Output = TextSpaceXY;

    fn add(self, other: ScrollXY) -> TextSpaceXY {
        TextSpaceXY {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl std::ops::Add<ScreenSpaceXY> for ScrollXY {
    type Output = TextSpaceXY;

    fn add(self, other: ScreenSpaceXY) -> TextSpaceXY {
        TextSpaceXY {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

pub fn screen_to_text(xy: ScreenSpaceXY, scroll: ScrollXY) -> TextSpaceXY {
    scroll + xy
}

impl std::ops::Sub<ScrollXY> for TextSpaceXY {
    type Output = ScreenSpaceXY;

    fn sub(self, other: ScrollXY) -> ScreenSpaceXY {
        ScreenSpaceXY {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

pub fn text_to_screen(xy: TextSpaceXY, scroll: ScrollXY) -> ScreenSpaceXY {
    xy - scroll
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// The top left corner of the screen is `(0.0, 0.0)1, top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down.
pub struct ScreenSpaceXY {
    pub x: f32,
    pub y: f32,
}

fmt_display!(for ScreenSpaceXY: ScreenSpaceXY {x, y} in "{:?}", (x, y));

impl From<ScreenSpaceXY> for (f32, f32) {
    fn from(ScreenSpaceXY { x, y }: ScreenSpaceXY) -> Self {
        (x, y)
    }
}

impl std::ops::Add for ScreenSpaceXY {
    type Output = ScreenSpaceXY;

    fn add(self, other: ScreenSpaceXY) -> ScreenSpaceXY {
        ScreenSpaceXY {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct ScreenSpaceWH {
    pub w: f32,
    pub h: f32,
}

fmt_display!(for ScreenSpaceWH: ScreenSpaceWH {w, h} in "{:?}", (w, h));

impl From<ScreenSpaceWH> for (f32, f32) {
    fn from(ScreenSpaceWH { w, h }: ScreenSpaceWH) -> Self {
        (w, h)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// It's nice for it to be harder to mixup screen dimensions and Character dimension.
// Plus since `CharDim` came before `ScreenSpaceWH` less code has to change if we keep `CharDim`
/// We are currently assuming the font is monospace!
pub struct CharDim {
    pub w: f32,
    pub h: f32,
}

fmt_display!(for CharDim: CharDim {w, h} in "{:?}", (w, h));

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
    if x.is_normal() {
        x
    } else {
        0.0
    }
}

pub fn screen_space_to_position(
    xy: ScreenSpaceXY,
    char_dim: CharDim,
    scroll: ScrollXY,
    round: PositionRound,
) -> Position {
    text_space_to_position(screen_to_text(xy, scroll), char_dim, round)
}

pub fn text_space_to_position(
    TextSpaceXY { x, y }: TextSpaceXY,
    CharDim { w, h }: CharDim,
    round: PositionRound,
) -> Position {
    // This is made much more conveinient by the monospace assumption!
    let pre_rounded = x / w;

    // if the value would not fit in a `usize` then the `as usize` is undefined behaviour.
    // https://github.com/rust-lang/rust/issues/10184
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=21e5f8c502c8e6e16a685449ccc9db82
    let offset = normal_or_zero(match round {
        PositionRound::TowardsZero => pre_rounded,
        PositionRound::Up => {
            // The right half of a character should correspond to the position to the
            // right of the character.
            pre_rounded + 0.5
        }
    }) as usize;
    let line = normal_or_zero(y / h) as usize;

    Position {
        offset: CharOffset(offset),
        line,
    }
}

pub fn position_to_screen_space(
    pos: Position,
    char_dim: CharDim,
    scroll: ScrollXY,
) -> ScreenSpaceXY {
    text_to_screen(position_to_text_space(pos, char_dim), scroll)
}

pub fn position_to_text_space(
    Position { offset, line }: Position,
    CharDim { w, h }: CharDim,
) -> TextSpaceXY {
    // This is made much more conveinient by the monospace assumption!

    // Weird *graphical-only* stuff given a >2^24 long line and/or >2^24
    // lines seems better than an error box or something like that.
    #[allow(clippy::cast_precision_loss)]
    TextSpaceXY {
        x: offset.0 as f32 * w,
        y: line as f32 * h,
    }
}

/// This represents the visible portion of the screen. This struct primarily exists to make it
/// clear that the negtive y direction is used instead of the positive one. That is, the area
/// where `scroll.x` is between `0.0` and `wh.w` and `scroll.y` is between `-wh.h` and `0.0` is
/// considered to be on the screen. As a side effect, this struct also allows functions that
/// operate on the `scroll` to be harder to use incorrectly, by preventing mixing up what would
/// otherwise be two `ScreenSpaceXY` params.
#[derive(Default, Debug)]
pub struct ScrollableScreen {
    /// A negative `scroll.x` value means move the screen left, so you can see things that are
    /// further right. A negative `scroll.y` value means move the screen up, so you can see things
    /// that are further down.
    pub scroll: ScrollXY,
    pub wh: ScreenSpaceWH,
}

fmt_display!(for ScrollableScreen : ScrollableScreen {scroll, wh}
      in "ScrollableScreen {{ scroll:{}, wh: {} }}", scroll, wh
 );

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum VisibilityAttemptResult {
    Succeeded,
    ScreenTooSmall,
    ScreenTooLarge,
    ScreenTooWeird,
    ApronEdgeTooSmall,
    ApronEdgeTooLarge,
    ApronEdgeTooWeird,
}

pub struct Apron {
    pub left_w: f32,
    pub right_w: f32,
    pub top_h: f32,
    pub bottom_h: f32,
}

impl From<CharDim> for Apron {
    fn from(CharDim { w, h }: CharDim) -> Self {
        Apron {
            left_w: w,
            right_w: w,
            top_h: h,
            bottom_h: h,
        }
    }
}

/// if it is off the screen, scroll so it is inside an at least `char_dim` sized apron inside
/// from the edge of the screen. But if it is inside the apron, then don't bother scrolling.
///
/// +-------------------+
/// | +---------------+ |
/// | |...............| |
/// | +---------------+ |
/// +-------------------+
///
/// The outer box is what we call the "apron".
pub fn attempt_to_make_xy_visible(
    ScrollableScreen { ref mut scroll, wh }: &mut ScrollableScreen,
    apron: Apron,
    text: TextSpaceXY,
) -> VisibilityAttemptResult {
    use std::num::FpCategory::*;
    use VisibilityAttemptResult::*;

    let ScreenSpaceXY { x, y } = text_to_screen(text, *scroll);
    let &mut ScreenSpaceWH { w, h } = wh;

    // If these checks ever actually become a bottleneck ,the nthe easy solution is to just make
    // types that can't represent these cases and enforce them at startup!
    match (w.classify(), h.classify()) {
        (Nan, _) | (_, Nan) => return ScreenTooWeird,
        (Infinite, _) | (_, Infinite) => return ScreenTooLarge,
        (Zero, _) | (_, Zero) | (Subnormal, _) | (_, Subnormal) => return ScreenTooSmall,
        (Normal, Normal) if w < 1.0 || h < 1.0 => return ScreenTooSmall,
        (Normal, Normal) => {}
    }

    match (apron.left_w.classify(), apron.top_h.classify()) {
        (Nan, _) | (_, Nan) => return ApronEdgeTooWeird,
        (Infinite, _) | (_, Infinite) => return ApronEdgeTooLarge,
        (Zero, _) | (_, Zero) | (Subnormal, _) | (_, Subnormal) => return ApronEdgeTooSmall,
        (Normal, Normal) if apron.left_w < 1.0 || apron.top_h < 1.0 => return ApronEdgeTooSmall,
        (Normal, Normal) if apron.left_w > w || apron.top_h > h => return ApronEdgeTooLarge,
        (Normal, Normal) => {}
    }

    match (apron.right_w.classify(), apron.bottom_h.classify()) {
        (Nan, _) | (_, Nan) => return ApronEdgeTooWeird,
        (Infinite, _) | (_, Infinite) => return ApronEdgeTooLarge,
        (Zero, _) | (_, Zero) | (Subnormal, _) | (_, Subnormal) => return ApronEdgeTooSmall,
        (Normal, Normal) if apron.right_w < 1.0 || apron.bottom_h < 1.0 => {
            return ApronEdgeTooSmall
        }
        (Normal, Normal) if apron.right_w > w || apron.bottom_h > h => return ApronEdgeTooLarge,
        (Normal, Normal) => {}
    }

    // We don't ever want to automatically show space that text can never be inside.
    macro_rules! stay_positive {
        ($n: expr) => {{
            let n = $n;
            if n > 0.0 {
                n
            } else {
                0.0
            }
        }};
    }

    if x < apron.left_w {
        scroll.x = stay_positive!(text.x - apron.left_w);
    } else if x >= w - apron.right_w {
        scroll.x = stay_positive!(text.x - w + apron.right_w);
    } else {
        // leave it alone
    }

    if y < apron.top_h {
        scroll.y = stay_positive!(text.y - apron.top_h);
    } else if y >= h - apron.bottom_h {
        scroll.y = stay_positive!(text.y - (h - apron.bottom_h));
    } else {
        // leave it alone
    }

    Succeeded
}

pub fn xy_is_visible(
    ScrollableScreen {
        scroll,
        wh: ScreenSpaceWH { w, h },
    }: &ScrollableScreen,
    text: TextSpaceXY,
) -> bool {
    let ScreenSpaceXY { x, y } = text_to_screen(text, *scroll);
    x >= 0.0 && x < *w && y >= 0.0 && y < *h
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

#[derive(Default, Debug)]
pub struct View {
    pub buffers: Vec<BufferView>,
}

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
    // The position of the buffer view's origin (upper-left) point.
    pub screen_position: ScreenSpaceXY,
    pub bounds: (f32, f32),
    pub color: [f32; 4],
    //TODO make this a &str or a char iterator
    pub chars: String,
    pub highlights: Vec<Highlight>,
}

#[derive(Copy, Clone, Debug)]
pub enum BufferViewKind {
    Edit,
    StatusLine,
    Cursor,
    Tab,
}

d!(for BufferViewKind: BufferViewKind::Cursor);

// Short form "Command".
// This is for telling the platform layer that it should do things something in addition to
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

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct FontInfo {
    pub text_char_dim: CharDim,
    pub status_char_dim: CharDim,
    pub tab_char_dim: CharDim,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Sizes {
    pub screen: Option<ScreenSpaceWH>,
    pub font_info: Option<FontInfo>,
}

#[macro_export]
macro_rules! Sizes {
    {
        screen: $screen:expr,
        font_info: $font_info:expr$(,)?
    } => (
        Sizes {
            screen: $screen.into(),
            font_info: $font_info.into(),
        }
    );
}

#[cfg(test)]
mod tests;
