use macros::{fmt_debug, fmt_display, dbg, u};
use text_pos::{CharOffset, Position};
pub use non_neg_f32::{NonNegF32, non_neg_f32};
pub use pos_f32::{PosF32, pos_f32};
pub use pos_f32_trunc::{PosF32Trunc, pos_f32_trunc};
pub use f32_0_1::{F32_0_1, f32_0_1};
pub use abs::{abs_pos, abs_length};
pub use screen_space::*;

// TODO should this be moved into screen_space?
impl MapElements<abs::Pos> for ScreenSpaceXY {
    fn map_elements(&self, mapper: &impl Fn(abs::Pos) -> abs::Pos) -> Self {
        Self { 
            x: mapper(self.x),
            y: mapper(self.y),
        }
    }
}

impl MapElements<abs::Pos> for ScreenSpaceWH {
    fn map_elements(&self, mapper: &impl Fn(abs::Pos) -> abs::Pos) -> Self {
        Self {
            w: abs_length!(mapper(self.w.into())),
            h: abs_length!(mapper(self.h.into())),
        }
    }
}

impl MapElements<abs::Length> for ScreenSpaceWH {
    fn map_elements(&self, mapper: &impl Fn(abs::Length) -> abs::Length) -> Self {
        Self {
            w: mapper(self.w),
            h: mapper(self.h),
        }
    }
}

#[derive(Clone, Copy, Default, PartialEq, Hash)]
/// A postion in screen space which represents the top left corner of a text box
/// Not to be confused with a `TextBoxSpaceXY`.
pub struct TextBoxXY {
    pub x: abs::Pos,
    pub y: abs::Pos,
}

fmt_debug!(for TextBoxXY: TextBoxXY {x, y} in "tbxy!({}, {})", x, y);
fmt_display!(for TextBoxXY: TextBoxXY {x, y} in "({},{})", x, y);

#[macro_export]
macro_rules! tbxy {
    //
    // Initialization
    //
    ($x: expr, $y: expr $(,)?) => {
        TextBoxXY { x: $x.into(), y: $y.into() }
    };
    () => {
        TextBoxXY::default()
    };
    //
    // Pattern matching
    //
    ($x: ident $(,)? $y: ident $(,)?) => {
        TextBoxXY { x: $x, y: $y }
    };
}

impl From<TextBoxXY> for (f32, f32) {
    fn from(TextBoxXY { x, y }: TextBoxXY) -> Self {
        (x.into(), y.into())
    }
}

/// All `TextBoxXY` are screen space positions but the reverse is not true.
impl From<TextBoxXY> for ScreenSpaceXY {
    fn from(TextBoxXY { x, y }: TextBoxXY) -> Self {
        ScreenSpaceXY { x, y }
    }
}

impl MapElements<abs::Pos> for TextBoxXY {
    fn map_elements(&self, mapper: &impl Fn(abs::Pos) -> abs::Pos) -> Self {
        Self { 
            x: mapper(self.x),
            y: mapper(self.y),
        }
    }
}

#[derive(Clone, Copy, Default, PartialEq)]
/// A vector in the space with the origin at the top left corner of a given text box.
/// The top left corner of the text box is `(0.0, 0.0), top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down. Note that this is different than `TextSpaceXY` since the text can be scrolled.
pub struct TextBoxSpaceXY {
    pub x: abs::Pos,
    pub y: abs::Pos,
}

fmt_debug!(for TextBoxSpaceXY: TextBoxSpaceXY {x, y} in "tbsxy!({}, {})", x, y);
fmt_display!(for TextBoxSpaceXY: TextBoxSpaceXY {x, y} in "{:?}", (x, y));

#[macro_export]
macro_rules! tbsxy {
    //
    // Initialization
    //
    ($x: expr, $y: expr) => {
        $crate::TextBoxSpaceXY { x: $x.into(), y: $y.into() }
    };
    () => {
        $crate::TextBoxSpaceXY::default()
    };
    //
    // Pattern matching
    //
    ($x: ident, $y: ident) => {
        $crate::TextBoxSpaceXY { x: $x, y: $y }
    };
}

impl From<TextBoxSpaceXY> for (f32, f32) {
    fn from(TextBoxSpaceXY { x, y }: TextBoxSpaceXY) -> Self {
        (x.into(), y.into())
    }
}

impl std::ops::Add<TextBoxXY> for TextBoxSpaceXY {
    type Output = ScreenSpaceXY;

    fn add(self, other: TextBoxXY) -> ScreenSpaceXY {
        ssxy!{
            self.x + other.x,
            self.y + other.y,
        }
    }
}

impl std::ops::Add<TextBoxSpaceXY> for TextBoxXY {
    type Output = ScreenSpaceXY;

    fn add(self, other: TextBoxSpaceXY) -> ScreenSpaceXY {
        ssxy!{
            self.x + other.x,
            self.y + other.y,
        }
    }
}

pub fn text_box_to_screen(xy: TextBoxSpaceXY, pos: TextBoxXY) -> ScreenSpaceXY {
    xy + pos
}

impl std::ops::Sub<TextBoxXY> for ScreenSpaceXY {
    type Output = TextBoxSpaceXY;

    fn sub(self, other: TextBoxXY) -> TextBoxSpaceXY {
        tbsxy!(self.x - other.x, self.y - other.y)
    }
}

pub fn screen_to_text_box(xy: ScreenSpaceXY, pos: TextBoxXY) -> TextBoxSpaceXY {
    xy - pos
}

#[derive(Clone, Copy, Default, Hash, PartialEq)]
/// The top left corner of the text is `(0.0, 0.0), top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down. Note that this is different than `TextBoxSpaceXY` since the text can be scrolled.
pub struct TextSpaceXY {
    pub x: abs::Pos,
    pub y: abs::Pos,
}

fmt_debug!(for TextSpaceXY: TextSpaceXY {x, y} in "tsxy!({}, {})", x, y);
fmt_display!(for TextSpaceXY: TextSpaceXY {x, y} in "{:?}", (x, y));

#[macro_export]
macro_rules! tsxy {
    //
    // Pattern matching
    //
    ($x: ident $(,)? $y: ident $(,)?) => {
        $crate::TextSpaceXY { x: $x.into(), y: $y.into() }
    };
    //
    // Initialization
    //
    ($x: expr, $y: expr $(,)?) => {
        $crate::TextSpaceXY { x: $x.into(), y: $y.into() }
    };
    () => {
        $crate::TextSpaceXY::default()
    };
}

impl From<TextSpaceXY> for (f32, f32) {
    fn from(TextSpaceXY { x, y }: TextSpaceXY) -> Self {
        (x.into(), y.into())
    }
}

impl MapElements<abs::Pos> for TextSpaceXY {
    fn map_elements(&self, mapper: &impl Fn(abs::Pos) -> abs::Pos) -> Self {
        Self {
            x: mapper(self.x),
            y: mapper(self.y),
        }
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

#[derive(Clone, Copy, Default, Hash, PartialEq)]
pub struct TextSpaceXYWH {
    pub xy: TextSpaceXY,
    pub wh: ScreenSpaceWH,
}

fmt_debug!(for TextSpaceXYWH: TextSpaceXYWH {xy, wh} in "tsxywh!({}, {})", xy, wh);

#[macro_export]
macro_rules! tsxywh {
    //
    // Pattern matching
    //
    ($x: ident, $y: ident, $w: ident, $h: ident) => {
        $crate::TextSpaceXYWH { 
            xy: $crate::tsxy!($x, $y),
            wh: $crate::sswh!($w, $h),
        }
    };
    //
    // Initialization
    //
    ($x: expr, $y: expr, $w: expr, $h: expr) => {
        $crate::TextSpaceXYWH { 
            xy: $crate::tsxy!($x, $y),
            wh: $crate::sswh!($w, $h),
        }
    };
    ($xy: expr, $wh: expr) => {
        $crate::TextSpaceXYWH { 
            xy: $xy,
            wh: $wh,
        }
    };
    () => {
        $crate::TextSpaceXYWH::default()
    };
}

#[derive(Clone, Copy, Default, Hash, PartialEq)]
/// An offset in TextBoxSpace.
/// The top left corner of the text is `(0.0, 0.0)`, top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down.
pub struct ScrollXY {
    pub x: abs::Pos,
    pub y: abs::Pos,
}

fmt_debug!(for ScrollXY: ScrollXY {x, y} in "slxy!({}, {})", x, y);
fmt_display!(for ScrollXY: ScrollXY {x, y} in "({}, {})", x, y);

/// This uses `slxy` becasue `scxy`, or `srxy` seem confusable with being for 
/// ScreenSpaceXY. `soxy` seems less evocative of scrolling than `slxy`.
#[macro_export]
macro_rules! slxy {
    //
    // Initialization
    //
    ($x: literal $(,)? $y: literal $(,)?) => {
        ScrollXY { x: $x.into(), y: $y.into() }
    };
    ($x: expr, $y: expr $(,)?) => {
        ScrollXY { x: $x.into(), y: $y.into() }
    };
    () => {
        ScrollXY::default()
    };
    //
    // Pattern matching
    //
    ($x: ident $(,)? $y: ident $(,)?) => {
        ScrollXY { x: $x, y: $y }
    };
    (_ $(,)? $y: ident $(,)?) => {
        ScrollXY { x: _, y: $y }
    };
    ($x: ident $(,)? _ $(,)?) => {
        ScrollXY { x: $x, y: _ }
    };
}

impl MapElements<abs::Pos> for ScrollXY {
    fn map_elements(&self, mapper: &impl Fn(abs::Pos) -> abs::Pos) -> Self {
        Self { 
            x: mapper(self.x),
            y: mapper(self.y),
        }
    }
}

impl From<ScrollXY> for (f32, f32) {
    fn from(ScrollXY { x, y }: ScrollXY) -> Self {
        (x.into(), y.into())
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

impl std::ops::Add<ScrollXY> for TextBoxSpaceXY {
    type Output = TextSpaceXY;

    fn add(self, other: ScrollXY) -> TextSpaceXY {
        TextSpaceXY {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl std::ops::Add<TextBoxSpaceXY> for ScrollXY {
    type Output = TextSpaceXY;

    fn add(self, other: TextBoxSpaceXY) -> TextSpaceXY {
        TextSpaceXY {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

pub fn text_box_to_text(xy: TextBoxSpaceXY, scroll: ScrollXY) -> TextSpaceXY {
    scroll + xy
}

impl std::ops::Sub<ScrollXY> for TextSpaceXY {
    type Output = TextBoxSpaceXY;

    fn sub(self, other: ScrollXY) -> TextBoxSpaceXY {
        TextBoxSpaceXY {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

pub fn text_to_text_box(xy: TextSpaceXY, scroll: ScrollXY) -> TextBoxSpaceXY {
    xy - scroll
}

pub enum PositionRound {
    Up,
    TowardsZero,
}

pub fn screen_space_to_position(
    xy: ScreenSpaceXY,
    text_box_pos: TextBoxXY,
    scroll: ScrollXY,
    char_dim: CharDim,
    round: PositionRound,
) -> Position {
    text_space_to_position(
        screen_space_to_text_space(xy, text_box_pos, scroll),
        char_dim,
        round,
    )
}

pub fn screen_space_to_text_space(
    xy: ScreenSpaceXY,
    text_box_pos: TextBoxXY,
    scroll: ScrollXY
) -> TextSpaceXY {
    text_box_to_text(screen_to_text_box(xy, text_box_pos), scroll)
}

fn normal_or_zero(x: f32) -> f32 {
    if x.is_normal() {
        x
    } else {
        0.0
    }
}

pub fn text_space_to_position(
    TextSpaceXY { x, y }: TextSpaceXY,
    CharDim { w, h }: CharDim,
    round: PositionRound,
) -> Position {
    // This is made much more conveinient by the monospace assumption!
    let pre_rounded = x.get() / w.get();

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
    let line = normal_or_zero(y.get() / h.get()) as usize;

    Position {
        offset: CharOffset(offset),
        line,
    }
}

pub fn text_space_to_screen_space(
    scroll: ScrollXY,
    text_box_pos: TextBoxXY,
    text_space_xy: TextSpaceXY
) -> ScreenSpaceXY {
    text_box_to_screen(
        text_to_text_box(text_space_xy, scroll),
        text_box_pos,
    )
}

pub fn position_to_screen_space(
    pos: Position,
    char_dim: CharDim,
    scroll: ScrollXY,
    text_box_pos: TextBoxXY,
) -> ScreenSpaceXY {
    text_space_to_screen_space(
        scroll,
        text_box_pos,
        position_to_text_space(pos, char_dim)
    )
}

pub fn position_to_text_space(
    Position { offset, line }: Position,
    CharDim { w, h }: CharDim,
) -> TextSpaceXY {
    // This is made much more conveinient by the monospace assumption!

    // Weird *graphical-only* stuff given a >2^24 long line and/or >2^24
    // lines seems better than an error box or something like that.
    TextSpaceXY {
        x: abs::Pos::from(abs::Ratio::from(offset.0) * w),
        y: abs::Pos::from(abs::Ratio::from(line) * h),
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum VisibilityAttemptResult {
    Succeeded,
//    ScreenTooSmall,
//    ScreenTooLarge,
//    ScreenTooWeird,
}

/// Each of these ratios represent the amount of the specifed *half*
/// of the given dimension will be part of the apron. So apron!(0.5)
/// means that a rectangular ring with thickness equal to 25% of the
/// width and height of the full rectangle.
#[derive(Clone, Copy, Debug, Default)]
pub struct Apron {
    pub left_w_ratio: F32_0_1,
    pub right_w_ratio: F32_0_1,
    pub top_h_ratio: F32_0_1,
    pub bottom_h_ratio: F32_0_1,
}

// TODO make a derive macro that hashes all the fields, but checks if fields are 
// f32/f64 and calls `to_bits` if they are.
macro_rules! hash_to_bits {
    (for $name: ty : $self: ident, $state: ident in $($field: ident),* ) => {
        macros::hash!(for $name: $self, $state in {
            $(
                $self.$field.to_bits().hash($state);
            )*
        });
    }
}

hash_to_bits!(for Apron : s, state in bottom_h_ratio, top_h_ratio, right_w_ratio, left_w_ratio);

#[macro_export]
macro_rules! apron {
    (
        $left_w_ratio: literal $(,)?
        $right_w_ratio: literal $(,)?
        $top_h_ratio: literal $(,)?
        $bottom_h_ratio: literal $(,)?
    ) => {
        Apron {
            left_w_ratio: $crate::f32_0_1!($left_w_ratio),
            right_w_ratio: $crate::f32_0_1!($right_w_ratio),
            top_h_ratio: $crate::f32_0_1!($top_h_ratio),
            bottom_h_ratio: $crate::f32_0_1!($bottom_h_ratio),
        }
    };
    (raw
        $left_w_ratio: literal $(,)?
        $right_w_ratio: literal $(,)?
        $top_h_ratio: literal $(,)?
        $bottom_h_ratio: literal $(,)?
    ) => {
        Apron {
            left_w_ratio: $left_w_ratio,
            right_w_ratio: $right_w_ratio,
            top_h_ratio: $top_h_ratio,
            bottom_h_ratio: $bottom_h_ratio,
        }
    };
    (
        $left_w_ratio: expr,
        $right_w_ratio: expr,
        $top_h_ratio: expr,
        $bottom_h_ratio: expr $(,)?
    ) => {
        Apron {
            left_w_ratio: $crate::f32_0_1!($left_w_ratio),
            right_w_ratio: $crate::f32_0_1!($right_w_ratio),
            top_h_ratio: $crate::f32_0_1!($top_h_ratio),
            bottom_h_ratio: $crate::f32_0_1!($bottom_h_ratio),
        }
    };
    (raw
        $left_w_ratio: expr,
        $right_w_ratio: expr,
        $top_h_ratio: expr,
        $bottom_h_ratio: expr $(,)?
    ) => {
        Apron {
            left_w_ratio: $left_w_ratio,
            right_w_ratio: $right_w_ratio,
            top_h_ratio: $top_h_ratio,
            bottom_h_ratio: $bottom_h_ratio,
        }
    };
    ($size: literal) => {
        Apron {
            left_w_ratio: $crate::f32_0_1!($size),
            right_w_ratio: $crate::f32_0_1!($size),
            top_h_ratio: $crate::f32_0_1!($size),
            bottom_h_ratio: $crate::f32_0_1!($size),
        }
    };
    (raw $size: expr) => {
        Apron {
            left_w_ratio: $size,
            right_w_ratio: $size,
            top_h_ratio: $size,
            bottom_h_ratio: $size,
        }
    };
    () => { Apron::default() }
}

impl MapElements<F32_0_1> for Apron {
    fn map_elements(&self, mapper: &impl Fn(F32_0_1) -> F32_0_1) -> Self {
        Self {
            left_w_ratio: mapper(self.left_w_ratio),
            right_w_ratio: mapper(self.right_w_ratio),
            top_h_ratio: mapper(self.top_h_ratio),
            bottom_h_ratio: mapper(self.bottom_h_ratio),
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
/// The space taken up by the outer box is what we call the "apron".
#[perf_viz::record]
pub fn attempt_to_make_xy_visible(
    scroll: &mut ScrollXY,
    outer_rect: TextBoxXYWH,
    apron: Apron,
    to_make_visible: TextSpaceXY,
) -> VisibilityAttemptResult {
    u!{VisibilityAttemptResult}

    attempt_to_make_line_space_pos_visible(
        &mut scroll.x,
        (outer_rect.xy.x, outer_rect.wh.w),
        apron.left_w_ratio,
        apron.right_w_ratio,
        to_make_visible.x,
    );

    attempt_to_make_line_space_pos_visible(
        &mut scroll.y,
        (outer_rect.xy.y, outer_rect.wh.h),
        apron.top_h_ratio,
        apron.bottom_h_ratio,
        to_make_visible.y,
    );

    dbg!(scroll);

    Succeeded
}

/// By line space, I mean that `to_make_visible` must be relative to `line_min`
/// and `visible` means within `line_min` to `line_min + w`, given `scroll` is
/// added to `to_make_visible`.
pub fn attempt_to_make_line_space_pos_visible(
    scroll: &mut abs::Pos,
    (line_min, w): (abs::Pos, abs::Length),
    apron_min_ratio: F32_0_1,
    apron_max_ratio: F32_0_1,
    to_make_visible: abs::Pos,
) -> VisibilityAttemptResult {
    u!{VisibilityAttemptResult}

    let to_make_visible_screen_space = (to_make_visible - *scroll) + line_min;

    let left_w = abs::Length::from(w.get() * apron_min_ratio.get()).halve();
    let right_w = abs::Length::from(w.get() *  apron_max_ratio.get()).halve();

    // In screen space
    let min: abs::Pos = left_w + line_min;
    let max: abs::Pos = abs::Pos::from(w) - right_w + line_min;

    // "Why do we assign to_make_visible to scroll?":
    // let to_make_visible = tmv
    // (here = is the algebra =)
    // tmv_screen = (tmv - scroll) + line_min
    // so if we want tmv_screen = line_min
    // tmv_screen = (tmv - scroll) + tmv_screen
    // 0 = (tmv - scroll)
    // scroll = tmv
    // therefore setting scroll to the value of tmv places the point
    // at the top left corner of the text box. We make further adjustments as needed.

    dbg!(
        to_make_visible,
        to_make_visible_screen_space,
        min,
        max,
        &scroll,
    );

    if to_make_visible_screen_space < min {
        dbg!();
        *scroll = to_make_visible - left_w;
    } else if to_make_visible_screen_space >= max {
        dbg!();
        *scroll = to_make_visible - (w - right_w);
    } else {
        // leave it alone
    }

    dbg!(scroll);

    Succeeded
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Default)]
/// A rectangle in screen space which represents the the space taken up by a text box.
pub struct TextBoxXYWH {
    pub xy: TextBoxXY,
    pub wh: ScreenSpaceWH,
}

impl From<TextBoxXYWH> for ScreenSpaceXYWH {
    fn from(TextBoxXYWH { xy, wh }: TextBoxXYWH) -> Self {
        Self { xy: xy.into(), wh }
    }
}

impl From<TextBoxXYWH> for ScreenSpaceRect {
    fn from(xywh: TextBoxXYWH) -> Self {
        let ssxywh: ScreenSpaceXYWH = xywh.into();
        ssxywh.into()
    }
}

impl MapElements<abs::Pos> for TextBoxXYWH {
    fn map_elements(&self, mapper: &impl Fn(abs::Pos) -> abs::Pos) -> Self {
        Self {
            xy: self.xy.map_elements(mapper),
            wh: self.wh.map_elements(mapper),
        }
    }
}

#[macro_export]
macro_rules! tbxywh {
    //
    // Pattern matching
    //
    ($x: ident, $y: ident, $w: ident, $h: ident) => {
        TextBoxXYWH {
            xy: tbxy!($x, $y),
            wh: sswh!($w, $h),
        }
    };
    ($xy: ident, $wh: ident) => {
        TextBoxXYWH {
            xy: $xy,
            wh: $wh,
        }
    };
    ($xy: ident) => {
        TextBoxXYWH {
            xy: $xy,
            wh: _
        }
    };
    //
    // Initialization
    //
    ($x: expr, $y: expr, $w: literal, $h: literal) => {
        TextBoxXYWH {
            xy: tbxy!($x, $y),
            wh: sswh!($w, $h),
        }
    };
    (raw $x: expr, $y: expr, $w: expr, $h: expr) => {
        TextBoxXYWH {
            xy: tbxy!($x, $y),
            wh: sswh!(raw $w, $h),
        }
    };
    ($x: expr, $y: expr, $w: expr, $h: expr) => {
        TextBoxXYWH {
            xy: tbxy!($x, $y),
            wh: sswh!($w, $h),
        }
    };
    ($xy: expr, $wh: expr) => {
        TextBoxXYWH {
            xy: $xy,
            wh: $wh,
        }
    };
    ($xy: expr) => {
        TextBoxXYWH {
            xy: $xy,
            ..TextBoxXYWH::default()
        }
    };
    () => {
        TextBoxXYWH::default()
    };
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq)]
pub struct FontInfo {
    pub text_char_dim: CharDim,
    pub status_char_dim: CharDim,
    pub tab_char_dim: CharDim,
    pub find_replace_char_dim: CharDim,
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq)]
/// Things that the editor needs to know which (may) depend on the size of the screen.
/// In a given `SetSizeDependents` call any of these are optional, but they should all be set
/// initially. Otherwise the defaults will be used.
pub struct SizeDependents {
    pub font_info: Option<FontInfo>,
    pub buffer_xywh: Option<TextBoxXYWH>,
    pub find_xywh: Option<TextBoxXYWH>,
    pub replace_xywh: Option<TextBoxXYWH>,
    pub go_to_position_xywh: Option<TextBoxXYWH>,
}

pub trait MapElements<T> {
    fn map_elements(&self, mapper: &impl Fn(T) -> T) -> Self;
}
