use super::*;
use macros::{dbg, u};

// TODO make a derive macro that hashes all the fields, but checks if fields are f32/f64 and
// calls `to_bits` if they are.
macro_rules! hash_to_bits {
    (for $name: ty : $self: ident, $state: ident in $($field: ident),* ) => {
        macros::hash!(for $name: $self, $state in {
            $(
                $self.$field.to_bits().hash($state);
            )*
        });
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// The top left corner of the screen is `(0.0, 0.0)`, top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down.
pub struct ScreenSpaceXY {
    pub x: f32,
    pub y: f32,
}

hash_to_bits!(for ScreenSpaceXY: s, state in x, y);

fmt_display!(for ScreenSpaceXY: ScreenSpaceXY {x, y} in "{:?}", (x, y));

impl MapElements<f32> for ScreenSpaceXY {
    fn map_elements(&self, mapper: &impl Fn(f32) -> f32) -> Self {
        Self { 
            x: mapper(self.x),
            y: mapper(self.y),
        }
    }
}

#[macro_export]
macro_rules! ssxy {
    //
    // Pattern matching
    //
    ($x: ident, $y: ident) => {
        ScreenSpaceXY { x: $x, y: $y }
    };
    (_, $y: ident) => {
        ScreenSpaceXY { x: _, y: $y }
    };
    ($x: ident, _) => {
        ScreenSpaceXY { x: $x, y: _ }
    };
    //
    // Initialization
    //
    ($x: expr, $y: expr) => {
        ScreenSpaceXY { x: $x, y: $y }
    };
    () => {
        ScreenSpaceXY::default()
    };
}

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

impl std::ops::Add<(f32, f32)> for ScreenSpaceXY {
    type Output = ScreenSpaceXY;

    fn add(self, (x, y): (f32, f32)) -> ScreenSpaceXY {
        ScreenSpaceXY {
            x: self.x + x,
            y: self.y + y,
        }
    }
}
add_assign!(<(f32, f32)> for ScreenSpaceXY);

impl std::ops::Add<ScreenSpaceXY> for (f32, f32) {
    type Output = (f32, f32);

    fn add(self, ScreenSpaceXY { x, y }: ScreenSpaceXY) -> (f32, f32) {
        (self.0 + x, self.1 + y)
    }
}
add_assign!(<ScreenSpaceXY> for (f32, f32));

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct ScreenSpaceWH {
    pub w: f32,
    pub h: f32,
}

fmt_display!(for ScreenSpaceWH: ScreenSpaceWH {w, h} in "{:?}", (w, h));

hash_to_bits!(for ScreenSpaceWH: s, state in w, h);

impl MapElements< f32> for ScreenSpaceWH {
    fn map_elements(&self, mapper: &impl Fn(f32) -> f32) -> Self {
        Self { 
            w: mapper(self.w),
            h: mapper(self.h),
        }
    }
}

#[macro_export]
macro_rules! sswh {
    //
    // Pattern matching
    //
    (_, $h: ident) => {
        $crate::ScreenSpaceWH { w: _, h: $h }
    };
    ($w: ident, _) => {
        $crate::ScreenSpaceWH { w: $w, h: _ }
    };
    ($w: ident, $h: ident) => {
        $crate::ScreenSpaceWH { w: $w, h: $h }
    };
    //
    // Initialization
    //
    ($w: expr, $h: expr) => {
        $crate::ScreenSpaceWH { w: $w, h: $h }
    };
    () => {
        $crate::ScreenSpaceWH::default()
    };
}

impl From<ScreenSpaceWH> for (f32, f32) {
    fn from(sswh!(w, h): ScreenSpaceWH) -> Self {
        (w, h)
    }
}

impl From<ScreenSpaceRect> for ScreenSpaceWH {
    fn from(ssr!(_, _, w, h): ScreenSpaceRect) -> Self {
        sswh!(w, h)
    }
}

pub fn inside_rect(
    ScreenSpaceXY { x, y }: ScreenSpaceXY,
    ScreenSpaceRect { min, max }: ScreenSpaceRect,
) -> bool {
    x >= min.0 && x <= max.0 && y >= min.1 && y <= max.1
}

pub fn clamp_within(rect: &mut ScreenSpaceRect, ScreenSpaceRect { min, max }: ScreenSpaceRect) {
    if rect.min.0 < min.0 {
        rect.min.0 = min.0
    } else {
        // NaN ends up here
    };
    if rect.min.1 < min.1 {
        rect.min.1 = min.1
    } else {
        // NaN ends up here
    };

    if rect.max.0 > max.0 {
        rect.max.0 = max.0
    } else {
        // NaN ends up here
    };
    if rect.max.1 > max.1 {
        rect.max.1 = max.1
    } else {
        // NaN ends up here
    };
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// It's nice for it to be harder to mixup screen dimensions and Character dimension.
// Plus since `CharDim` came before `ScreenSpaceWH` less code has to change if we keep `CharDim`
/// We are currently assuming the font is monospace!
pub struct CharDim {
    pub w: f32,
    pub h: f32,
}

hash_to_bits!(for CharDim: s, state in w, h);

fmt_display!(for CharDim: CharDim {w, h} in "{:?}", (w, h));

impl From<CharDim> for (f32, f32) {
    fn from(CharDim { w, h }: CharDim) -> Self {
        (w, h)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// A postion in screen space which represents the top left corner of a text box
/// Not to be confused with a `TextBoxSpaceXY`.
pub struct TextBoxXY {
    pub x: f32,
    pub y: f32,
}

fmt_display!(for TextBoxXY: TextBoxXY {x, y} in "{:?}", (x, y));

hash_to_bits!(for TextBoxXY: s, state in x, y);

#[macro_export]
macro_rules! tbxy {
    //
    // Pattern matching
    //
    ($x: ident, $y: ident) => {
        TextBoxXY { x: $x, y: $y }
    };
    //
    // Initialization
    //
    ($x: expr, $y: expr) => {
        TextBoxXY { x: $x, y: $y }
    };
    () => {
        TextBoxXY::default()
    };
}

impl From<TextBoxXY> for (f32, f32) {
    fn from(TextBoxXY { x, y }: TextBoxXY) -> Self {
        (x, y)
    }
}

/// All `TextBoxXY` are screen space positions but the reverse is not true.
impl From<TextBoxXY> for ScreenSpaceXY {
    fn from(TextBoxXY { x, y }: TextBoxXY) -> Self {
        ScreenSpaceXY { x, y }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// A vector in the space with the origin at the top left corner of a given text box.
/// The top left corner of the text box is `(0.0, 0.0), top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down. Note that this is different than `TextSpaceXY` since the text can be scrolled.
pub struct TextBoxSpaceXY {
    pub x: f32,
    pub y: f32,
}

fmt_display!(for TextBoxSpaceXY: TextBoxSpaceXY {x, y} in "{:?}", (x, y));

hash_to_bits!(for TextBoxSpaceXY: s, state in x, y);

impl From<TextBoxSpaceXY> for (f32, f32) {
    fn from(TextBoxSpaceXY { x, y }: TextBoxSpaceXY) -> Self {
        (x, y)
    }
}

impl std::ops::Add<TextBoxXY> for TextBoxSpaceXY {
    type Output = ScreenSpaceXY;

    fn add(self, other: TextBoxXY) -> ScreenSpaceXY {
        ScreenSpaceXY {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl std::ops::Add<TextBoxSpaceXY> for TextBoxXY {
    type Output = ScreenSpaceXY;

    fn add(self, other: TextBoxSpaceXY) -> ScreenSpaceXY {
        ScreenSpaceXY {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

pub fn text_box_to_screen(xy: TextBoxSpaceXY, pos: TextBoxXY) -> ScreenSpaceXY {
    xy + pos
}

impl std::ops::Sub<TextBoxXY> for ScreenSpaceXY {
    type Output = TextBoxSpaceXY;

    fn sub(self, other: TextBoxXY) -> TextBoxSpaceXY {
        TextBoxSpaceXY {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

pub fn screen_to_text_box(xy: ScreenSpaceXY, pos: TextBoxXY) -> TextBoxSpaceXY {
    xy - pos
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// The top left corner of the text is `(0.0, 0.0), top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down. Note that this is different than `TextBoxSpaceXY` since the text can be scrolled.
pub struct TextSpaceXY {
    pub x: f32,
    pub y: f32,
}

fmt_display!(for TextSpaceXY: TextSpaceXY {x, y} in "{:?}", (x, y));

hash_to_bits!(for TextSpaceXY: s, state in x, y);

#[macro_export]
macro_rules! tsxy {
    //
    // Pattern matching
    //
    ($x: ident, $y: ident) => {
        $crate::TextSpaceXY { x: $x, y: $y }
    };
    //
    // Initialization
    //
    ($x: expr, $y: expr) => {
        $crate::TextSpaceXY { x: $x, y: $y }
    };
    () => {
        $crate::TextSpaceXY::default()
    };
}

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

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq)]
pub struct TextSpaceXYWH {
    pub xy: TextSpaceXY,
    pub wh: ScreenSpaceWH,
}

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

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// An offset in TextBoxSpace.
/// The top left corner of the text is `(0.0, 0.0)`, top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down.
pub struct ScrollXY {
    pub x: f32,
    pub y: f32,
}

fmt_display!(for ScrollXY: ScrollXY {x, y} in "{:?}", (x, y));

hash_to_bits!(for ScrollXY: s, state in x, y);

impl MapElements< f32> for ScrollXY {
    fn map_elements(&self, mapper: &impl Fn(f32) -> f32) -> Self {
        Self { 
            x: mapper(self.x),
            y: mapper(self.y),
        }
    }
}

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
    #[allow(clippy::cast_precision_loss)]
    TextSpaceXY {
        x: offset.0 as f32 * w,
        y: line as f32 * h,
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum VisibilityAttemptResult {
    Succeeded,
    ScreenTooSmall,
    ScreenTooLarge,
    ScreenTooWeird,
    ApronEdgeTooSmall,
    ApronEdgeTooLarge,
    ApronEdgeTooWeird,
}

#[derive(Clone, Copy, Debug)]
pub struct Apron {
    pub left_w: f32,
    pub right_w: f32,
    pub top_h: f32,
    pub bottom_h: f32,
}

hash_to_bits!(for Apron : s, state in bottom_h, top_h, right_w, left_w);

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
/// The space taken up by the outer box is what we call the "apron".
#[perf_viz::record]
pub fn attempt_to_make_xy_visible(
    scroll: &mut ScrollXY,
    outer_rect: TextBoxXYWH,
    apron: Apron,
    to_make_visible: TextSpaceXY,
) -> VisibilityAttemptResult {
    u!{std::num::FpCategory, VisibilityAttemptResult}

    let ScreenSpaceWH { w, h } = outer_rect.wh;

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

    let TextSpaceXY { x, y } = to_make_visible;

    let to_make_visible_ss = text_space_to_screen_space(
        *scroll,
        outer_rect.xy,
        to_make_visible,
    );

    // In screen space
    let min_x = apron.left_w + outer_rect.xy.x;
    let max_x = stay_positive!(w - apron.right_w) + outer_rect.xy.x;
    let min_y = apron.top_h + outer_rect.xy.y;
    let max_y = stay_positive!(h - apron.bottom_h) + outer_rect.xy.y;

    dbg!(    
        &scroll,
        &outer_rect,
        x,
        y,
        w,
        h,
        &apron,
        to_make_visible,
        min_x,
        max_x,
        min_y,
        max_y
    );

    // If these checks ever actually become a bottleneck, then the easy solution is to just make
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
        (Subnormal, _) | (_, Subnormal) => return ApronEdgeTooSmall,
        (Normal, Normal) if apron.left_w > w || apron.top_h > h => return ApronEdgeTooLarge,
        (Zero, _) | (_, Zero) | (Normal, Normal) => {}
    }

    match (apron.right_w.classify(), apron.bottom_h.classify()) {
        (Nan, _) | (_, Nan) => return ApronEdgeTooWeird,
        (Infinite, _) | (_, Infinite) => return ApronEdgeTooLarge,
         | (Subnormal, _) | (_, Subnormal) => return ApronEdgeTooSmall,
        (Normal, Normal) if apron.right_w > w || apron.bottom_h > h => return ApronEdgeTooLarge,
        (Zero, _) | (_, Zero) | (Normal, Normal) => {}
    }

    // let to_make_visible = tmv
    // (here = is the algebra =)
    // tmv_screen = (tmv_text - scroll_xy) + outer_rect.xy
    // so if we want tmv_screen = outer_rect.xy
    // tmv_screen = (tmv_text - scroll_xy) + tmv_screen
    // 0 = (tmv_text - scroll_xy)
    // scroll_xy = tmv_text
    // therefore setting scroll_xy to the value of tmv_text places the point
    // at the top left corner of the text box. We make further adjustments as needed.

    dbg!(x, to_make_visible_ss.x, min_x);
    if to_make_visible_ss.x < min_x {
        scroll.x = x - apron.left_w;
    } else if to_make_visible_ss.x >= max_x {
        scroll.x = x - (w - apron.right_w);
    } else {
        // leave it alone
    }

    dbg!(y, to_make_visible_ss.y, min_y, max_y);
    if to_make_visible_ss.y < min_y {
        scroll.y = y - apron.top_h;
    } else if to_make_visible_ss.y >= max_y {
        scroll.y = y - (h - apron.bottom_h);
    } else {
        // leave it alone
    }

    dbg!(scroll);

    Succeeded
}

#[derive(Copy, Clone, Debug)]
pub struct ScreenSpaceRect {
    /// min: Position on screen to render, in pixels from top-left. Defaults to (0, 0).
    pub min: (f32, f32),
    /// max: Max (width, height) bounds, in pixels from top-left. Defaults to unbounded.
    pub max: (f32, f32),
}
d!(for ScreenSpaceRect : ScreenSpaceRect{
min: (0.0, 0.0), max: (std::f32::INFINITY, std::f32::INFINITY)
});
ord!(and friends for ScreenSpaceRect : r, other in {
// I don't care if this is the best ordering, I just want an ordering,
r.min.0.to_bits().cmp(&other.min.0.to_bits())
    .then_with(|| r.min.1.to_bits().cmp(&other.min.1.to_bits()))
    .then_with(|| r.max.0.to_bits().cmp(&other.max.0.to_bits()))
    .then_with(|| r.max.1.to_bits().cmp(&other.max.1.to_bits()))
});

impl std::hash::Hash for ScreenSpaceRect {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.min.0.to_bits().hash(state);
        self.min.1.to_bits().hash(state);
        self.max.0.to_bits().hash(state);
        self.max.1.to_bits().hash(state);
    }}

#[macro_export]
macro_rules! ssr {
    //
    // Pattern matching
    //
    ($min_x: ident, $min_y: ident, $max_x: ident, $max_y: ident) => {
        ScreenSpaceRect {
            min: ($min_x, $min_y),
            max: ($max_x, $max_y),
        }
    };
    ($min_x: ident, _, $max_x: ident, _) => {
        ScreenSpaceRect {
            min: ($min_x, _),
            max: ($max_x, _),
        }
    };
    (_, $min_y: ident, _, $max_y: ident) => {
        ScreenSpaceRect {
            min: (_, $min_y),
            max: (_, $max_y),
        }
    };
    ($min_x: ident, $min_y: ident, _, _) => {
        ScreenSpaceRect {
            min: ($min_x, $min_y),
            max: (_, _),
        }
    };
    (_, _, $max_x: ident, $max_y: ident) => {
        ScreenSpaceRect {
            min: (_, _),
            max: ($max_x, $max_y),
        }
    };
    ($min: ident, $max: ident) => {
        ScreenSpaceRect {
            min: $min,
            max: $max,
        }
    };
    ($min: ident) => {
        ScreenSpaceRect {
            min: $min,
            max: _
        }
    };
    //
    // Initialization
    //
    ($min_x: expr, $min_y: expr, $max_x: expr, $max_y: expr) => {
        ScreenSpaceRect {
            min: ($min_x, $min_y),
            max: ($max_x, $max_y),
        }
    };
    ($min: expr, $max: expr) => {
        ScreenSpaceRect {
            min: $min,
            max: $max,
        }
    };
    ($min: expr) => {
        ScreenSpaceRect {
            min: $min,
            ..ScreenSpaceRect::default()
        }
    };
    () => {
        ScreenSpaceRect::default()
    };
}

impl std::ops::Add<ScreenSpaceXY> for ScreenSpaceRect {
    type Output = ScreenSpaceRect;

    fn add(mut self, other: ScreenSpaceXY) -> ScreenSpaceRect {
        self.min += other;
        self.max += other;
        self
    }
}

impl ScreenSpaceRect {
    #[allow(dead_code)]
    pub fn with_min_x(&self, min_x: f32) -> Self {
        ScreenSpaceRect {
            min: (min_x, self.min.1),
            ..*self
        }
    }
    pub fn with_min_y(&self, min_y: f32) -> Self {
        ScreenSpaceRect {
            min: (self.min.0, min_y),
            ..*self
        }
    }

    pub fn with_max_x(&self, max_x: f32) -> Self {
        ScreenSpaceRect {
            max: (max_x, self.max.1),
            ..*self
        }
    }
    pub fn with_max_y(&self, max_y: f32) -> Self {
        ScreenSpaceRect {
            max: (self.max.0, max_y),
            ..*self
        }
    }

    pub fn width(&self) -> f32 {
        self.max.0 - self.min.0
    }

    pub fn height(&self) -> f32 {
        self.max.1 - self.min.1
    }

    pub fn middle(&self) -> (f32, f32) {
        (
            (self.min.0 + self.max.0) / 2.0,
            (self.min.1 + self.max.1) / 2.0,
        )
    }

    pub fn has_any_area(&self) -> bool {
        self.min.0 < self.max.0 && self.min.1 < self.max.1
    }
}

#[derive(Copy, Clone, Debug, Hash)]
pub struct ScreenSpaceXYWH {
    pub xy: ScreenSpaceXY,
    pub wh: ScreenSpaceWH,
}

impl From<ScreenSpaceXYWH> for ScreenSpaceRect {
    fn from(
        ScreenSpaceXYWH {
            xy: ScreenSpaceXY { x, y },
            wh: ScreenSpaceWH { w, h },
        }: ScreenSpaceXYWH,
    ) -> Self {
        ssr!(x, y, x + w, y + h)
    }
}

impl From<(ScreenSpaceXY, ScreenSpaceWH)> for ScreenSpaceRect {
    fn from(
        (ScreenSpaceXY { x, y }, ScreenSpaceWH { w, h }): (ScreenSpaceXY, ScreenSpaceWH),
    ) -> Self {
        ssr!(x, y, x + w, y + h)
    }
}

#[macro_export]
macro_rules! ssxywh {
    //
    // Pattern matching
    //
    ($x: ident, $y: ident, $w: ident, $h: ident) => {
        ScreenSpaceXYWH {
            xy: ssxy!($x, $y),
            wh: sswh!($w, $h),
        }
    };
    ($xy: ident, $wh: ident) => {
        ScreenSpaceXYWH {
            xy: $xy,
            wh: $wh,
        }
    };
    ($xy: ident) => {
        ScreenSpaceXYWH {
            xy: $xy,
            wh: _
        }
    };
    //
    // Initialization
    //
    ($x: expr, $y: expr, $w: expr, $h: expr) => {
        ScreenSpaceXYWH {
            xy: ssxy!($x, $y),
            wh: sswh!($w, $h),
        }
    };
    ($xy: expr, $wh: expr) => {
        ScreenSpaceXYWH {
            xy: $xy,
            wh: $wh,
        }
    };
    ($xy: expr) => {
        ScreenSpaceXYWH {
            xy: $xy,
            ..ScreenSpaceXYWH::default()
        }
    };
    () => {
        ScreenSpaceXYWH::default()
    };
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
