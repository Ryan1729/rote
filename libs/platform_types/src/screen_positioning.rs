use super::*;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// The top left corner of the screen is `(0.0, 0.0)1, top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down.
pub struct ScreenSpaceXY {
    pub x: f32,
    pub y: f32,
}

fmt_display!(for ScreenSpaceXY: ScreenSpaceXY {x, y} in "{:?}", (x, y));

#[macro_export]
macro_rules! ssxy {
    //
    // Pattern matching
    //
    ($x: ident, $y: ident) => {
        ScreenSpaceXY { x: $x, y: $y }
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

#[macro_export]
macro_rules! sswh {
    //
    // Pattern matching
    //
    ($w: ident, $h: ident) => {
        ScreenSpaceWH { w: $w, h: $h }
    };
    //
    // Initialization
    //
    ($w: expr, $h: expr) => {
        ScreenSpaceWH { w: $w, h: $h }
    };
    () => {
        ScreenSpaceWH::default()
    };
}

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

#[derive(Clone, Copy, Debug, Default, PartialEq)]
/// A postion in screen space which represents the top left corner of a text box
/// Not to be confused with a `TextBoxSpaceXY`.
pub struct TextBoxXY {
    pub x: f32,
    pub y: f32,
}

fmt_display!(for TextBoxXY: TextBoxXY {x, y} in "{:?}", (x, y));

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

/// All `TextBoxXY` are screen space positions but the recerse is not true.
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
        text_box_to_text(screen_to_text_box(xy, text_box_pos), scroll),
        char_dim,
        round,
    )
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

pub fn position_to_screen_space(
    pos: Position,
    char_dim: CharDim,
    scroll: ScrollXY,
    text_box_pos: TextBoxXY,
) -> ScreenSpaceXY {
    text_box_to_screen(
        text_to_text_box(position_to_text_space(pos, char_dim), scroll),
        text_box_pos,
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

#[derive(Debug)]
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
    scroll: &mut ScrollXY,
    TextBoxXYWH {
        xy,
        wh: ScreenSpaceWH { w, h },
    }: TextBoxXYWH,
    apron: Apron,
    text: TextSpaceXY,
) -> VisibilityAttemptResult {
    use std::num::FpCategory::*;
    use VisibilityAttemptResult::*;

    let ScreenSpaceXY { x, y } = text_box_to_screen(text_to_text_box(text, *scroll), xy);

    // If these checks ever actually become a bottleneck ,then the easy solution is to just make
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

#[derive(Copy, Clone, Debug)]
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

#[derive(Clone, Copy, Debug, PartialEq, Default)]
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

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct FontInfo {
    pub text_char_dim: CharDim,
    pub status_char_dim: CharDim,
    pub tab_char_dim: CharDim,
    pub find_replace_char_dim: CharDim,
}

#[derive(Clone, Copy, Debug, PartialEq, Default)]
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
