#![deny(clippy::float_arithmetic)]
#![deny(unused)]
use macros::{add_assign, fmt_debug, fmt_display, d};

pub use abs;

#[derive(Clone, Copy, Default, Hash, PartialEq, PartialOrd)]
/// The top left corner of the screen is `(0.0, 0.0)`, top right corner is `(width, 0.0)`,
/// the bottom left corner is `(0.0, height)`. In other words, the x-axis point right, the y-axis
/// points down.
pub struct ScreenSpaceXY {
    pub x: abs::Pos,
    pub y: abs::Pos,
}

fmt_debug!(for ScreenSpaceXY: ScreenSpaceXY {x, y} in "ssxy!({}, {})", x, y);

fmt_display!(for ScreenSpaceXY: ScreenSpaceXY {x, y} in "({}, {})", x, y);

#[macro_export]
macro_rules! ssxy {
    //
    // Initialization
    //
    ($x: literal $(,)? $y: literal $(,)?) => {
        ScreenSpaceXY { x: $x.into(), y: $y.into() }
    };
    ($x: expr, $y: expr $(,)?) => {
        ScreenSpaceXY { x: $x.into(), y: $y.into() }
    };
    (raw $x: expr, $y: expr $(,)?) => {
        ScreenSpaceXY { x: $x, y: $y }
    };
    () => {
        ScreenSpaceXY::default()
    };
    //
    // Pattern matching
    //
    ($x: ident $(,)? $y: ident $(,)?) => {
        ScreenSpaceXY { x: $x, y: $y }
    };
    (_ $(,)? $y: ident $(,)?) => {
        ScreenSpaceXY { x: _, y: $y }
    };
    ($x: ident $(,)? _ $(,)?) => {
        ScreenSpaceXY { x: $x, y: _ }
    };
}

impl From<ScreenSpaceXY> for (f32, f32) {
    fn from(ScreenSpaceXY { x, y }: ScreenSpaceXY) -> Self {
        (f32::from(x), f32::from(y))
    }
}

impl std::ops::Add<ScreenSpaceXY> for ScreenSpaceXY {
    type Output = ScreenSpaceXY;

    fn add(self, other: ScreenSpaceXY) -> ScreenSpaceXY {
        ssxy!{
            self.x + other.x,
            self.y + other.y,
        }
    }
}

add_assign!(<ScreenSpaceXY> for ScreenSpaceXY);

impl std::ops::Add<(f32, f32)> for ScreenSpaceXY {
    type Output = ScreenSpaceXY;

    fn add(self, (x, y): (f32, f32)) -> ScreenSpaceXY {
        ssxy!{
            self.x + abs::Pos::from(x),
            self.y + abs::Pos::from(y),
        }
    }
}
add_assign!(<(f32, f32)> for ScreenSpaceXY);

impl std::ops::Add<ScreenSpaceXY> for (f32, f32) {
    type Output = (f32, f32);

    fn add(self, ScreenSpaceXY { x, y }: ScreenSpaceXY) -> (f32, f32) {
        (f32::from(self.0 + x), f32::from(self.1 + y))
    }
}
add_assign!(<ScreenSpaceXY> for (f32, f32));

#[derive(Clone, Copy, Default, Hash, PartialEq)]
pub struct ScreenSpaceWH {
    pub w: abs::Length,
    pub h: abs::Length,
}

fmt_debug!(for ScreenSpaceWH: ScreenSpaceWH {w, h} in "sswh!({}, {})", w, h);

fmt_display!(for ScreenSpaceWH: ScreenSpaceWH {w, h} in "{:?}", (w, h));

#[macro_export]
macro_rules! sswh {
    //
    // Pattern matching
    //
    (_ $(,)? $h: ident $(,)?) => {
        $crate::ScreenSpaceWH { w: _, h: $h }
    };
    ($w: ident $(,)? _ $(,)?) => {
        $crate::ScreenSpaceWH { w: $w, h: _ }
    };
    ($w: ident $(,)? $h: ident $(,)?) => {
        $crate::ScreenSpaceWH { w: $w, h: $h }
    };
    //
    // Initialization
    //
    ($w: literal $(,)? $h: literal $(,)?) => {
        $crate::ScreenSpaceWH { 
            w: $w.into(), 
            h: $h.into()
        }
    };
    (raw $w: literal $(,)? $h: literal $(,)?) => {
        $crate::ScreenSpaceWH { w: $w, h: $h }
    };
    ($w: expr, $h: expr $(,)?) => {
        $crate::ScreenSpaceWH { 
            w: $w.into(),
            h: $h.into()
        }
    };
    (raw $w: expr, $h: expr $(,)?) => {
        $crate::ScreenSpaceWH { w: $w, h: $h }
    };
    () => {
        $crate::ScreenSpaceWH::default()
    };
}

impl From<ScreenSpaceWH> for (f32, f32) {
    fn from(sswh!(w, h): ScreenSpaceWH) -> Self {
        (w.get(), h.get())
    }
}

impl From<ScreenSpaceRect> for ScreenSpaceWH {
    fn from(ssr!(min_x, min_y, max_x, max_y): ScreenSpaceRect) -> Self {
        sswh!(
            abs::Length::new_saturating(max_x - min_x),
            abs::Length::new_saturating(max_y - min_y)
        )
    }
}

#[must_use]
pub fn inside_rect(
    ScreenSpaceXY { x, y }: ScreenSpaceXY,
    ssr!{ min_x, min_y, max_x, max_y }: ScreenSpaceRect,
) -> bool {
    x >= min_x && x <= max_x && y >= min_y && y <= max_y
}

pub fn clamp_within(
    rect: &mut ScreenSpaceRect,
    ssr!{ min_x, min_y, max_x, max_y }: ScreenSpaceRect
) {
    if rect.min.x < min_x {
        rect.min.x = min_x;
    } else {
        // NaN ends up here
    };
    if rect.min.y < min_y {
        rect.min.y = min_y;
    } else {
        // NaN ends up here
    };

    if rect.max.x > max_x {
        rect.max.x = max_x;
    } else {
        // NaN ends up here
    };
    if rect.max.y > max_y {        rect.max.y = max_y;
    } else {
        // NaN ends up here
    };
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Hash)]
/// The dimensions of a character, in screen-space. This is very similar to 
/// `ScreenSpaceWH`, but it's nice for it to be harder to mixup generic screen
/// dimensions and Character dimensions.
// Plus since `CharDim` came before `ScreenSpaceWH` less code has to change if we keep `CharDim`
/// We are currently assuming the font is monospace!
pub struct CharDim {
    pub w: abs::Length,
    pub h: abs::Length,
}

fmt_display!(for CharDim: CharDim {w, h} in "({}, {})", w, h);

impl From<CharDim> for (f32, f32) {
    fn from(CharDim { w, h }: CharDim) -> Self {
        (w.get(), h.get())
    }
}

#[macro_export]
macro_rules! char_dim {
    ($w: literal $(,)? $h: literal $(,)?) => {
        CharDim {
            w: $w.into(),
            h: $h.into(),
        }
    };
    ($w: expr, $h: expr $(,)?) => {
        CharDim {
            w: $w.into(),
            h: $h.into(),
        }
    };
    (raw $w: expr, $h: expr $(,)?) => {
        CharDim {
            w: $w,
            h: $h,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, PartialOrd)]
pub struct ScreenSpaceRect {
    /// min: Position on screen to render, in pixels from top-left. Defaults to (0, 0).
    pub min: ScreenSpaceXY,
    /// max: Max (width, height) bounds, in pixels from top-left. Defaults to unbounded.
    pub max: ScreenSpaceXY,
}
d!(for ScreenSpaceRect : ScreenSpaceRect{
min: ssxy!(abs::Pos::ZERO, abs::Pos::ZERO), max: ssxy!(abs::Pos::MAX, abs::Pos::MAX)
});

fmt_display!(for ScreenSpaceRect: ScreenSpaceRect {min, max} in "({},{})", min, max);

#[macro_export]
macro_rules! ssr {
    //
    // Pattern matching
    //
    ($min_x: ident $(,)? $min_y: ident $(,)? $max_x: ident $(,)? $max_y: ident $(,)?) => {
        ScreenSpaceRect {
            min: ScreenSpaceXY{ x: $min_x, y: $min_y },
            max: ScreenSpaceXY{ x: $max_x, y: $max_y },
        }
    };
    ($min_x: ident $(,)? _ $(,)? $max_x: ident $(,)? _ $(,)?) => {
        ScreenSpaceRect {
            min: ScreenSpaceXY{ x: $min_x, _ },
            max: ScreenSpaceXY{ x: $max_x, _ },
        }
    };
    (_ $(,)? $min_y: ident $(,)? _ $(,)? $max_y: ident $(,)?) => {
        ScreenSpaceRect {
            min: ScreenSpaceXY{ x: _, $min_y },
            max: ScreenSpaceXY{ x: _, $max_y },
        }
    };
    ($min_x: ident $(,)? $min_y: ident $(,)? _ $(,)? _ $(,)?) => {
        ScreenSpaceRect {
            min: ScreenSpaceXY{ x: $min_x, y: $min_y },
            max: _,
        }
    };
    (_ $(,)? _ $(,)? $max_x: ident $(,)? $max_y: ident $(,)?) => {
        ScreenSpaceRect {
            min: _,
            max: ScreenSpaceXY{ x: $max_x, y: $max_y },
        }
    };
    ($min: ident $(,)? $max: ident $(,)?) => {
        ScreenSpaceRect {
            min: $min,
            max: $max,
        }
    };
    ($min: ident $(,)? $max: ident $(,)?) => {
        ScreenSpaceRect {
            min: $min,
            max: $max,
        }
    };
    ($min: ident $(,)?) => {
        ScreenSpaceRect {
            min: $min,
            max: _
        }
    };
    //
    // Initialization
    //
    ($min_x: expr, $min_y: expr, $max_x: expr, $max_y: expr $(,)?) => {
        ScreenSpaceRect {
            min: ssxy!($min_x, $min_y),
            max: ssxy!($max_x, $max_y),
        }
    };
    (_, _, $max_x: expr, $max_y: expr $(,)?) => {
        ScreenSpaceRect {
            max: ssxy!($max_x, $max_y),
            ..ScreenSpaceRect::default()
        }
    };
    (raw $min_x: expr, $min_y: expr, $max_x: expr, $max_y: expr $(,)?) => {
        ScreenSpaceRect {
            min: ssxy!(raw $min_x, $min_y),
            max: ssxy!(raw $max_x, $max_y),
        }
    };
    ($min: expr, $max: expr $(,)?) => {
        ScreenSpaceRect {
            min: $min,
            max: $max,
        }
    };
    ($min: expr $(,)?) => {
        ScreenSpaceRect {
            min: $min,
            ..ScreenSpaceRect::default()
        }
    };
    (_, $max: expr $(,)?) => {
        ScreenSpaceRect {
            max: $max,
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
    #[must_use]
    #[allow(dead_code)]
    pub fn with_min_x(&self, min_x: abs::Pos) -> Self {
        ScreenSpaceRect {
            min: ssxy!(min_x, self.min.y),
            ..*self
        }
    }

    #[must_use]
    pub fn with_min_y(&self, min_y: abs::Pos) -> Self {
        ScreenSpaceRect {
            min: ssxy!(self.min.x, min_y),
            ..*self
        }
    }

    #[must_use]
    pub fn with_max_x(&self, max_x: abs::Pos) -> Self {
        ScreenSpaceRect {
            max: ssxy!(max_x, self.max.y),
            ..*self
        }
    }
    #[must_use]
    pub fn with_max_y(&self, max_y: abs::Pos) -> Self {
        ScreenSpaceRect {
            max: ssxy!(self.max.x, max_y),
            ..*self
        }
    }

    #[must_use]
    pub fn width(&self) -> abs::Length {
        (self.max.x - self.min.x).into()
    }
    #[must_use]
    pub fn height(&self) -> abs::Length {
        (self.max.y - self.min.y).into()
    }

    #[must_use]
    pub fn middle(&self) -> (abs::Pos, abs::Pos) {
        (
            (self.min.x + self.max.x).halve(),
            (self.min.y + self.max.y).halve(),
        )
    }

    #[must_use]
    pub fn has_any_area(&self) -> bool {
        self.min.x < self.max.x && self.min.y < self.max.y
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
            xy: ScreenSpaceXY{x, y},
            wh: ScreenSpaceWH{w, h},
        }: ScreenSpaceXYWH,
    ) -> Self {
        ssr!(
            x,
            y,
            x + w,
            y + h
        )
    }
}

impl From<(ScreenSpaceXY, ScreenSpaceWH)> for ScreenSpaceRect {
    fn from(
        (xy, wh): (ScreenSpaceXY, ScreenSpaceWH),
    ) -> Self {
        ssxywh!(xy, wh).into()
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