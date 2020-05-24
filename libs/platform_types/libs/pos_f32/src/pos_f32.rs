//! This module is called `pos_f32` and the main type is called `PosF32` but we 
//! actually restrict the values of `PosF32` to be positive, normal (that is 
//! non-sub-normal), values. The smallest value allowed is `f32::MIN_POSITIVE`.

use macros::{d, add_assign, sub_assign, mul_assign, div_assign};
use std::ops::{Add, Sub, Mul, Div};

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct PosF32(f32);

d!(for PosF32: PosF32::MIN);

impl PosF32 {
    pub const MIN: PosF32 = PosF32(f32::MIN_POSITIVE);
    pub const ONE: PosF32 = PosF32(1.0);
    pub const INFINITY: PosF32 = PosF32(f32::INFINITY);

    /// These were added on an as-needed basis, and arguably are only need to
    /// be here because as of this writing `is_non_neg` cannot be const-eval'd
    pub const ONE_SIXTY_FOURTH: PosF32 = PosF32(1.0 / 64.0);
    pub const ONE_THIRTY_SECONDTH: PosF32 = PosF32(1.0 / 32.0);
    pub const TWO: PosF32 = PosF32(2.0);
    pub const FOUR: PosF32 = PosF32(4.0);
    pub const ONE_HUNDRED_TWENTY_EIGHT: PosF32 = PosF32(128.0);
}

#[macro_export]
macro_rules! is_pos {
    ($float: expr) => {{
        // This returns false for NaNs
        $float >= f32::MIN_POSITIVE
    }};
}

#[macro_export]
macro_rules! pos_f32 {
    ($float: literal) => {{
        // const assertion 
        #[allow(unknown_lints, eq_op)]
        const _: [(); 0 - !{ $crate::is_pos!($float) } as usize] = [];

        $crate::PosF32::new_saturating($float)
    }};
    ($float: expr) => {
        $crate::PosF32::new_saturating($float)
    };
}

impl PosF32 {
    pub fn new_saturating(f: f32) -> Self {
        Self(if is_pos!(f) {
            f
        } else {
            // NaN ends up here
            f32::MIN_POSITIVE
        })
    }

    #[inline]
    pub fn get(&self) -> f32 {
        self.0
    }

    #[inline]
    pub fn to_bits(&self) -> u32 {
        self.0.to_bits()
    }
}

impl From<PosF32> for f32 {
    fn from(PosF32(f): PosF32) -> Self {
        f
    }
}

impl PartialEq<f32> for PosF32 {
    fn eq(&self, other: &f32) -> bool {
        self.0 == *other
    }
}

impl PartialEq<PosF32> for f32 {
    fn eq(&self, other: &PosF32) -> bool {
        *self == other.0
    }
}

impl PartialOrd<f32> for PosF32 {
    fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl PartialOrd<PosF32> for f32 {
    fn partial_cmp(&self, other: &PosF32) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.0)
    }
}

impl Add for PosF32 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        // Since we know both are positive, we know the sum is as well.
        PosF32(self.0 + other.0)
    }
}

add_assign!(for PosF32);

impl Add<f32> for PosF32 {
    type Output = f32;

    fn add(self, other: f32) -> Self::Output {
        self.0 + other
    }
}

impl Add<PosF32> for f32 {
    type Output = f32;

    fn add(self, other: PosF32) -> Self::Output {
        self + other.0
    }
}

add_assign!(<PosF32> for f32);

impl Sub for PosF32 {
    type Output = f32;

    fn sub(self, other: Self) -> Self::Output {
        // Even though we know both are positive, the difference may not be.
        self.0 - other.0
    }
}

impl Sub<f32> for PosF32 {
    type Output = f32;

    fn sub(self, other: f32) -> Self::Output {
        self.0 - other
    }
}

impl Sub<PosF32> for f32 {
    type Output = f32;

    fn sub(self, other: PosF32) -> Self::Output {
        self - other.0
    }
}

sub_assign!(<PosF32> for f32);

impl Mul for PosF32 {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        // Note that since nearly every valid element of PosF32 has a reciprocal
        // the set of operations included here overlaps significantly with the
        // ones in `impl Div for PosF32` See the comment on `impl Div for PosF32`
        // for why we decided to saturate here.
        pos_f32!(self.0 * other.0)
    }
}

mul_assign!(for PosF32);

impl Mul<f32> for PosF32 {
    type Output = f32;

    fn mul(self, other: f32) -> Self::Output {
        self.0 * other
    }
}

impl Mul<PosF32> for f32 {
    type Output = f32;

    fn mul(self, other: PosF32) -> Self::Output {
        self * other.0
    }
}

mul_assign!(<PosF32> for f32);

impl Div for PosF32 {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        // First, I will note here that since f32 values are only finitely many
        // bits wide, we will always have some precision issues. Just for 
        // convenience's sake, we would like this operation to be closed, since
        // the analogous operation over the strictly positive real numbers is
        // in fact closed. The question then becomes: do we care about the precision
        // loss? Since f32::MIN_POSITIVE is approximately 1.17*10^-38, some fiddling
        // around with graphs suggests that we don't get what seems like significant
        // precision loss until we get to numbers that approach 10^38 at least 
        // regarding whether (x * s) / s gets us back to x. 
        // See https://www.desmos.com/calculator/ntff4pab6c
        // For at least the current expected usage of this library, we won't need 
        // numbers close enough to 10^38 to cause those issues
        pos_f32!(self.0 / other.0)
    }
}

impl Div<f32> for PosF32 {
    type Output = f32;

    fn div(self, other: f32) -> Self::Output {
        self.0 / other
    }
}

impl Div<PosF32> for f32 {
    type Output = f32;

    fn div(self, other: PosF32) -> Self::Output {
        self / other.0
    }
}

div_assign!(<PosF32> for f32);

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;