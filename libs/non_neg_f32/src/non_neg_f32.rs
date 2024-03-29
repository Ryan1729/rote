//! This module is called `non_neg_f32` and the main type is called `NonNegF32` 
//! but we actually restrict the values of `PosF32` to be positive, normal (that is 
//! non-sub-normal), values. The smallest non-zero value allowed is 
//! `f32::MIN_POSITIVE`.
use macros::{add_assign, sub_assign, mul_assign, div_assign, SaturatingSub};
pub use pos_f32::{is_pos, pos_f32, PosF32};
use std::ops::{Add, Sub, Mul, Div};

#[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct NonNegF32(f32);

impl NonNegF32 {
    pub const ZERO: NonNegF32 = NonNegF32(0.0);
    pub const ONE: NonNegF32 = NonNegF32(1.0);
    pub const INFINITY: NonNegF32 = NonNegF32(f32::INFINITY);
}

#[macro_export]
macro_rules! is_non_neg {
    ($float: expr) => {{
        let f: f32 = $float;
        // `>= f32::MIN_POSITIVE` is needed to eliminate positive 
        // NaNs and subnormals and `is_sign_positive` is needed 
        // to eliminate -0.0
        (f == 0.0 && f.is_sign_positive()) || $crate::is_pos!(f)
    }};
}

#[macro_export]
macro_rules! non_neg_f32 {
    ($float: literal) => {{
        // This doesn't work on stable as of this writing
        // const assertion 
        // #[allow(unknown_lints, eq_op)]
        // const _: [(); 0 - !{ $crate::is_non_neg!($float) } as usize] = [];

        NonNegF32::new_saturating($float)
    }};
    ($float: expr) => {
        NonNegF32::new_saturating($float)
    };
}

impl NonNegF32 {
    #[must_use]
    pub fn new_saturating(f: f32) -> Self {
        Self(if is_non_neg!(f) {
            f
        } else {
            // NaN ends up here
            0.0
        })
    }

    #[must_use]
    #[inline]
    pub fn get(&self) -> f32 {
        self.0
    }

    #[must_use]
    #[inline]
    pub fn to_bits(&self) -> u32 {
        self.0.to_bits()
    }
}

impl From<NonNegF32> for f32 {
    fn from(NonNegF32(f): NonNegF32) -> Self {
        f
    }
}

impl From<PosF32> for NonNegF32 {
    fn from(p: PosF32) -> Self {
        NonNegF32(p.get())
    }
}

impl PartialEq<f32> for NonNegF32 {
    fn eq(&self, other: &f32) -> bool {
        self.0 == *other
    }
}

impl PartialEq<NonNegF32> for f32 {
    fn eq(&self, other: &NonNegF32) -> bool {
        *self == other.0
    }
}

impl PartialEq<PosF32> for NonNegF32 {
    fn eq(&self, other: &PosF32) -> bool {
        self.0 == other.get()
    }
}

impl PartialEq<NonNegF32> for PosF32 {
    fn eq(&self, other: &NonNegF32) -> bool {
        self.get() == other.0
    }
}

impl PartialOrd<f32> for NonNegF32 {
    fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl PartialOrd<NonNegF32> for f32 {
    fn partial_cmp(&self, other: &NonNegF32) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.0)
    }
}

impl PartialOrd<PosF32> for NonNegF32 {
    fn partial_cmp(&self, other: &PosF32) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.get())
    }
}

impl PartialOrd<NonNegF32> for PosF32 {
    fn partial_cmp(&self, other: &NonNegF32) -> Option<std::cmp::Ordering> {
        self.get().partial_cmp(&other.0)
    }
}

impl Add for NonNegF32 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        // Since we know both are non-negative, we know the sum is as well.
        NonNegF32(self.0 + other.0)
    }
}

add_assign!(for NonNegF32);

impl Add<f32> for NonNegF32 {
    type Output = f32;

    fn add(self, other: f32) -> Self::Output {
        self.0 + other
    }
}

impl Add<NonNegF32> for f32 {
    type Output = f32;

    fn add(self, other: NonNegF32) -> Self::Output {
        self + other.0
    }
}

add_assign!(<NonNegF32> for f32);

impl Add<PosF32> for NonNegF32 {
    type Output = PosF32;

    fn add(self, other: PosF32) -> Self::Output {
        pos_f32!(self.0 + other.get())
    }
}

impl Add<NonNegF32> for PosF32 {
    type Output = PosF32;

    fn add(self, other: NonNegF32) -> Self::Output {
        pos_f32!(self.get() + other.0)
    }
}

add_assign!(<NonNegF32> for PosF32);

impl Sub for NonNegF32 {
    type Output = f32;

    fn sub(self, other: Self) -> Self::Output {
        // Even though we know both are non-negative, the difference may not be.
        self.0 - other.0
    }
}

impl Sub<f32> for NonNegF32 {
    type Output = f32;

    fn sub(self, other: f32) -> Self::Output {
        self.0 - other
    }
}

impl Sub<NonNegF32> for f32 {
    type Output = f32;

    fn sub(self, other: NonNegF32) -> Self::Output {
        self - other.0
    }
}

sub_assign!(<NonNegF32> for f32);

impl Sub<PosF32> for NonNegF32 {
    type Output = f32;

    fn sub(self, other: PosF32) -> Self::Output {
        self.0 - other.get()
    }
}

impl Sub<NonNegF32> for PosF32 {
    type Output = f32;

    fn sub(self, other: NonNegF32) -> Self::Output {
        self.get() - other.0
    }
}

impl Mul for NonNegF32 {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        // Since we know both are non-negative, we know the product is as well.
        // Note we need to deal with sub-normals
        non_neg_f32!(self.0 * other.0)
    }
}

mul_assign!(for NonNegF32);

impl Mul<f32> for NonNegF32 {
    type Output = f32;

    fn mul(self, other: f32) -> Self::Output {
        self.0 * other
    }
}

impl Mul<NonNegF32> for f32 {
    type Output = f32;

    fn mul(self, other: NonNegF32) -> Self::Output {
        self * other.0
    }
}

mul_assign!(<NonNegF32> for f32);

impl Mul<PosF32> for NonNegF32 {
    type Output = NonNegF32;

    fn mul(self, other: PosF32) -> Self::Output {
        // Note we need to deal with sub-normals
        non_neg_f32!(self.0 * other)
    }
}

mul_assign!(<PosF32> for NonNegF32);

impl Mul<NonNegF32> for PosF32 {
    type Output = NonNegF32;

    fn mul(self, other: NonNegF32) -> Self::Output {
        // Note we need to deal with sub-normals
        non_neg_f32!(self * other.0)
    }
}

impl Div for NonNegF32 {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        // IEEE-754 specifies that a positive number divided by a positive zero
        // is positive infinity, so since we know both are non-negative, we know
        // the quotient is as well. At least with the exception of `0.0 / 0.0`.
        // Just for convenicence's sake, we would like this operation to be closed
        // since the naive version is nearly so. Therefore, we just define 
        // `0.0 / 0.0` to be positive infinity. If you want a mathematical 
        // justification for choosing this value, observe that the graph of 
        // `f(x) = a/x` for positive `a` approaches infinity as `x` goes to `0` and
        // it continues to do so as `a` approaches `0` as well. A pragmatic 
        // justification for this value is that `0.0 / 0.0` is likely to be an error
        // and a value of infinity demands more attention than say `0.0`.

        // Given the choice to map `0.0 / 0.0` to infinity, we can say that we get 
        // infinity for all cases where `other` is zero. Given we know that `other`
        // cannot be `-0.0` we can optimize a bit by not engaging the floating point
        // hardware to check for `0.0`, since we know the bit representation for 
        // `0.0` is the same as `0u32`.
        if other.0.to_bits() == 0 {
            NonNegF32::INFINITY
        } else {
            // Note we need to deal with sub-normals
            non_neg_f32!(self.0 / other.0)
        }
    }
}

impl Div<f32> for NonNegF32 {
    type Output = f32;

    fn div(self, other: f32) -> Self::Output {
        self.0 / other
    }
}

impl Div<NonNegF32> for f32 {
    type Output = f32;

    fn div(self, other: NonNegF32) -> Self::Output {
        self / other.0
    }
}

div_assign!(<NonNegF32> for f32);

impl Div<PosF32> for NonNegF32 {
    type Output = NonNegF32;

    fn div(self, other: PosF32) -> Self::Output {
        // 0.0 / PosF32::MIN == 0.0
        // Note we need to deal with sub-normals
        non_neg_f32!(self.0 / other.get())
    }
}

div_assign!(<PosF32> for NonNegF32);

impl Div<NonNegF32> for PosF32 {
    type Output = PosF32;

    fn div(self, other: NonNegF32) -> Self::Output {
        // PosF32::MIN / 0.0 == PosF32::INFINITY
        // Note we need to deal with sub-normals
        pos_f32!(self.get() / other.0)
    }
}

div_assign!(<NonNegF32> for PosF32);

impl SaturatingSub for NonNegF32 {
    type Output = NonNegF32;

    fn saturating_sub(self, rhs: NonNegF32) -> Self::Output {
        non_neg_f32!(self.0 - rhs.0)
    }
}

impl SaturatingSub<PosF32> for NonNegF32 {
    type Output = NonNegF32;

    fn saturating_sub(self, rhs: PosF32) -> Self::Output {
        non_neg_f32!(self.0 - rhs.get())
    }
}

impl SaturatingSub<NonNegF32> for PosF32 {
    // It seems like it makes more sense to have 
    // `pos_f32!(1.0).saturating_sub(non_neg_f32!(1.0))`
    // be NonNegF32::ZERO instead of PosF32::MIN.
    type Output = NonNegF32; 

    fn saturating_sub(self, rhs: NonNegF32) -> Self::Output {
        non_neg_f32!(self.get() - rhs.0)
    }
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;