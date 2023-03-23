//! This module is called `f32_0_1` and the main type is called `F32_0_1` 
//! but we actually restrict the values of `F32_0_1` to be positive, normal (that is 
//! non-sub-normal), values between 0.o and 1.0 inclusive. The smallest non-zero 
//! value allowed is `f32::MIN_POSITIVE`.
use macros::{add_assign, sub_assign, mul_assign, div_assign, SaturatingSub};
use non_neg_f32::{NonNegF32, non_neg_f32};
use pos_f32::{PosF32, pos_f32};
use std::ops::{Add, Sub, Mul, Div};

#[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct F32_0_1(f32);

impl F32_0_1 {
    pub const MIN: F32_0_1 = Self::ZERO;
    pub const ZERO: F32_0_1 = F32_0_1(0.0);
    pub const ONE_HALF: F32_0_1 = F32_0_1(0.5);
    pub const ONE: F32_0_1 = F32_0_1(1.0);
    pub const MAX: F32_0_1 = Self::ONE;
}

#[macro_export]
macro_rules! is_0_or_above {
    ($float: expr) => {{
        let f: f32 = $float;
        // `is_sign_positive` is needed to eliminate -0.0
        (f == 0.0 && f.is_sign_positive()) || f >= f32::MIN_POSITIVE
    }};
}

#[macro_export]
macro_rules! is_1_or_below {
    ($float: expr) => {{
        $float <= 1.0
    }};
}

#[macro_export]
macro_rules! is_0_1 {
    ($float: expr) => {{
        let f: f32 = $float;
        // `is_sign_positive` is needed to eliminate -0.0
        is_0_or_above!(f) && is_1_or_below!(f)
    }};
}

#[macro_export]
macro_rules! f32_0_1 {
    ($float: literal) => {{
        // This doesn't work on stable as of this writing
        // const assertion 
        // #[allow(unknown_lints, eq_op)]
        // const _: [(); 0 - !{ $crate::is_non_neg!($float) } as usize] = [];

        F32_0_1::new_saturating($float)
    }};
    ($float: expr) => {
        F32_0_1::new_saturating($float)
    };
}

impl F32_0_1 {
    #[must_use]
    pub fn new_saturating(f: f32) -> Self {
        Self(if is_0_or_above!(f) {
            if is_1_or_below!(f) {
                f
            } else {
                1.0
            }
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

impl From<F32_0_1> for f32 {
    fn from(F32_0_1(f): F32_0_1) -> Self {
        f
    }
}

impl From<PosF32> for F32_0_1 {
    fn from(p: PosF32) -> Self {
        F32_0_1(p.get())
    }
}

impl PartialEq<f32> for F32_0_1 {
    fn eq(&self, other: &f32) -> bool {
        self.0 == *other
    }
}

impl PartialEq<F32_0_1> for f32 {
    fn eq(&self, other: &F32_0_1) -> bool {
        *self == other.0
    }
}

impl PartialEq<PosF32> for F32_0_1 {
    fn eq(&self, other: &PosF32) -> bool {
        self.0 == other.get()
    }
}

impl PartialEq<F32_0_1> for PosF32 {
    fn eq(&self, other: &F32_0_1) -> bool {
        self.get() == other.0
    }
}

impl PartialOrd<f32> for F32_0_1 {
    fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl PartialOrd<F32_0_1> for f32 {
    fn partial_cmp(&self, other: &F32_0_1) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.0)
    }
}

impl PartialOrd<PosF32> for F32_0_1 {
    fn partial_cmp(&self, other: &PosF32) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.get())
    }
}

impl PartialOrd<F32_0_1> for PosF32 {
    fn partial_cmp(&self, other: &F32_0_1) -> Option<std::cmp::Ordering> {
        self.get().partial_cmp(&other.0)
    }
}

impl Add for F32_0_1 {
    type Output = NonNegF32;

    fn add(self, other: Self) -> NonNegF32 {
        // Since we know both are non-negative, we know the sum is as well.
        non_neg_f32!(self.0 + other.0)
    }
}

impl Add<f32> for F32_0_1 {
    type Output = f32;

    fn add(self, other: f32) -> Self::Output {
        self.0 + other
    }
}

impl Add<F32_0_1> for f32 {
    type Output = f32;

    fn add(self, other: F32_0_1) -> Self::Output {
        self + other.0
    }
}

add_assign!(<F32_0_1> for f32);

impl Add<PosF32> for F32_0_1 {
    type Output = PosF32;

    fn add(self, other: PosF32) -> Self::Output {
        pos_f32!(self.0 + other.get())
    }
}

impl Add<F32_0_1> for PosF32 {
    type Output = PosF32;

    fn add(self, other: F32_0_1) -> Self::Output {
        pos_f32!(self.get() + other.0)
    }
}

add_assign!(<F32_0_1> for PosF32);

impl Sub for F32_0_1 {
    type Output = f32;

    fn sub(self, other: Self) -> Self::Output {
        // Even though we know both are non-negative, the difference may not be.
        self.0 - other.0
    }
}

impl Sub<f32> for F32_0_1 {
    type Output = f32;

    fn sub(self, other: f32) -> Self::Output {
        self.0 - other
    }
}

impl Sub<F32_0_1> for f32 {
    type Output = f32;

    fn sub(self, other: F32_0_1) -> Self::Output {
        self - other.0
    }
}

sub_assign!(<F32_0_1> for f32);

impl Sub<PosF32> for F32_0_1 {
    type Output = f32;

    fn sub(self, other: PosF32) -> Self::Output {
        self.0 - other.get()
    }
}

impl Sub<F32_0_1> for PosF32 {
    type Output = f32;

    fn sub(self, other: F32_0_1) -> Self::Output {
        self.get() - other.0
    }
}

impl Mul for F32_0_1 {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        // Since we know both are 0-1, we know the product is as well.
        // Note we need to deal with sub-normals
        f32_0_1!(self.0 * other.0)
    }
}

mul_assign!(for F32_0_1);

impl Mul<f32> for F32_0_1 {
    type Output = f32;

    fn mul(self, other: f32) -> Self::Output {
        self.0 * other
    }
}

impl Mul<F32_0_1> for f32 {
    type Output = f32;

    fn mul(self, other: F32_0_1) -> Self::Output {
        self * other.0
    }
}

mul_assign!(<F32_0_1> for f32);

impl Mul<PosF32> for F32_0_1 {
    type Output = NonNegF32;

    fn mul(self, other: PosF32) -> Self::Output {
        // Note we need to deal with sub-normals
        non_neg_f32!(self.0 * other)
    }
}

impl Mul<F32_0_1> for PosF32 {
    type Output = NonNegF32;

    fn mul(self, other: F32_0_1) -> Self::Output {
        // Note we need to deal with sub-normals
        non_neg_f32!(self * other.0)
    }
}

impl Mul<NonNegF32> for F32_0_1 {
    type Output = NonNegF32;

    fn mul(self, other: NonNegF32) -> Self::Output {
        // Note we need to deal with sub-normals
        non_neg_f32!(self.0 * other)
    }
}

impl Mul<F32_0_1> for NonNegF32 {
    type Output = NonNegF32;

    fn mul(self, other: F32_0_1) -> Self::Output {
        // Note we need to deal with sub-normals
        non_neg_f32!(self * other.0)
    }
}

mul_assign!(<F32_0_1> for NonNegF32);

impl Div for F32_0_1 {
    type Output = NonNegF32;

    fn div(self, other: Self) -> Self::Output {
        // IEEE-754 specifies that a positive number divided by a positive zero
        // is positive infinity, so since we know both are non-negative, we know
        // the quotient is as well. At least with the exception of `0.0 / 0.0`.
        // Just for convenicence's sake, we would like this operation to be closed
        // over NonNegF32 since the naive version is nearly so. Therefore, we just 
        // define `0.0 / 0.0` to be positive infinity. If you want a mathematical 
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

impl Div<f32> for F32_0_1 {
    type Output = f32;

    fn div(self, other: f32) -> Self::Output {
        self.0 / other
    }
}

impl Div<F32_0_1> for f32 {
    type Output = f32;

    fn div(self, other: F32_0_1) -> Self::Output {
        self / other.0
    }
}

div_assign!(<F32_0_1> for f32);

impl Div<PosF32> for F32_0_1 {
    type Output = NonNegF32;

    fn div(self, other: PosF32) -> Self::Output {
        // 0.0 / PosF32::MIN == 0.0
        // Note we need to deal with sub-normals
        non_neg_f32!(self.0 / other.get())
    }
}

impl Div<F32_0_1> for PosF32 {
    type Output = PosF32;

    fn div(self, other: F32_0_1) -> Self::Output {
        // PosF32::MIN / 0.0 == PosF32::INFINITY
        // Note we need to deal with sub-normals
        pos_f32!(self.get() / other.0)
    }
}

div_assign!(<F32_0_1> for PosF32);

impl SaturatingSub for F32_0_1 {
    type Output = F32_0_1;

    fn saturating_sub(self, rhs: F32_0_1) -> Self::Output {
        f32_0_1!(self.0 - rhs.0)
    }
}

impl SaturatingSub<F32_0_1> for PosF32 {
    // It seems like it makes more sense to have 
    // `pos_f32!(1.0).saturating_sub(non_neg_f32!(1.0))`
    // be NonNegF32::ZERO instead of PosF32::MIN.
    type Output = NonNegF32; 

    fn saturating_sub(self, rhs: F32_0_1) -> Self::Output {
        non_neg_f32!(self.get() - rhs.0)
    }
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;