//! This module restrict the values of `PosF32Trunc` to be positive, integer values
//! by using the `trunc` method on f32s

use macros::{d, add_assign, sub_assign, mul_assign, div_assign};
use std::ops::{Add, Sub, Mul, Div};

use pos_f32::{PosF32, pos_f32};
use non_neg_f32::{NonNegF32, non_neg_f32};

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct PosF32Trunc(f32);

d!(for PosF32Trunc: PosF32Trunc::ONE);

impl PosF32Trunc {
    pub const ONE: PosF32Trunc = PosF32Trunc(1.0);
    pub const INFINITY: PosF32Trunc = PosF32Trunc(f32::INFINITY);

    /// These were added on an as-needed basis, and arguably are only need to
    /// be here because as of this writing `new_saturating` cannot be const-eval'd
    pub const TWO: PosF32Trunc = PosF32Trunc(2.0);
    pub const FOUR: PosF32Trunc = PosF32Trunc(4.0);
    pub const ONE_HUNDRED_TWENTY_EIGHT: PosF32Trunc = PosF32Trunc(128.0);
}

#[macro_export]
macro_rules! is_at_least_one {
    ($float: expr) => {{
        // This returns false for NaNs
        $float >= 1.0
    }};
}

#[macro_export]
macro_rules! pos_f32_trunc {
    ($float: literal) => {{
        // const assertion 
        #[allow(unknown_lints, eq_op)]
        const _: [(); 0 - !{ $crate::is_at_least_one!($float) } as usize] = [];

        $crate::PosF32Trunc::new_saturating($float)
    }};
    ($float: expr) => {
        $crate::PosF32Trunc::new_saturating($float)
    };
}

impl PosF32Trunc {
    pub fn new_saturating(f: f32) -> Self {
        Self(if is_at_least_one!(f) {
            f.trunc()
        } else {
            // NaN ends up here
            1.0
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

impl From<PosF32Trunc> for f32 {
    fn from(PosF32Trunc(f): PosF32Trunc) -> Self {
        f
    }
}

impl From<PosF32Trunc> for PosF32 {
    fn from(PosF32Trunc(f): PosF32Trunc) -> Self {
        pos_f32!(f)
    }
}

impl PartialEq<f32> for PosF32Trunc {
    fn eq(&self, other: &f32) -> bool {
        self.0 == *other
    }
}

impl PartialEq<PosF32Trunc> for f32 {
    fn eq(&self, other: &PosF32Trunc) -> bool {
        *self == other.0
    }
}

impl PartialEq<PosF32> for PosF32Trunc {
    fn eq(&self, other: &PosF32) -> bool {
        self.0 == other.get()
    }
}

impl PartialEq<PosF32Trunc> for PosF32 {
    fn eq(&self, other: &PosF32Trunc) -> bool {
        self.get() == other.0
    }
}

impl PartialEq<NonNegF32> for PosF32Trunc {
    fn eq(&self, other: &NonNegF32) -> bool {
        self.0 == other.get()
    }
}

impl PartialEq<PosF32Trunc> for NonNegF32 {
    fn eq(&self, other: &PosF32Trunc) -> bool {
        self.get() == other.0
    }
}

impl PartialOrd<f32> for PosF32Trunc {
    fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl PartialOrd<PosF32Trunc> for f32 {
    fn partial_cmp(&self, other: &PosF32Trunc) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.0)
    }
}

impl PartialOrd<PosF32> for PosF32Trunc {
    fn partial_cmp(&self, other: &PosF32) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.get())
    }
}

impl PartialOrd<PosF32Trunc> for PosF32 {
    fn partial_cmp(&self, other: &PosF32Trunc) -> Option<std::cmp::Ordering> {
        self.get().partial_cmp(&other.0)
    }
}

impl PartialOrd<NonNegF32> for PosF32Trunc {
    fn partial_cmp(&self, other: &NonNegF32) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.get())
    }
}

impl PartialOrd<PosF32Trunc> for NonNegF32 {
    fn partial_cmp(&self, other: &PosF32Trunc) -> Option<std::cmp::Ordering> {
        self.get().partial_cmp(&other.0)
    }
}

impl Add for PosF32Trunc {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        // Since we know both are positive and whole, we know the sum is as well.
        PosF32Trunc(self.0 + other.0)
    }
}

add_assign!(for PosF32Trunc);

impl Add<f32> for PosF32Trunc {
    type Output = f32;

    fn add(self, other: f32) -> Self::Output {
        self.0 + other
    }
}

impl Add<PosF32Trunc> for f32 {
    type Output = f32;

    fn add(self, other: PosF32Trunc) -> Self::Output {
        self + other.0
    }
}

add_assign!(<PosF32Trunc> for f32);

impl Add<PosF32> for PosF32Trunc {
    type Output = PosF32;

    fn add(self, other: PosF32) -> Self::Output {
        pos_f32!(self.0 + other.get())
    }
}

impl Add<PosF32Trunc> for PosF32 {
    type Output = PosF32;

    fn add(self, other: PosF32Trunc) -> Self::Output {
        pos_f32!(self.get() + other.0)
    }
}

add_assign!(<PosF32Trunc> for PosF32);

impl Add<NonNegF32> for PosF32Trunc {
    type Output = PosF32;

    fn add(self, other: NonNegF32) -> Self::Output {
        pos_f32!(self.0 + other.get())
    }
}

impl Add<PosF32Trunc> for NonNegF32 {
    type Output = PosF32;

    fn add(self, other: PosF32Trunc) -> Self::Output {
        pos_f32!(self.get() + other.0)
    }
}

impl Sub for PosF32Trunc {
    type Output = f32;

    fn sub(self, other: Self) -> Self::Output {
        // Even though we know both are positive, the difference may not be.
        self.0 - other.0
    }
}

impl Sub<f32> for PosF32Trunc {
    type Output = f32;

    fn sub(self, other: f32) -> Self::Output {
        self.0 - other
    }
}

impl Sub<PosF32Trunc> for f32 {
    type Output = f32;

    fn sub(self, other: PosF32Trunc) -> Self::Output {
        self - other.0
    }
}

sub_assign!(<PosF32Trunc> for f32);

impl Sub<PosF32> for PosF32Trunc {
    type Output = f32;

    fn sub(self, other: PosF32) -> Self::Output {
        self.0 - other.get()
    }
}

impl Sub<PosF32Trunc> for PosF32 {
    type Output = f32;

    fn sub(self, other: PosF32Trunc) -> Self::Output {
        self.get() - other.0
    }
}

impl Sub<NonNegF32> for PosF32Trunc {
    type Output = f32;

    fn sub(self, other: NonNegF32) -> Self::Output {
        self.0 - other.get()
    }
}

impl Sub<PosF32Trunc> for NonNegF32 {
    type Output = f32;

    fn sub(self, other: PosF32Trunc) -> Self::Output {
        self.get() - other.0
    }
}

impl Mul for PosF32Trunc {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        // Since we know both are positive and whole, we know the product is as 
        // well.
        PosF32Trunc(self.0 * other.0)
    }
}

impl Mul<f32> for PosF32Trunc {
    type Output = f32;

    fn mul(self, other: f32) -> Self::Output {
        self.0 * other
    }
}

impl Mul<PosF32Trunc> for f32 {
    type Output = f32;

    fn mul(self, other: PosF32Trunc) -> Self::Output {
        self * other.0
    }
}

mul_assign!(<PosF32Trunc> for f32);

impl Mul<PosF32> for PosF32Trunc {
    type Output = PosF32;

    fn mul(self, other: PosF32) -> Self::Output {
        pos_f32!(self.0 * other.get())
    }
}

impl Mul<PosF32Trunc> for PosF32 {
    type Output = PosF32;

    fn mul(self, other: PosF32Trunc) -> Self::Output {
        pos_f32!(self.get() * other.0)
    }
}

mul_assign!(<PosF32Trunc> for PosF32);

impl Mul<NonNegF32> for PosF32Trunc {
    type Output = NonNegF32;

    fn mul(self, other: NonNegF32) -> Self::Output {
        non_neg_f32!(self.0 * other.get())
    }
}

impl Mul<PosF32Trunc> for NonNegF32 {
    type Output = NonNegF32;

    fn mul(self, other: PosF32Trunc) -> Self::Output {
        non_neg_f32!(self.get() * other.0)
    }
}

mul_assign!(<PosF32Trunc> for NonNegF32);

impl Div for PosF32Trunc {
    type Output = PosF32;

    fn div(self, other: Self) -> PosF32 {
        pos_f32!(self.0 / other.0)
    }
}

impl Div<f32> for PosF32Trunc {
    type Output = f32;

    fn div(self, other: f32) -> Self::Output {
        self.0 / other
    }
}

impl Div<PosF32Trunc> for f32 {
    type Output = f32;

    fn div(self, other: PosF32Trunc) -> Self::Output {
        self / other.0
    }
}

div_assign!(<PosF32Trunc> for f32);

impl Div<PosF32> for PosF32Trunc {
    type Output = PosF32;

    fn div(self, other: PosF32) -> Self::Output {
        pos_f32!(self.0 / other.get())
    }
}

impl Div<PosF32Trunc> for PosF32 {
    type Output = PosF32;

    fn div(self, other: PosF32Trunc) -> Self::Output {
        pos_f32!(self.get() / other.0)
    }
}

div_assign!(<PosF32Trunc> for PosF32);

impl Div<NonNegF32> for PosF32Trunc {
    type Output = PosF32;

    fn div(self, other: NonNegF32) -> Self::Output {
        pos_f32!(self.0 / other.get())
    }
}

impl Div<PosF32Trunc> for NonNegF32 {
    type Output = NonNegF32;

    fn div(self, other: PosF32Trunc) -> Self::Output {
        non_neg_f32!(self.get() / other.0)
    }
}

div_assign!(<PosF32Trunc> for NonNegF32);

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;