//! This crate contains `AbsPos`, a type representing Absolute Position.
#![deny(unconditional_recursion)]
use macros::{fmt_display, add_assign, sub_assign, mul_assign, div_assign, d, u};
use std::ops::{Add, Sub, Mul, Div, Neg};

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct AbsPos(i64);

fmt_display!(for AbsPos: AbsPos(bits) in "{}.{}", bits >> 32, bits & 0xFFFF_FFFF);

impl AbsPos {
    const SCALE: f32 = (1u64 << 32) as f32;

    pub fn from_f32(f: f32) -> Self {
        u!{std::num::FpCategory}
        let scaled = f * Self::SCALE;

        Self::from_bits(
            match scaled.classify() {
                Normal => scaled as i64,
                Infinite => if scaled == f32::INFINITY {
                    i64::MAX
                } else {
                    i64::MIN
                },
                Zero | Subnormal | Nan => 0,
            },
        )
    }

    pub fn to_f32_lossy(&self) -> f32 {
        self.0 as f32 / Self::SCALE
    }

    fn from_bits(bits: i64) -> Self {
        Self(
            std::cmp::max(
                bits,
                i64::MIN + 1
            )
        )
    }
}

#[macro_export]
macro_rules! abs_pos {
    ($f32: expr $(,)?) => {
        AbsPos::from_f32($f32)
    };
    () => {
        AbsPos::default()
    }
}

impl From<f32> for AbsPos {
    fn from(f: f32) -> Self {
        AbsPos::from_f32(f)
    }
}

impl From<AbsPos> for f32 {
    fn from(a_p: AbsPos) -> Self {
        a_p.to_f32_lossy()
    }
}

impl PartialEq<f32> for AbsPos {
    fn eq(&self, other: &f32) -> bool {
        *self == AbsPos::from(*other)
    }
}

impl PartialEq<AbsPos> for f32 {
    fn eq(&self, other: &AbsPos) -> bool {
        AbsPos::from(*self) == *other
    }
}

impl PartialOrd<f32> for AbsPos {
    fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&AbsPos::from(*other))
    }
}

impl PartialOrd<AbsPos> for f32 {
    fn partial_cmp(&self, other: &AbsPos) -> Option<std::cmp::Ordering> {
        AbsPos::from(*self).partial_cmp(other)
    }
}

impl Add<f32> for AbsPos {
    type Output = AbsPos;

    fn add(self, other: f32) -> AbsPos {
        self + AbsPos::from(other)
    }
}

impl Add<AbsPos> for f32 {
    type Output = AbsPos;

    fn add(self, other: AbsPos) -> AbsPos {
        AbsPos::from(self) + other
        
    }
}

impl Sub<f32> for AbsPos {
    type Output = AbsPos;

    fn sub(self, other: f32) -> AbsPos {
        self.add(-other)
    }
}

impl Sub<AbsPos> for f32 {
    type Output = AbsPos;

    fn sub(self, other: AbsPos) -> AbsPos {
        self.add(-other)
    }
}

impl Add<AbsPos> for AbsPos {
    type Output = AbsPos;

    fn add(self, other: AbsPos) -> Self::Output {
        AbsPos::from_bits(self.0.saturating_add(other.0))
    }
}

impl Sub<AbsPos> for AbsPos {
    type Output = AbsPos;

    fn sub(self, other: AbsPos) -> Self::Output {
        AbsPos::from_bits(self.0.saturating_sub(other.0))
    }
}

impl Neg for AbsPos {
    type Output = AbsPos;

    fn neg(self) -> Self::Output {
        AbsPos::from_bits(if self.0 == i64::MIN {
            i64::MAX
        } else {
            -self.0
        })
    }
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;