//! This crate contains `AbsPos`, a type representing Absolute Position.
#![deny(unconditional_recursion)]
use macros::{add_assign, sub_assign, mul_assign, div_assign, d, u};
use std::ops::{Add, Sub, Mul, Div, Neg};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct AbsPos(i64);

impl AbsPos {
    const SCALE: f32 = (1u64 << 32) as f32;

    pub fn from_f32(f: f32) -> Self {
        u!{std::num::FpCategory}
        let scaled = f * Self::SCALE;

        Self(
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

impl Add<f32> for AbsPos {
    type Output = AbsPos;

    fn add(self, other: f32) -> AbsPos {
        AbsPos(
            self.0.saturating_add(AbsPos::from(other).0)
        )
    }
}

impl Add<AbsPos> for f32 {
    type Output = AbsPos;

    fn add(self, other: AbsPos) -> AbsPos {
        AbsPos(
            AbsPos::from(self).0.saturating_add(other.0)
        )
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
    type Output = f32;

    fn add(self, other: AbsPos) -> f32 {
        self.0.saturating_add(other.0) as f32
    }
}

impl Sub<AbsPos> for AbsPos {
    type Output = f32;

    fn sub(self, other: AbsPos) -> f32 {
        self.0.saturating_sub(other.0) as f32
    }
}

impl Neg for AbsPos {
    type Output = AbsPos;

    fn neg(self) -> Self::Output {
        AbsPos(if self.0 == i64::MIN {
            i64::MAX
        } else {
            -self.0
        })
    }
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;