//! This crate contains `AbsPos`, a type representing Absolute Position.
use macros::{add_assign, sub_assign, mul_assign, div_assign, d};
use std::ops::{Add, Sub, Mul, Div, Neg};

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct AbsPos{
}

#[macro_export]
macro_rules! abs_pos {
    () => {
        AbsPos::default()
    }
}

impl Add<f32> for AbsPos {
    type Output = AbsPos;

    fn add(self, other: f32) -> AbsPos {
        d!()
    }
}

impl Add<AbsPos> for f32 {
    type Output = AbsPos;

    fn add(self, other: AbsPos) -> AbsPos {
        d!()
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
        d!()
    }
}

impl Sub<AbsPos> for AbsPos {
    type Output = f32;

    fn sub(self, other: AbsPos) -> f32 {
        d!()
    }
}

impl Neg for AbsPos {
    type Output = AbsPos;

    fn neg(self) -> Self::Output {
        d!()
    }
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;