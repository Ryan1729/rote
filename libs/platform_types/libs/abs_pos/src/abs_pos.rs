//! This crate contains `AbsPos`, a type representing Absolute Position.
#![deny(unconditional_recursion)]
use macros::{fmt_debug, fmt_display, add_assign, sub_assign, u};
use std::ops::{Add, Sub, Neg};

#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct AbsPos(i64);

fmt_debug!(
    for AbsPos: AbsPos(bits) in "{} ({})", 
    AbsPos(*bits).to_string(),
    bits
);
fmt_display!(
    for AbsPos: AbsPos(bits) in "{}{}", 
    bits >> AbsPos::SCALE_BIT_COUNT, 
    (
        (bits & AbsPos::FRAC_BIT_MASK) as f64 
        / AbsPos::SCALE as f64
    )
    .to_string().trim_start_matches("0")
);

impl AbsPos {
    const SCALE_BIT_COUNT: u64 = 32;
    const SCALE: i64 = 1i64 << Self::SCALE_BIT_COUNT;
    const SCALE_F32: f32 = Self::SCALE as f32;
    const FRAC_BIT_MASK: i64 = Self::SCALE - 1;
    const TRUNC_BIT_MASK: i64 = !Self::FRAC_BIT_MASK;

    pub const MIN: AbsPos = AbsPos(-i64::max_value());
    pub const NEGATIVE_ONE: AbsPos = AbsPos(-Self::SCALE);
    pub const MAX_NEGATIVE: AbsPos = AbsPos(-1);
    pub const ZERO: AbsPos = AbsPos(0);
    pub const MIN_POSITIVE: AbsPos = AbsPos(1);
    pub const ONE_HALF: AbsPos = AbsPos(1i64 << (Self::SCALE_BIT_COUNT - 1));
    pub const ONE: AbsPos = AbsPos(Self::SCALE);
    pub const MAX: AbsPos = AbsPos(i64::max_value());


    #[must_use]
    pub fn from_f32(f: f32) -> Self {
        u!{std::num::FpCategory}
        let scaled = f * Self::SCALE_F32;

        match scaled.classify() {
            Normal => Self::from_bits(scaled as i64),
            Infinite => if scaled == f32::INFINITY {
                Self::MAX
            } else {
                Self::MIN
            },
            Zero | Subnormal | Nan => Self::ZERO,
        }
    }

    #[must_use]
    pub fn to_f32_lossy(&self) -> f32 {
        self.0 as f32 / Self::SCALE_F32
    }

    /// This method exists for ease of switching between this and f32 wrappers
    /// like F32_0_1 that have a method with the same signature.
    // TODO make this a trait? Why not just use `Into<f32>`?
    #[must_use]
    pub fn get(&self) -> f32 {
        self.to_f32_lossy()
    }

    #[must_use]
    pub fn abs(&self) -> Self {
        let me = *self;
        if me < Self::ZERO {
            -me
        } else {
            me
        }
    }

    #[must_use]
    pub fn signum(&self) -> Self {
        let me = *self;
        if me >= Self::MIN_POSITIVE {
            Self::ONE
        } else if me <= Self::MAX_NEGATIVE {
            Self::NEGATIVE_ONE
        } else {
            Self::ZERO
        }
    }

    #[must_use]
    pub fn frac(&self) -> Self {
        Self(self.0 & Self::FRAC_BIT_MASK)
    }

    #[must_use]
    pub fn trunc(&self) -> Self {
        Self(self.0 & Self::TRUNC_BIT_MASK)
    }

    #[must_use]
    pub fn trunc_to_i32(&self) -> i32 {
        (self.0 >> Self::SCALE_BIT_COUNT) as i32
    }

    #[must_use]
    pub fn double(&self) -> Self {
        Self::from_bits(self.0.saturating_mul(2))
    }

    #[must_use]
    pub fn halve(&self) -> Self {
        Self::from_bits(self.0 >> 1)
    }

    #[must_use]
    pub fn minimal_decrease(&self) -> Self {
        Self::from_bits(self.0 - 1)
    }

    #[must_use]
    fn from_bits(bits: i64) -> Self {
        Self(
            std::cmp::max(
                bits,
                Self::MIN.0
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

add_assign!(<f32> for AbsPos);

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

sub_assign!(<f32> for AbsPos);

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

add_assign!(<AbsPos> for AbsPos);

impl Sub<AbsPos> for AbsPos {
    type Output = AbsPos;

    fn sub(self, other: AbsPos) -> Self::Output {
        AbsPos::from_bits(self.0.saturating_sub(other.0))
    }
}

sub_assign!(<AbsPos> for AbsPos);

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

/// A wrapper around AbsPos that ensures the value is always positive, by clamping
/// any non-positive numbers to AbsPos::MIN_POSITIVE.
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PosAbsPos(AbsPos);

fmt_debug!(
    for PosAbsPos: PosAbsPos(pos) in "{:?}",
    pos
);
fmt_display!(
    for PosAbsPos: PosAbsPos(pos) in "{}",
    pos
);

impl PosAbsPos {
    pub const MIN: PosAbsPos = PosAbsPos(AbsPos::MIN_POSITIVE);
    pub const MIN_POSITIVE: PosAbsPos = Self::MIN;
    pub const ONE_HALF: PosAbsPos = PosAbsPos(AbsPos::ONE_HALF);
    pub const ONE: PosAbsPos = PosAbsPos(AbsPos::ONE);
    pub const MAX: PosAbsPos = PosAbsPos(AbsPos::MAX);

    pub fn new_saturating(p: AbsPos) -> Self {
        if p < AbsPos::MIN_POSITIVE {
            Self::MIN
        } else {
            Self(p)
        }
    }

    #[must_use]
    pub fn get(&self) -> f32 {
        self.0.get()
    }

    #[must_use]
    pub fn frac(&self) -> Self {
        Self(self.0.frac())
    }

    #[must_use]
    pub fn trunc(&self) -> Self {
        Self(self.0.trunc())
    }

    #[must_use]
    pub fn trunc_to_i32(&self) -> i32 {
        self.0.trunc_to_i32()
    }

    #[must_use]
    pub fn double(&self) -> Self {
        Self(self.0.double())
    }

    #[must_use]
    pub fn halve(&self) -> Self {
        Self(self.0.halve())
    }

    #[must_use]
    pub fn minimal_decrease(&self) -> Self {
        Self::new_saturating(self.0.minimal_decrease())
    }
}

#[macro_export]
macro_rules! pos_abs_pos {
    ($abs_pos: expr $(,)?) => {
        PosAbsPos::new_saturating($abs_pos)
    };
    () => {
        PosAbsPos::default()
    }
}

impl From<PosAbsPos> for AbsPos {
    fn from(p: PosAbsPos) -> Self {
        p.0
    }
}

impl From<f32> for PosAbsPos {
    fn from(f: f32) -> Self {
        PosAbsPos::new_saturating(f.into())
    }
}

impl From<PosAbsPos> for f32 {
    fn from(p_a_p: PosAbsPos) -> Self {
        AbsPos::from(p_a_p).to_f32_lossy()
    }
}

impl PartialEq<PosAbsPos> for AbsPos {
    fn eq(&self, other: &PosAbsPos) -> bool {
        *self == other.0
    }
}

impl PartialEq<AbsPos> for PosAbsPos {
    fn eq(&self, other: &AbsPos) -> bool {
        self.0 == *other
    }
}

impl PartialOrd<PosAbsPos> for AbsPos {
    fn partial_cmp(&self, other: &PosAbsPos) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&AbsPos::from(*other))
    }
}

impl PartialOrd<AbsPos> for PosAbsPos {
    fn partial_cmp(&self, other: &AbsPos) -> Option<std::cmp::Ordering> {
        AbsPos::from(*self).partial_cmp(other)
    }
}

impl Add<PosAbsPos> for AbsPos {
    type Output = AbsPos;

    fn add(self, other: PosAbsPos) -> AbsPos {
        self + AbsPos::from(other)
    }
}

add_assign!(<PosAbsPos> for AbsPos);

impl Add<AbsPos> for PosAbsPos {
    type Output = AbsPos;

    fn add(self, other: AbsPos) -> AbsPos {
        AbsPos::from(self) + other
        
    }
}

impl Sub<PosAbsPos> for AbsPos {
    type Output = AbsPos;

    fn sub(self, other: PosAbsPos) -> AbsPos {
        self - other.0
    }
}

sub_assign!(<PosAbsPos> for AbsPos);

impl Sub<AbsPos> for PosAbsPos {
    type Output = AbsPos;

    fn sub(self, other: AbsPos) -> AbsPos {
        self.0 - other
    }
}

impl Add<PosAbsPos> for PosAbsPos {
    type Output = PosAbsPos;

    fn add(self, other: PosAbsPos) -> Self::Output {
        PosAbsPos(self.0 + other.0)
    }
}

add_assign!(<PosAbsPos> for PosAbsPos);

impl Sub<PosAbsPos> for PosAbsPos {
    type Output = AbsPos;

    fn sub(self, other: PosAbsPos) -> Self::Output {
        self.0 - other.0
    }
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;