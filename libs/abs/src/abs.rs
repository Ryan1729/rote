//! This crate contains the following types:
//! * `Pos`, a type representing Absolute Position.
//! * `Length`, a type representing Length, which is usually measured absolutely.
//! * `Ratio`, a type which allows scaling Length values.
#![deny(unconditional_recursion)]
use macros::{
    fmt_debug,
    fmt_display,
    add_assign,
    sub_assign,
    mul_assign,
    div_assign,
    u
};
use std::ops::{Add, Sub, Mul, Div, Neg, Not};

type PosInner = i64;

#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos(PosInner);

fmt_debug!(
    for Pos: Pos(bits) in "{} ({})",
    Pos(*bits).to_string(),
    bits
);

#[allow(clippy::cast_precision_loss)]
fn lossy_to_f64(bits: i64) -> f64 {
    // It's just for display purposes.
    (bits & Pos::FRAC_BIT_MASK) as f64
        / Pos::SCALE as f64
}

fmt_display!(
    for Pos: Pos(bits) in "{}{}",
    bits >> Pos::SCALE_BIT_COUNT,
    (lossy_to_f64(*bits))
    .to_string().trim_start_matches('0')
);

impl Pos {
    const SCALE_BIT_COUNT: u8 = 32;
    const SCALE: i64 = 1_i64 << Self::SCALE_BIT_COUNT as i64;
    // Since `SCALE` is a power of 2, which is below `f32::MAX`
    // there isn't actually any precision loss in this case.
    #[allow(clippy::cast_precision_loss)]
    const SCALE_F32: f32 = Self::SCALE as f32;
    const FRAC_BIT_MASK: i64 = Self::SCALE - 1;
    const TRUNC_BIT_MASK: i64 = !Self::FRAC_BIT_MASK;

    pub const MIN: Pos = Pos(-i64::max_value());
    pub const NEGATIVE_ONE: Pos = Pos(-Self::SCALE);
    pub const MAX_NEGATIVE: Pos = Pos(-1);
    pub const ZERO: Pos = Pos(0);
    pub const MIN_POSITIVE: Pos = Pos(1);
    pub const ONE_HALF: Pos = Pos(1_i64 << (Self::SCALE_BIT_COUNT - 1));
    pub const ONE: Pos = Pos(Self::SCALE);
    pub const TWO: Pos = Pos(Self::SCALE << 1);
    pub const FOUR: Pos = Pos(Self::SCALE << 2);
    pub const FIVE: Pos = Pos((Self::SCALE << 2) + Self::SCALE);
    pub const SEVEN: Pos = Pos((Self::SCALE << 3) - Self::SCALE);
    pub const ONE_TWENTY_EIGHT: Pos = Pos(Self::SCALE << 7);
    pub const TWO_FIFTY_SIX: Pos = Pos(Self::SCALE << 8);

    pub const TWO_TO_THE_TWENTY_THREE: Pos = Pos(Self::SCALE << 23);
    pub const MAX: Pos = Pos(i64::max_value());
    // This is actually slightly higher than `i64::max_value()`, so comparing
    // to this for saturation purposes gives the correct answer.
    #[allow(clippy::cast_precision_loss)]
    const MAX_F64: f64 = Self::MAX.0 as f64;


    #[must_use]
    pub fn from_f32(f: f32) -> Self {
        u!{std::num::FpCategory}
        let scaled = f * Self::SCALE_F32;

        match scaled.classify() {
            Normal => {
                if f64::from(scaled) >= Self::MAX_F64 {
                    Self::MAX
                } else {
                    // I'm pretty sure the above `if` check prevents the truncation.
                    #[allow(clippy::cast_possible_truncation)]
                    let bits = scaled as i64;

                    Self::from_bits(bits)
                }
            },
            Infinite => if scaled == f32::INFINITY {
                Self::MAX
            } else {
                Self::MIN
            },
            Zero | Subnormal | Nan => Self::ZERO,
        }
    }

    #[must_use]
    // We know it's lossy.
    #[allow(clippy::cast_precision_loss)]
    pub fn to_f32_lossy(&self) -> f32 {
        self.0 as f32 / Self::SCALE_F32
    }

    /// This method exists for ease of switching between this and f32 wrappers
    /// like `F32_0_1` that have a method with the same signature.
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

    #[perf_viz::record]
    #[must_use]
    pub fn trunc_to_i32(&self) -> i32 {
        (self.0 >> Self::SCALE_BIT_COUNT) as i32
    }

    // TODO do `double` and `halve` on positions actually make sense?
    // Should we be converting to `Length`s instead or something?
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
        Self::from_bits(self.0.saturating_sub(1))
    }

    #[must_use]
    pub fn minimal_increase(&self) -> Self {
        Self::from_bits(self.0.saturating_add(1))
    }

    #[must_use]
    pub fn from_bits(bits: i64) -> Self {
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
        $crate::Pos::from_f32($f32)
    };
    () => {
        $crate::Pos::default()
    }
}

impl From<u32> for Pos {
    fn from(n: u32) -> Self {
        Pos(PosInner::from(n) << Self::SCALE_BIT_COUNT)
    }
}

impl From<f32> for Pos {
    fn from(f: f32) -> Self {
        Pos::from_f32(f)
    }
}

impl From<&Pos> for f32 {
    fn from(a_p: &Pos) -> Self {
        From::from(*a_p)
    }
}

impl From<Pos> for f32 {
    fn from(a_p: Pos) -> Self {
        a_p.to_f32_lossy()
    }
}

impl PartialEq<f32> for Pos {
    fn eq(&self, other: &f32) -> bool {
        *self == Pos::from_f32(*other)
    }
}

impl PartialEq<Pos> for f32 {
    fn eq(&self, other: &Pos) -> bool {
        Pos::from_f32(*self) == *other
    }
}

impl PartialOrd<f32> for Pos {
    fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&Pos::from(*other))
    }
}

impl PartialOrd<Pos> for f32 {
    fn partial_cmp(&self, other: &Pos) -> Option<std::cmp::Ordering> {
        Pos::from(*self).partial_cmp(other)
    }
}

impl Add<f32> for Pos {
    type Output = Pos;

    fn add(self, other: f32) -> Pos {
        self + Pos::from(other)
    }
}

add_assign!(<f32> for Pos);

impl Add<Pos> for f32 {
    type Output = Pos;

    fn add(self, other: Pos) -> Pos {
        Pos::from(self) + other

    }
}

impl Sub<f32> for Pos {
    type Output = Pos;

    fn sub(self, other: f32) -> Pos {
        self.add(-other)
    }
}

sub_assign!(<f32> for Pos);

impl Sub<Pos> for f32 {
    type Output = Pos;

    fn sub(self, other: Pos) -> Pos {
        self.add(-other)
    }
}

impl Add<Pos> for Pos {
    type Output = Pos;

    fn add(self, other: Pos) -> Self::Output {
        Pos::from_bits(self.0.saturating_add(other.0))
    }
}

add_assign!(<Pos> for Pos);

impl Sub<Pos> for Pos {
    type Output = Vector;

    fn sub(self, other: Pos) -> Self::Output {
        Vector(Pos::from_bits(self.0.saturating_sub(other.0)))
    }
}

impl Neg for Pos {
    type Output = Pos;

    fn neg(self) -> Self::Output {
        Pos::from_bits(if self.0 == i64::MIN {
            i64::MAX
        } else {
            -self.0
        })
    }
}

/// A type that represents the displacement of a `Pos`. Has the same ragnge of
/// values as `Pos`.
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Vector(Pos);

fmt_debug!(
    for Vector: Vector(pos) in "{:?}",
    pos
);
fmt_display!(
    for Vector: Vector(pos) in "{}",
    pos
);

impl From<u32> for Vector {
    fn from(n: u32) -> Self {
        Vector(Pos::from(n))
    }
}

impl From<f32> for Vector {
    fn from(f: f32) -> Self {
        Vector(Pos::from_f32(f))
    }
}

impl From<&Vector> for f32 {
    fn from(v: &Vector) -> Self {
        From::from(*v)
    }
}

impl From<Vector> for f32 {
    fn from(v: Vector) -> Self {
        v.0.to_f32_lossy()
    }
}

impl From<Length> for Vector {
    fn from(l: Length) -> Self {
        Self(l.0)
    }
}

impl PartialEq<f32> for Vector {
    fn eq(&self, other: &f32) -> bool {
        self.0 == Pos::from_f32(*other)
    }
}

impl PartialOrd<f32> for Vector {
    fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&Vector::from(*other))
    }
}

impl PartialEq<Vector> for f32 {
    fn eq(&self, other: &Vector) -> bool {
        Pos::from_f32(*self) == other.0
    }
}

impl PartialOrd<Vector> for f32 {
    fn partial_cmp(&self, other: &Vector) -> Option<std::cmp::Ordering> {
        Vector::from(*self).partial_cmp(other)
    }
}

impl PartialEq<Length> for Vector {
    fn eq(&self, other: &Length) -> bool {
        self.0 == other.0
    }
}

impl PartialOrd<Length> for Vector {
    fn partial_cmp(&self, other: &Length) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&Vector::from(*other))
    }
}

impl PartialEq<Vector> for Length {
    fn eq(&self, other: &Vector) -> bool {
        self.0 == other.0
    }
}

impl PartialOrd<Vector> for Length {
    fn partial_cmp(&self, other: &Vector) -> Option<std::cmp::Ordering> {
        Vector::from(*self).partial_cmp(other)
    }
}

impl Add<Vector> for Pos {
    type Output = Pos;

    fn add(self, other: Vector) -> Pos {
        Pos::from_bits(self.0.saturating_add(other.0.0))
    }
}

add_assign!(<Vector> for Pos);

impl Add<Pos> for Vector {
    type Output = Pos;

    fn add(self, other: Pos) -> Pos {
        Pos::from_bits(self.0.0.saturating_add(other.0))
    }
}

impl Add<Vector> for Vector {
    type Output = Vector;

    fn add(self, other: Vector) -> Self::Output {
        Vector(self.0 + other)
    }
}

add_assign!(<Vector> for Vector);

impl Add<Length> for Vector {
    type Output = Vector;

    fn add(self, other: Length) -> Self::Output {
        Vector(self.0 + other)
    }
}

add_assign!(<Length> for Vector);

impl Add<Vector> for Length {
    type Output = Vector;

    fn add(self, other: Vector) -> Self::Output {
        Vector(self.0 + other)
    }
}

impl Sub<Vector> for Pos {
    type Output = Pos;

    fn sub(self, other: Vector) -> Pos {
        Pos::from_bits(self.0.saturating_sub(other.0.0))
    }
}

sub_assign!(<Vector> for Pos);

impl Sub<Vector> for Vector {
    type Output = Vector;

    fn sub(self, other: Vector) -> Self::Output {
        Vector(self.0 - other)
    }
}

sub_assign!(<Vector> for Vector);

impl Sub<Length> for Vector {
    type Output = Vector;

    fn sub(self, other: Length) -> Self::Output {
        Vector(self.0 - other)
    }
}

sub_assign!(<Length> for Vector);

impl Vector {
    pub const ZERO: Self = Self(Pos::ZERO);
    pub const ONE: Self = Self(Pos::ONE);

    pub const TWO_TO_THE_TWENTY_THREE: Self = Self(Pos::TWO_TO_THE_TWENTY_THREE);
    pub const MAX: Self = Self(Pos::MAX);

    #[must_use]
    pub fn from_bits(bits: i64) -> Self {
        Self(Pos::from_bits(bits))
    }

    #[allow(unused)]
    #[must_use]
    const fn to_bits(self) -> i64 {
        (self.0).0
    }
}

/// A type that represents the length of something, which since negative lengths
// do not make sense, can never be negative. Has the same maximum values as `Pos`
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Length(Pos);

fmt_debug!(
    for Length: Length(pos) in "{:?}",
    pos
);
fmt_display!(
    for Length: Length(pos) in "{}",
    pos
);

impl Length {
    pub const MIN: Length = Length(Pos::ZERO);
    pub const ZERO: Length = Self::MIN;
    pub const MIN_POSITIVE: Length = Length(Pos::MIN_POSITIVE);
    pub const ONE_HALF: Length = Length(Pos::ONE_HALF);
    pub const ONE: Length = Length(Pos::ONE);
    pub const TWO: Length = Length(Pos::TWO);
    pub const FOUR: Length = Length(Pos::FOUR);
    pub const FIVE: Length = Length(Pos::FIVE);
    pub const SEVEN: Length = Length(Pos::SEVEN);
    pub const ONE_TWENTY_EIGHT: Length = Length(Pos::ONE_TWENTY_EIGHT);
    pub const TWO_FIFTY_SIX: Length = Length(Pos::TWO_FIFTY_SIX);
    pub const MAX: Length = Length(Pos::MAX);

    #[must_use]
    pub fn new_saturating(p: Pos) -> Self {
        if p < Self::MIN.0 {
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
    pub fn trunc_to_u32(&self) -> u32 {
        // We maintain elsewhere that `Length`s cannot be negative.
        #[allow(clippy::cast_sign_loss)]
        let bits = (self.0.0 >> Pos::SCALE_BIT_COUNT) as u32;

        bits
    }

    #[must_use]
    pub fn scale_saturating(&self, ratio: Ratio) -> Self {
        let payload = ratio.0;

        Self::from_bits(if payload >= 0 {
            self.to_bits()
                .saturating_mul(
                    // 0 maps to 1, 1 maps to 2
                    i64::from(payload.saturating_add(1))
                )
        } else if payload > Ratio::ZERO_BITS {
            self.to_bits()
                // -1 maps to 2, -2 maps to 3
                / i64::from((-payload).saturating_add(1))
        } else {
            0
        })
    }

    #[must_use]
    pub fn double(&self) -> Self {
        self.scale_saturating(Ratio::TWO)
    }

    #[must_use]
    pub fn halve(&self) -> Self {
        self.scale_saturating(Ratio::HALF)
    }

    #[must_use]
    pub fn minimal_decrease(&self) -> Self {
        Self::new_saturating(self.0.minimal_decrease())
    }

    #[must_use]
    pub fn minimal_increase(&self) -> Self {
        Self::new_saturating(self.0.minimal_increase())
    }

    #[must_use]
    pub fn from_bits(bits: i64) -> Self {
        Self(Pos::from_bits(bits))
    }

    #[must_use]
    const fn to_bits(self) -> i64 {
        (self.0).0
    }

    #[must_use]
    pub fn from_vector_saturating(vector: Vector) -> Self {
        Self::new_saturating(vector.0)
    }
}

#[macro_export]
macro_rules! abs_length {
    ($abs_pos: expr $(,)?) => {
        $crate::Length::new_saturating($abs_pos)
    };
    () => {
        $crate::Length::default()
    }
}

impl From<u32> for Length {
    fn from(n: u32) -> Self {
        Length(Pos::from(n))
    }
}

impl From<Length> for Pos {
    fn from(p: Length) -> Self {
        p.0
    }
}

impl From<Pos> for Length {
    fn from(p: Pos) -> Self {
        Length::new_saturating(p)
    }
}

impl From<f32> for Length {
    fn from(f: f32) -> Self {
        Length::new_saturating(f.into())
    }
}

impl From<&Length> for f32 {
    fn from(p_a_p: &Length) -> Self {
        From::from(*p_a_p)
    }
}

impl From<Length> for f32 {
    fn from(p_a_p: Length) -> Self {
        Pos::from(p_a_p).to_f32_lossy()
    }
}

impl PartialEq<Length> for Pos {
    fn eq(&self, other: &Length) -> bool {
        *self == other.0
    }
}

impl PartialEq<Pos> for Length {
    fn eq(&self, other: &Pos) -> bool {
        self.0 == *other
    }
}

impl PartialOrd<Length> for Pos {
    fn partial_cmp(&self, other: &Length) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&Pos::from(*other))
    }
}

impl PartialOrd<Pos> for Length {
    fn partial_cmp(&self, other: &Pos) -> Option<std::cmp::Ordering> {
        Pos::from(*self).partial_cmp(other)
    }
}

impl Add<Length> for Pos {
    type Output = Pos;

    fn add(self, other: Length) -> Pos {
        self + Pos::from(other)
    }
}

add_assign!(<Length> for Pos);

impl Add<Pos> for Length {
    type Output = Pos;

    fn add(self, other: Pos) -> Pos {
        Pos::from(self) + other

    }
}

impl Sub<Length> for Pos {
    type Output = Pos;

    fn sub(self, other: Length) -> Pos {
        Pos(self.0.saturating_sub(other.0.0))
    }
}

sub_assign!(<Length> for Pos);

impl Add<Length> for Length {
    type Output = Length;

    fn add(self, other: Length) -> Self::Output {
        Length(self.0 + other.0)
    }
}

add_assign!(<Length> for Length);

impl Sub<Length> for Length {
    type Output = Length;

    fn sub(self, other: Length) -> Self::Output {
        Length::new_saturating(self.0 - other)
    }
}

sub_assign!(<Length> for Length);

type RatioBits = i32;

/// Ratio allows scaling a `Length` by a restricted set of values.
#[derive(Copy, Clone, Debug)]
// Internal details:
//  0 means 1/1
//  1 means 2/1
// -1 means 1/2
//
pub struct Ratio(RatioBits);

impl Ratio {
    pub const ZERO: Ratio = Ratio(Self::ZERO_BITS);
    pub const TWO_FIFTY_SIXTH: Ratio = Ratio(-255);
    pub const ONE_TWENTY_EIGHTH: Ratio = Ratio(-127);
    pub const SIXTY_FOURTH: Ratio = Ratio(-63);
    pub const THIRTY_SECONDTH: Ratio = Ratio(-31);
    pub const SIXTEENTH: Ratio = Ratio(-15);
    pub const EIGHTH: Ratio = Ratio(-7);
    pub const QUARTER: Ratio = Ratio(-3);
    pub const HALF: Ratio = Ratio(-1);
    pub const ONE: Ratio = Ratio(0);
    pub const TWO: Ratio = Ratio(1);
    pub const THREE: Ratio = Ratio(2);
    pub const FOUR: Ratio = Ratio(3);
    pub const FIVE: Ratio = Ratio(4);
    pub const SIX: Ratio = Ratio(5);
    pub const SEVEN: Ratio = Ratio(6);
    pub const EIGHT: Ratio = Ratio(7);
    pub const SIXTEEN: Ratio = Ratio(15);
    pub const THIRTY_TWO: Ratio = Ratio(31);
    pub const SIXTY_FOUR: Ratio = Ratio(63);
    pub const ONE_TWENTY_EIGHT: Ratio = Ratio(127);
    pub const TWO_FIFTY_SIX: Ratio = Ratio(255);

    const ZERO_BITS: RatioBits = RatioBits::min_value();

    #[must_use]
    pub fn from_bits(bits: RatioBits) -> Self {
        Self(
            bits
        )
    }

    #[must_use]
    pub fn from_u16_saturating(x: u16) -> Self {
        if x == 0 {
            Self::ZERO
        } else {
            Self::from_bits(RatioBits::from(x - 1))
        }
    }
}

impl Mul<Ratio> for Length {
    type Output = Length;

    fn mul(self, other: Ratio) -> Self::Output {
        self.scale_saturating(other)
    }
}
mul_assign!(<Ratio> for Length);

impl Mul<Length> for Ratio {
    type Output = Length;

    fn mul(self, other: Length) -> Self::Output {
        other.scale_saturating(self)
    }
}

impl Div<Ratio> for Length {
    type Output = Length;

    fn div(self, other: Ratio) -> Self::Output {
        self.scale_saturating(!other)
    }
}
div_assign!(<Ratio> for Length);

impl Not for Ratio {
    type Output = Ratio;

    fn not(self) -> Self::Output {
        if self.0 == Self::ZERO_BITS {
            // This seems reasonable since it makes dividing by zero give zero,
            // which seems like the least unreasonable value.
            Self::ZERO
        } else {
            Ratio(-self.0)
        }
    }
}

impl From<usize> for Ratio {
    fn from(x: usize) -> Self {
        // This check avoids the trucation and saturates instead.
        #[allow(clippy::cast_possible_truncation)]
        let x = if x < u16::MAX as usize {
            x as _
        } else {
            u16::MAX
        };

        Ratio::from_u16_saturating(x)
    }
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;