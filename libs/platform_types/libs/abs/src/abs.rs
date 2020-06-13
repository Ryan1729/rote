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

#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos(i64);

fmt_debug!(
    for Pos: Pos(bits) in "{} ({})", 
    Pos(*bits).to_string(),
    bits
);
fmt_display!(
    for Pos: Pos(bits) in "{}{}", 
    bits >> Pos::SCALE_BIT_COUNT, 
    (
        (bits & Pos::FRAC_BIT_MASK) as f64 
        / Pos::SCALE as f64
    )
    .to_string().trim_start_matches("0")
);

impl Pos {
    const SCALE_BIT_COUNT: u64 = 32;
    const SCALE: i64 = 1i64 << Self::SCALE_BIT_COUNT;
    const SCALE_F32: f32 = Self::SCALE as f32;
    const FRAC_BIT_MASK: i64 = Self::SCALE - 1;
    const TRUNC_BIT_MASK: i64 = !Self::FRAC_BIT_MASK;

    pub const MIN: Pos = Pos(-i64::max_value());
    pub const NEGATIVE_ONE: Pos = Pos(-Self::SCALE);
    pub const MAX_NEGATIVE: Pos = Pos(-1);
    pub const ZERO: Pos = Pos(0);
    pub const MIN_POSITIVE: Pos = Pos(1);
    pub const ONE_HALF: Pos = Pos(1i64 << (Self::SCALE_BIT_COUNT - 1));
    pub const ONE: Pos = Pos(Self::SCALE);
    pub const TWO: Pos = Pos(Self::SCALE << 1);
    pub const FOUR: Pos = Pos(Self::SCALE << 2);
    pub const ONE_TWENTY_EIGHT: Pos = Pos(Self::SCALE << 7);
    pub const TWO_FIFTY_SIX: Pos = Pos(Self::SCALE << 8);
    pub const MAX: Pos = Pos(i64::max_value());


    #[must_use]
    pub fn from_f32(f: f32) -> Self {
        u!{std::num::FpCategory}
        let scaled = f * Self::SCALE_F32;

        dbg!(scaled, scaled as i64);

        match scaled.classify() {
            Normal => {
                if scaled as f64 >= Self::MAX.0 as f64 {
                    Self::MAX
                } else {
                    Self::from_bits(scaled as i64)
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
        $crate::Pos::from_f32($f32)
    };
    () => {
        $crate::Pos::default()
    }
}

impl From<f32> for Pos {
    fn from(f: f32) -> Self {
        Pos::from_f32(f)
    }
}

impl From<Pos> for f32 {
    fn from(a_p: Pos) -> Self {
        a_p.to_f32_lossy()
    }
}

impl PartialEq<f32> for Pos {
    fn eq(&self, other: &f32) -> bool {
        *self == Pos::from(*other)
    }
}

impl PartialEq<Pos> for f32 {
    fn eq(&self, other: &Pos) -> bool {
        Pos::from(*self) == *other
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
    type Output = Pos;

    fn sub(self, other: Pos) -> Self::Output {
        Pos::from_bits(self.0.saturating_sub(other.0))
    }
}

sub_assign!(<Pos> for Pos);

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

/// A type that represents the length of something, which since negative lengths
// do not make sense, can never be negative. Has te same maximum valus as `Pos`
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
    pub const ONE_TWENTY_EIGHT: Length = Length(Pos::ONE_TWENTY_EIGHT);
    pub const TWO_FIFTY_SIX: Length = Length(Pos::TWO_FIFTY_SIX);
    pub const MAX: Length = Length(Pos::MAX);
    

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
    pub fn scale_saturating(&self, ratio: Ratio) -> Self {
        let payload = ratio as i8;

        if payload >= 0 {
            Self::from_bits(self.to_bits().saturating_mul(1i64 << payload))
        } else {
            Self::from_bits(self.to_bits() >> (-payload))
        }
    }

    #[must_use]
    pub fn double(&self) -> Self {
        self.scale_saturating(Ratio::Two)
    }

    #[must_use]
    pub fn halve(&self) -> Self {
        self.scale_saturating(Ratio::Half)
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
    fn from_bits(bits: i64) -> Self {
        Self(Pos::from_bits(bits))
    }

    #[must_use]
    const fn to_bits(self) -> i64 {
        (self.0).0
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

impl From<Length> for Pos {
    fn from(p: Length) -> Self {
        p.0
    }
}

impl From<f32> for Length {
    fn from(f: f32) -> Self {
        Length::new_saturating(f.into())
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
        self - other.0
    }
}

sub_assign!(<Length> for Pos);

impl Sub<Pos> for Length {
    type Output = Pos;

    fn sub(self, other: Pos) -> Pos {
        self.0 - other
    }
}

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
        Length::new_saturating(self.0 - other.0)
    }
}

/// Ratio allows scaling a langth by a restricted set of values.
#[derive(Copy, Clone, Debug)]
#[repr(i8)]
pub enum Ratio {
    TwoFiftySixth = -8,
    OneTwentyEighth = -7,
    SixtyFourth = -6,
    ThirtySecondth = -5,
    Sixteenth = -4,
    Eighth = -3,
    Fourth = -2,
    Half = -1,
    One = 0,
    Two = 1,
    Four = 2,
    Eight = 3,
    Sixteen = 4,
    ThirtyTwo = 5,
    SixtyFour = 6,
    OneTwentyEight = 7,
    TwoFiftySix = 8,
}

impl Ratio {
    fn from_i8_saturating(x: i8) -> Self {
        u!{Ratio}
        match x {
          std::i8::MIN..=-8i8 => TwoFiftySixth,
          -7 => OneTwentyEighth,
          -6 => SixtyFourth,
          -5 => ThirtySecondth,
          -4 => Sixteenth,
          -3 => Eighth,
          -2 => Fourth,
          -1 => Half,
          0 => One,
          1 => Two,
          2 => Four,
          3 => Eight,
          4 => Sixteen,
          5 => ThirtyTwo,
          6 => SixtyFour,
          7 => OneTwentyEight,
          8i8..=std::i8::MAX => TwoFiftySix,
        }
    }

    fn scale_saturating(self, other: Ratio) -> Ratio {
        Self::from_i8_saturating(self as i8 + other as i8)
    }
}

impl Mul<Ratio> for Ratio {
    type Output = Ratio;

    fn mul(self, other: Ratio) -> Self::Output {
        self.scale_saturating(other)
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

impl Div<Ratio> for Ratio {
    type Output = Ratio;

    fn div(self, other: Ratio) -> Self::Output {
        self.scale_saturating(!other)
    }
}

impl Div<Ratio> for Length {
    type Output = Length;

    fn div(self, other: Ratio) -> Self::Output {
        self.scale_saturating(!other)
    }
}
div_assign!(<Ratio> for Length);

impl Div<Length> for Ratio {
    type Output = Length;

    fn div(self, other: Length) -> Self::Output {
        other.scale_saturating(!self)
    }
}

impl Not for Ratio {
    type Output = Ratio;

    fn not(self) -> Self::Output {
        Ratio::from_i8_saturating(-(self as i8))
    }
}


/// The default signum returns 1.0 for 0.0, where we want 0.0.
fn f32_signum(f: f32) -> f32 {
    let no_zero = f.signum();
    if no_zero == 1.0 && f == 0.0 {
        0.0
    } else {
        // NaN ends up here
        no_zero
    }
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;