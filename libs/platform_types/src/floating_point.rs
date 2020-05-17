const SIGN_BIT: u32 = 0x8000_0000;
const ALL_BUT_SIGN_BIT: u32 = 0x7fff_ffff;

/// Assumes x is one of the "usual" `f32`s, AKA not sub/denormal, Infinity or NaN.
/// So normal numbers or 0. This does not imply that the output is a usual f32.
pub fn usual_f32_minimal_increase<F32: Into<f32>>(x: F32) -> f32 {
    let x = x.into();
    let non_sign_bits = x.to_bits() & ALL_BUT_SIGN_BIT;
    // if is 0 or -0
    if non_sign_bits == 0 {
        std::f32::MIN_POSITIVE
    } else {
        let sign_bit = x.to_bits() & SIGN_BIT;
        let sign = if sign_bit == 0 { 1 } else { -1 };
        f32::from_bits(sign_bit | (non_sign_bits as i32 + sign) as u32)
    }
}

/// Assumes x is one of the "usual" `f32`s, AKA not sub/denormal, Infinity or NaN.
/// So normal numbers or 0. This does not imply that the output is a usual f32.
pub fn usual_f32_minimal_decrease<F32: Into<f32>>(x: F32) -> f32 {
    let x = x.into();
    let non_sign_bits = x.to_bits() & ALL_BUT_SIGN_BIT;
    // if is 0 or -0
    if non_sign_bits == 0 {
        -std::f32::MIN_POSITIVE
    } else {
        let sign_bit = x.to_bits() & SIGN_BIT;
        let sign = if sign_bit == 0 { 1 } else { -1 };
        f32::from_bits(sign_bit | (non_sign_bits as i32 - sign) as u32)
    }
}

pub fn is_normal_or_0<F32: Into<f32>>(x: F32) -> bool {
    let x = x.into();
    use std::num::FpCategory::{Normal, Zero};
    let category = x.classify();
    category == Zero || category == Normal
}

/// returns the next largest floating point number if the input is normal or 0
/// and the ouput would be normal or 0. Otherwise, the input is returned.
pub fn next_largest_f32_if_normal_or_0<F32: Into<f32>>(x: F32) -> f32 {
    let x = x.into();
    if is_normal_or_0(x) {
        let larger = usual_f32_minimal_increase(x);
        if is_normal_or_0(larger) {
            return larger;
        }
    }
    x
}

/// returns the next smallest floating point number if the input is normal or 0
/// and the ouput would be normal or 0. Otherwise, the input is returned.
pub fn next_smallest_f32_if_normal_or_0<F32: Into<f32>>(x: F32) -> f32 {
    let x = x.into();
    if is_normal_or_0(x) {
        let smaller = usual_f32_minimal_decrease(x);
        if is_normal_or_0(smaller) {
            return smaller;
        }
    }
    x
}

#[cfg(test)]
mod floating_point_tests {
    use super::*;
    use crate::tests::arb;
    use proptest::num::f32;
    use proptest::proptest;
    use std::f32::{MAX, MIN, MIN_POSITIVE};
    use std::num::FpCategory::{Infinite, Normal, Zero};
    use pub_arb_std::usual;
    

    // note, the following tests demostates that prop testing is not the same thing as testing every
    // case!
    proptest! {
        #[test]
        fn usual_f32_minimal_increase_outputs_usual_f32s(
            x in usual(),
        ) {
            let category = usual_f32_minimal_increase(x).classify();
            assert!(
                category == Zero || category == Normal,
                "category was {:?}, not Zero or Normal",
                category
            );
        }
    }
    proptest! {
        #[test]
        fn usual_f32_minimal_decrease_outputs_usual_f32s(
            x in usual(),
        ) {
            let category = usual_f32_minimal_decrease(x).classify();
            assert!(
                category == Zero || category == Normal,
                "category was {:?}, not Zero or Normal",
                category
            );
        }
    }
    proptest! {
        #[test]
        fn usual_f32_minimal_increase_increases(
            old in usual(),
        ) {
            let new = usual_f32_minimal_increase(old);
            assert!(
                new > old,
                "{:?} <= {:?}\n{:b} <= {:b}",
                new,
                old,
                new.to_bits(),
                old.to_bits()
            );
        }
    }
    proptest! {
        #[test]
        fn usual_f32_minimal_decrease_decreases(
            old in usual(),
        ) {
            let new = usual_f32_minimal_decrease(old);
            assert!(
                new < old,
                "{:?} >= {:?}\n{:b} >= {:b}",
                new,
                old,
                new.to_bits(),
                old.to_bits()
            );
        }
    }
    #[test]
    fn how_usual_f32_minimal_decrease_works_at_the_edge() {
        // precondition
        assert_eq!(Normal, MIN.classify());
        let category = usual_f32_minimal_decrease(MIN).classify();
        assert_eq!(Infinite, category);
    }
    #[test]
    fn how_usual_f32_minimal_increase_works_at_the_edge() {
        // precondition
        assert_eq!(Normal, MAX.classify());
        let category = usual_f32_minimal_increase(MAX).classify();
        assert_eq!(Infinite, category);
    }

    #[test]
    fn how_usual_f32_minimal_decrease_works_around_zero() {
        assert_eq!(usual_f32_minimal_decrease(0.0), -MIN_POSITIVE);
    }
    #[test]
    fn how_usual_f32_minimal_increase_works_around_zero() {
        assert_eq!(usual_f32_minimal_increase(0.0), MIN_POSITIVE);
    }
}
