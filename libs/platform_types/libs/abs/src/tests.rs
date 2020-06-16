#![cfg_attr(feature = "pub_arb", allow(dead_code))]
#![cfg_attr(feature = "pub_arb", allow(unused_macros))]
#![cfg_attr(feature = "pub_arb", allow(unused_imports))]

use super::*;

use proptest::prelude::{Strategy, proptest};
use proptest::num;

use macros::{d};

pub mod arb {
    use super::*;
    use proptest::test_runner::{TestRunner};
    use proptest::strategy::{Strategy, NewTree, ValueTree};
    use proptest::num::i64;

    #[derive(Clone, Copy, Debug)]
    enum Kind {
        Full,
        Quarter
    }

    #[derive(Clone, Copy, Debug)]
    #[must_use = "strategies do nothing unless used"]
    struct PosStrat(Kind);

    struct PosValueTree(Kind, <i64::Any as Strategy>::Tree);

    impl ValueTree for PosValueTree {
        type Value = Pos;
        fn current(&self) -> Self::Value {
            let pos = Pos::from_bits(self.1.current());

            match self.0 {
                Kind::Full => pos,
                Kind::Quarter => Pos(pos.0 / 4),
            }
        }
        fn simplify(&mut self) -> bool {
            self.1.simplify()
        }
        fn complicate(&mut self) -> bool {
            self.1.complicate()
        }
    }

    impl Strategy for PosStrat {
        type Tree = PosValueTree;
        type Value = Pos;

        fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
            i64::ANY
                .new_tree(runner)
                .map(|t| PosValueTree(self.0, t))
        }
    }

    pub fn abs_pos() -> impl Strategy<Value = Pos> + Copy {
        PosStrat(Kind::Full)
    }

    pub fn abs_pos_quarter() -> impl Strategy<Value = Pos> + Copy {
        PosStrat(Kind::Quarter)
    }

    #[derive(Clone, Copy, Debug)]
    #[must_use = "strategies do nothing unless used"]
    struct LengthStrat(Kind);

    struct LengthValueTree(Kind, <i64::Any as Strategy>::Tree);

    impl ValueTree for LengthValueTree {
        type Value = Length;
        fn current(&self) -> Self::Value {
            let pos = Pos::from_bits(self.1.current());

            Length::new_saturating(
                match self.0 {
                    Kind::Full => pos,
                    Kind::Quarter => Pos(pos.0 / 4),
                }.abs()
            )
        }
        fn simplify(&mut self) -> bool {
            self.1.simplify()
        }
        fn complicate(&mut self) -> bool {
            self.1.complicate()
        }
    }

    impl Strategy for LengthStrat {
        type Tree = LengthValueTree;
        type Value = Length;

        fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
            i64::ANY
                .new_tree(runner)
                .map(|t| LengthValueTree(self.0, t))
        }
    }

    pub fn abs_length() -> impl Strategy<Value = Length> + Copy {
        LengthStrat(Kind::Full)
    }

    pub fn abs_length_quarter() -> impl Strategy<Value = Length> + Copy {
        LengthStrat(Kind::Quarter)
    }
}

// These operations not being inverses when expressed in f32 was the initial reason 
// for this library to exist.
fn a_space_to_b(
    delta: Pos,
    base: Pos,
    a_space_point: Pos
) -> Pos {
    (a_space_point - delta) + base
}

fn b_space_to_a(
    b_space_point: Pos,
    base: Pos,
    delta: Pos
) -> Pos {
    delta + (b_space_point - base)
}

proptest!{
    #[test]
    fn a_to_b_to_a_is_identity_if_all_values_are_small_enough(
        delta in arb::abs_pos_quarter(),
        base in arb::abs_pos_quarter(),
        a_space_point in arb::abs_pos_quarter(),
    ) {
        let actual = b_space_to_a(
            a_space_to_b(
                delta,
                base,
                a_space_point,
            ),
            base,
            delta,
        );

        assert_eq!(
            actual,
            a_space_point,
            "delta {:?}, base {:?}, a_space_point {:?}",
            delta,       
            base,
            a_space_point,
        );
    }
}

fn b_to_a_to_b_is_identity_on(
    delta: Pos,
    base: Pos,
    b_space_point: Pos,
) {
    let intermediate = b_space_to_a(
        b_space_point,
        base,
        delta,
    );

    let actual = a_space_to_b(
        delta,
        base,
        intermediate
    );

    assert_eq!(
        actual,
        b_space_point,
        "delta {:?}, base {:?}, b_space_point {:?}, intermediate {:?}",
        delta,       
        base,
        b_space_point,
        intermediate,
    );
}

proptest!{
    #[test]
    fn b_to_a_to_b_is_identity_if_all_values_are_small_enough(
        delta in arb::abs_pos_quarter(),
        base in arb::abs_pos_quarter(),
        b_space_point in arb::abs_pos_quarter(),
    ) {
        b_to_a_to_b_is_identity_on(
            delta,
            base,
            b_space_point,
        );
    }
}

#[test]
fn b_to_a_to_b_is_identity_in_this_maximum_point_case() {
    b_to_a_to_b_is_identity_on(
        d!(),
        d!(),
        Pos::from_f32(f32::INFINITY),
    );
}

proptest!{
    #[test]
    fn adding_default_is_identity(
        pos in arb::abs_pos(),
    ) {
        let actual = pos + Pos::default();
        assert_eq!(actual, pos);

        let actual = Pos::default() + pos;
        assert_eq!(actual, pos);
    }
}

proptest!{
    #[test]
    fn subtracting_default_is_identity(
        pos in arb::abs_pos(),
    ) {
        let actual = pos - Pos::default();
        assert_eq!(actual, pos);
    }
}

proptest!{
    #[test]
    fn b_space_to_a_with_defaults_is_identity(
        pos in arb::abs_pos(),
    ) {
        let actual = b_space_to_a(pos, d!(), d!());
        assert_eq!(actual, pos);
    }
}

proptest!{
    #[test]
    fn b_space_to_a_with_defaults_is_identity_gen_float(
        pos in proptest::num::f32::ANY,
    ) {
        let actual = b_space_to_a(pos.into(), d!(), d!());
        assert_eq!(actual, Pos::from(pos));
    }
}

#[test]
fn b_space_to_a_with_defaults_is_identity_generated_reduction() {
    let pos = -51382920.0;
    
    let actual: Pos = Pos::default() + (Pos::from(pos) - Pos::default());
    assert_eq!(actual, pos);
}

#[test]
fn b_space_to_a_with_defaults_is_identity_for_inf() {
    let pos = Pos::from(f32::INFINITY);
    
    let actual: Pos = Pos::default() + (pos - Pos::default());
    assert_eq!(actual, pos);
}

#[test]
fn abs_pos_to_f32_and_f32_to_abs_pos_do_not_change_the_signum_numbers() {
    assert_eq!(Pos::from(-1.0f32), Pos::NEGATIVE_ONE, "Pos::from -1");
    assert_eq!(f32::from(Pos::NEGATIVE_ONE), -1.0f32, "f32::from -1");

    assert_eq!(Pos::from(0.0f32), Pos::ZERO, "Pos::from 0");
    assert_eq!(f32::from(Pos::ZERO), 0.0f32, "f32::from 0");

    assert_eq!(Pos::from(1.0f32), Pos::ONE, "Pos::from 1");
    assert_eq!(f32::from(Pos::ONE), 1.0f32, "f32::from 1");
}

// We're okay with stuff truncating to 0, but something positive becoming negative,
// is not desired.
proptest!{
    #[test]
    fn abs_pos_to_f32_to_abs_pos_does_not_flip_the_signum(
        pos in arb::abs_pos(),
    ) {
        let expected = pos.signum();

        let f = f32::from(pos);
        if expected == Pos::ONE {
            assert_ne!(f32_signum(f), -1.0);
        } else if expected == Pos::NEGATIVE_ONE {
            assert_ne!(f32_signum(f), 1.0);
        }
        
        let round_tripped = Pos::from(f);

        if expected == Pos::ONE {
            assert_ne!(round_tripped.signum(), Pos::NEGATIVE_ONE);
        } else if expected == Pos::NEGATIVE_ONE {
            assert_ne!(round_tripped.signum(), Pos::ONE);
        }
    }
}

fn f32_to_abs_pos_to_f32_does_not_change_the_signum_on(
    f: f32,
) {
    let expected = f32_signum(f);

    let pos = Pos::from(f);

    if expected == 1.0 {
        assert_ne!(
            pos.signum(),
            Pos::NEGATIVE_ONE,
            "converting {} (f32) to {} (Pos) made the signum negative",
            f,
            pos
        );
    } else if expected == -1.0 {
        assert_ne!(
            pos.signum(),
            Pos::ONE,
            "converting {} (f32) to {} (Pos) made the signum positive",
            f,
            pos
        );
    }

    let round_tripped = f32::from(pos);

    if expected == 1.0 {
        assert_ne!(f32_signum(round_tripped), -1.0);
    } else if expected == -1.0 {
        assert_ne!(f32_signum(round_tripped), 1.0);
    }
}

proptest!{
    #[test]
    fn f32_to_abs_pos_to_f32_does_not_change_the_signum(
        f in proptest::num::f32::NORMAL,
    ) {
        f32_to_abs_pos_to_f32_does_not_change_the_signum_on(
            f
        )
    }
}

#[test]
fn f32_to_abs_pos_to_f32_does_not_change_the_signum_on_zero(
) {
    f32_to_abs_pos_to_f32_does_not_change_the_signum_on(
        0.0
    )
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
