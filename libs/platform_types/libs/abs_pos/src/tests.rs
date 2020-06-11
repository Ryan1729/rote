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
    struct AbsPosStrat(Kind);

struct AbsPosValueTree(Kind, <i64::Any as Strategy>::Tree);

    impl ValueTree for AbsPosValueTree {
        type Value = AbsPos;
        fn current(&self) -> Self::Value {
            let pos = AbsPos::from_bits(self.1.current());

            match self.0 {
                Kind::Full => pos,
                Kind::Quarter => AbsPos(pos.0 / 4),
            }
        }
        fn simplify(&mut self) -> bool {
            self.1.simplify()
        }
        fn complicate(&mut self) -> bool {
            self.1.complicate()
        }
    }

    impl Strategy for AbsPosStrat {
        type Tree = AbsPosValueTree;
        type Value = AbsPos;

        fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
            i64::ANY
                .new_tree(runner)
                .map(|t| AbsPosValueTree(self.0, t))
        }
    }

    pub fn abs_pos() -> impl Strategy<Value = AbsPos> + Copy {
        AbsPosStrat(Kind::Full)
    }

    pub fn abs_pos_quarter() -> impl Strategy<Value = AbsPos> + Copy {
        AbsPosStrat(Kind::Quarter)
    }

    #[derive(Clone, Copy, Debug)]
    #[must_use = "strategies do nothing unless used"]
    struct PosAbsPosStrat(Kind);

    struct PosAbsPosValueTree(Kind, <i64::Any as Strategy>::Tree);

    impl ValueTree for PosAbsPosValueTree {
        type Value = PosAbsPos;
        fn current(&self) -> Self::Value {
            let pos = AbsPos::from_bits(self.1.current());

            PosAbsPos::new_saturating(
                match self.0 {
                    Kind::Full => pos,
                    Kind::Quarter => AbsPos(pos.0 / 4),
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

    impl Strategy for PosAbsPosStrat {
        type Tree = PosAbsPosValueTree;
        type Value = PosAbsPos;

        fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
            i64::ANY
                .new_tree(runner)
                .map(|t| PosAbsPosValueTree(self.0, t))
        }
    }

    pub fn pos_abs_pos() -> impl Strategy<Value = PosAbsPos> + Copy {
        PosAbsPosStrat(Kind::Full)
    }

    pub fn pos_abs_pos_quarter() -> impl Strategy<Value = PosAbsPos> + Copy {
        PosAbsPosStrat(Kind::Quarter)
    }
}

// These operations not being inverses when expressed in f32 was the initial reason 
// for this library to exist.
fn a_space_to_b(
    delta: AbsPos,
    base: AbsPos,
    a_space_point: AbsPos
) -> AbsPos {
    (a_space_point - delta) + base
}

fn b_space_to_a(
    b_space_point: AbsPos,
    base: AbsPos,
    delta: AbsPos
) -> AbsPos {
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
    delta: AbsPos,
    base: AbsPos,
    b_space_point: AbsPos,
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
        AbsPos::from_f32(f32::INFINITY),
    );
}

proptest!{
    #[test]
    fn adding_default_is_identity(
        pos in arb::abs_pos(),
    ) {
        let actual = pos + AbsPos::default();
        assert_eq!(actual, pos);

        let actual = AbsPos::default() + pos;
        assert_eq!(actual, pos);
    }
}

proptest!{
    #[test]
    fn subtracting_default_is_identity(
        pos in arb::abs_pos(),
    ) {
        let actual = pos - AbsPos::default();
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
        assert_eq!(actual, AbsPos::from(pos));
    }
}

#[test]
fn b_space_to_a_with_defaults_is_identity_generated_reduction() {
    let pos = -51382920.0;
    
    let actual: AbsPos = AbsPos::default() + (AbsPos::from(pos) - AbsPos::default());
    assert_eq!(actual, pos);
}

#[test]
fn b_space_to_a_with_defaults_is_identity_for_inf() {
    let pos = AbsPos::from(f32::INFINITY);
    
    let actual: AbsPos = AbsPos::default() + (pos - AbsPos::default());
    assert_eq!(actual, pos);
}

#[test]
fn abs_pos_to_f32_and_f32_to_abs_pos_do_not_change_the_signum_numbers() {
    assert_eq!(AbsPos::from(-1.0f32), AbsPos::NEGATIVE_ONE);
    assert_eq!(f32::from(AbsPos::NEGATIVE_ONE), -1.0f32);

    assert_eq!(AbsPos::from(0.0f32), AbsPos::ZERO);
    assert_eq!(f32::from(AbsPos::ZERO), 0.0f32);

    assert_eq!(AbsPos::from(1.0f32), AbsPos::ONE);
    assert_eq!(f32::from(AbsPos::ONE), 1.0f32);
}

proptest!{
    #[test]
    fn abs_pos_to_f32_to_abs_pos_does_not_change_the_signum(
        pos in arb::abs_pos(),
    ) {
        let expected = pos.signum();

        let f = f32::from(pos);
        assert_eq!(f.signum(), f32::from(expected));

        let round_tripped = AbsPos::from(f);
        assert_eq!(round_tripped.signum(), expected);   
    }
}

proptest!{
    #[test]
    fn f32_to_abs_pos_to_f32_does_not_change_the_signum(
        f in proptest::num::f32::ANY,
    ) {
        let expected = f.signum();

        let pos = AbsPos::from(f);
        assert_eq!(pos.signum(), AbsPos::from(expected));

        let round_tripped = f32::from(pos);
        assert_eq!(round_tripped.signum(), expected);   
    }
}
