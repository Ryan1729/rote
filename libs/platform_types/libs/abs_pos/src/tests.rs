#![cfg_attr(feature = "pub_arb", allow(dead_code))]
#![cfg_attr(feature = "pub_arb", allow(unused_macros))]
#![cfg_attr(feature = "pub_arb", allow(unused_imports))]

use super::*;

use proptest::prelude::{Strategy, proptest};
use proptest::num;

pub mod arb {
    use super::*;
    use proptest::test_runner::{TestRunner};
    use proptest::strategy::{Strategy, NewTree, ValueTree};
    use proptest::num::f32;
    

    struct AbsPosValueTree(Kind, <f32::Any as Strategy>::Tree);

    impl ValueTree for AbsPosValueTree {
        type Value = AbsPos;
        fn current(&self) -> Self::Value {
            let pos = self.1.current().into();

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

    #[derive(Clone, Copy, Debug)]
    enum Kind {
        Full,
        Quarter
    }

    #[derive(Clone, Copy, Debug)]
    #[must_use = "strategies do nothing unless used"]
    struct AbsPosStrat(Kind);

    impl Strategy for AbsPosStrat {
        type Tree = AbsPosValueTree;
        type Value = AbsPos;

        fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
            f32::ANY
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