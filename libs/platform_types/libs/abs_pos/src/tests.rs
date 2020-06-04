use super::*;

use proptest::prelude::{Strategy, proptest};
use proptest::num;

pub mod arb {
    use super::*;
    use proptest::test_runner::{TestRunner};
    use proptest::strategy::{Strategy, NewTree, ValueTree};
    use proptest::num::f32;
    

    struct AbsPosValueTree(<f32::Any as Strategy>::Tree);

    impl ValueTree for AbsPosValueTree {
        type Value = AbsPos;
        fn current(&self) -> Self::Value {
            self.0.current().into()
        }
        fn simplify(&mut self) -> bool {
            self.0.simplify()
        }
        fn complicate(&mut self) -> bool {
            self.0.complicate()
        }
    }

    #[derive(Clone, Copy, Debug)]
    #[must_use = "strategies do nothing unless used"]
    struct AbsPosStrat;

    impl Strategy for AbsPosStrat {
        type Tree = AbsPosValueTree;
        type Value = AbsPos;

        fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
            f32::ANY
                .new_tree(runner)
                .map(AbsPosValueTree)
        }
    }

    pub fn abs_pos() -> impl Strategy<Value = AbsPos> + Copy {
        AbsPosStrat
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
    fn a_to_b_to_a_is_identity(
        delta in arb::abs_pos(),
        base in arb::abs_pos(),
        a_space_point in arb::abs_pos(),
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
    fn b_to_a_to_b_is_identity(
        delta in arb::abs_pos(),
        base in arb::abs_pos(),
        b_space_point in arb::abs_pos(),
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
        assert_eq!(actual, pos.into());

        let actual = AbsPos::default() + pos;
        assert_eq!(actual, pos.into());
    }
}

proptest!{
    #[test]
    fn subtracting_default_is_identity(
        pos in arb::abs_pos(),
    ) {
        let actual = pos - AbsPos::default();
        assert_eq!(actual, pos.into());
    }
}

proptest!{
    #[test]
    fn b_space_to_a_with_defaults_is_identity(
        pos in arb::abs_pos(),
    ) {
        let actual = b_space_to_a(pos, d!(), d!());
        assert_eq!(actual, pos.into());
    }
}

proptest!{
    #[test]
    fn b_space_to_a_with_defaults_is_identity_gen_float(
        pos in proptest::num::f32::ANY,
    ) {
        let actual = b_space_to_a(pos.into(), d!(), d!());
        assert_eq!(actual, pos.into());
    }
}

#[test]
fn b_space_to_a_with_defaults_is_identity_generated_reduction() {
    let pos = -51382920.0;
    
    let actual: AbsPos = AbsPos::default() + (AbsPos::from(pos) - AbsPos::default());
    assert_eq!(actual, pos.into());
}

proptest!{
    #[test]
    fn abs_pos_into_f32_into_abs_pos_is_identity(
        pos in arb::abs_pos(),
    ) {
        let actual = AbsPos::from(f32::from(pos));
        assert_eq!(actual, pos);
    }
}


#[test]
fn abs_pos_into_f32_into_abs_pos_is_identity_in_this_generated_case() {
    let pos = AbsPos(9223372036854775807);
    let actual = AbsPos::from(f32::from(pos));
    assert_eq!(actual, pos);
}

#[test]
fn abs_pos_into_f32_into_abs_pos_is_identity_in_this_generated_case_reduction() {
    const SCALE: f32 = (1u64 << 32) as f32;
    dbg!(SCALE);

    let pos = AbsPos(9223372036854775807);
    let actual = AbsPos::from(dbg!(pos.0 as f64 / SCALE as f64) as f32);
    assert_eq!(actual, pos);
}