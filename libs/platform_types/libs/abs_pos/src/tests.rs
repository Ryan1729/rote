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
            todo!()
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
            todo!()
            /*()
                .new_tree(runner)
                .map(AbsPosValueTree)*/
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

        assert_eq!(actual, a_space_point);
    }
}

proptest!{
    #[test]
    fn b_to_a_to_b_is_identity(
        delta in arb::abs_pos(),
        base in arb::abs_pos(),
        b_space_point in arb::abs_pos(),
    ) {
        let actual = a_space_to_b(
            delta,
            base,
            b_space_to_a(
                b_space_point,
                base,
                delta,
            )
        );

        assert_eq!(actual, b_space_point);
    }
}
