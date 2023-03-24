use super::*;
pub mod arb {
    use super::*;
    use proptest::test_runner::{TestRunner};
    use proptest::strategy::{Strategy, NewTree, ValueTree};
    use proptest::f32;

    struct F32_0_1ValueTree(<f32::Any as Strategy>::Tree);

    impl ValueTree for F32_0_1ValueTree {
        type Value = F32_0_1;
        fn current(&self) -> Self::Value {
            // This clamps away only half the possible values, so maybe that's fine?
            f32_0_1!(self.0.current())
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
    struct F32_0_1Strat;

    impl Strategy for F32_0_1Strat {
        type Tree = F32_0_1ValueTree;
        type Value = F32_0_1;

        fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
            (f32::POSITIVE | f32::ZERO)
                .new_tree(runner)
                .map(F32_0_1ValueTree)
        }
    }

    pub fn f32_0_1() -> impl Strategy<Value = F32_0_1> + Copy {
        F32_0_1Strat
    }
}