use super::*;
pub mod arb {
    use super::*;
    use proptest::test_runner::{TestRunner};
    use proptest::strategy::{Strategy, NewTree, ValueTree};
    use proptest::num::f32;

    struct PosF32ValueTree(<f32::Any as Strategy>::Tree);

    impl ValueTree for PosF32ValueTree {
        type Value = PosF32;
        fn current(&self) -> Self::Value {
            pos_f32!(self.0.current())
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
    struct PosF32Strat;

    impl Strategy for PosF32Strat {
        type Tree = PosF32ValueTree;
        type Value = PosF32;

        fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
            (f32::POSITIVE | f32::NORMAL)
                .new_tree(runner)
                .map(PosF32ValueTree)
        }
    }

    pub fn pos_f32() -> impl Strategy<Value = PosF32> + Copy {
        PosF32Strat
    }
}