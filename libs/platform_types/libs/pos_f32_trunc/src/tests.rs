use super::*;
pub mod arb {
    use super::*;
    use proptest::test_runner::{TestRunner};
    use proptest::strategy::{Strategy, NewTree, ValueTree};
    use proptest::num::f32;

    struct PosF32TruncValueTree(<f32::Any as Strategy>::Tree);

    impl ValueTree for PosF32TruncValueTree {
        type Value = PosF32Trunc;
        fn current(&self) -> Self::Value {
            pos_f32_trunc!(self.0.current())
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
    struct PosF32TruncStrat;

    impl Strategy for PosF32TruncStrat {
        type Tree = PosF32TruncValueTree;
        type Value = PosF32Trunc;

        fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
            (f32::POSITIVE | f32::NORMAL)
                .new_tree(runner)
                .map(PosF32TruncValueTree)
        }
    }

    pub fn pos_f32_trunc() -> impl Strategy<Value = PosF32Trunc> + Copy {
        PosF32TruncStrat
    }
}