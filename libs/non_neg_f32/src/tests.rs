use super::*;
pub mod arb {
    use super::*;
    use proptest::test_runner::{TestRunner};
    use proptest::strategy::{Strategy, NewTree, ValueTree};
    use proptest::num::f32;

    struct NonNegF32ValueTree(<f32::Any as Strategy>::Tree);

    impl ValueTree for NonNegF32ValueTree {
        type Value = NonNegF32;
        fn current(&self) -> Self::Value {
            non_neg_f32!(self.0.current())
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
    struct NonNegF32Strat;

    impl Strategy for NonNegF32Strat {
        type Tree = NonNegF32ValueTree;
        type Value = NonNegF32;

        fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
            (f32::POSITIVE | f32::ZERO)
                .new_tree(runner)
                .map(NonNegF32ValueTree)
        }
    }

    pub fn non_neg_f32() -> impl Strategy<Value = NonNegF32> + Copy {
        NonNegF32Strat
    }
}