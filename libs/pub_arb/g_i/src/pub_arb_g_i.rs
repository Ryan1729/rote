use g_i::{SelectableVec1};
use proptest::prelude::{Strategy, prop_compose};

prop_compose!{
    pub fn selectable_vec1(strat: impl Strategy)(
        chars in ".*",
    ) -> SelectableVec1 {
        SelectableVec1 {
            chars,
        }
    }
}
