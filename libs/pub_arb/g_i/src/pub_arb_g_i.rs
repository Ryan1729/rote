use g_i::{LengthSize, SelectableVec1, Vec1};
pub use g_i::tests::arb::*;
use proptest::collection::vec;
use proptest::prelude::{BoxedStrategy, Strategy, prop_compose, Just};

pub fn selectable_vec1<'strat, A: std::fmt::Debug + Clone, S: Strategy<Value = A>>(
    strat: S, max_len: LengthSize,
) -> impl Strategy<Value = SelectableVec1<A>> {
    (
        vec(strat, 1..(max_len as usize)),
        state_with_index(max_len)
    )
    .prop_map(|(v, (current_index, i_s))| {
        let elements = Vec1::try_from_vec(v).unwrap();
        SelectableVec1::from_parts(
            elements,
            i_s,
            current_index,
        )
    })
}
