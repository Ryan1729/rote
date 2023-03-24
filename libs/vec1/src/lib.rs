pub use vec1_external::*;

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests {
    use super::*;
    pub mod arb {
        use super::*;
        use proptest::{
            collection,
            Strategy
        };

        pub fn vec1<D: core::fmt::Debug>(
            strat: impl Strategy<Value = D>,
            max_len: usize,
        ) -> impl Strategy<Value = Vec1<D>> {
            collection::vec(strat, 1..std::cmp::max(2, max_len))
                .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
        }
    }
}