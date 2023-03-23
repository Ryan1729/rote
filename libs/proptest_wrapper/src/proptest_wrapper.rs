pub use proptest::{proptest, prop_compose, prop_oneof};

pub mod prelude {
    pub use proptest::prelude::{any, proptest, prop_compose, prop_oneof, Strategy, Just, ProptestConfig};
}

pub mod collection {
    pub use proptest::collection::{vec};
}

pub mod sample {
    pub use proptest::sample::{Selector};
}

pub mod strategy {
    pub use proptest::strategy::{Strategy, NewTree, ValueTree};
}

pub mod arbitrary {
    pub use proptest::arbitrary::{any};
}

pub mod num {
    pub use proptest::num::{i64, f32};
}

pub mod option {
    pub use proptest::option::{of};
}

pub mod test_runner {
    pub use proptest::test_runner::{FileFailurePersistence, TestRunner, TestCaseError};
}