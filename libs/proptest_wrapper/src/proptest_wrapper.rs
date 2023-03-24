pub use proptest::{
    proptest,
    prop_compose,
    prop_oneof
};
pub use proptest::strategy::{Just, Strategy, NewTree, ValueTree};
pub use proptest::num::{
    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    f32,
    f64,
};
pub use proptest::char::CharStrategy;
pub use proptest::sample::{Selector, SelectorStrategy};
pub use proptest::prelude::ProptestConfig;

/// These are not built-in to the proptest crate, but are added here for convenience.
/// Also, some of these are suspected to compile faster due to not exposing generics
/// acros the crate boundary.
pub mod extra {
    use super::{CharStrategy, Selector, SelectorStrategy};
    use proptest::arbitrary::Arbitrary;

    pub fn any_bool() -> proptest::bool::Any {
        bool::arbitrary()
    }

    pub fn any_u8() -> proptest::num::u8::Any {
        u8::arbitrary()
    }

    pub fn any_u16() -> proptest::num::u16::Any {
        u16::arbitrary()
    }

    pub fn any_u32() -> proptest::num::u32::Any {
        u32::arbitrary()
    }

    pub fn any_u64() -> proptest::num::u64::Any {
        u64::arbitrary()
    }

    pub fn any_u128() -> proptest::num::u128::Any {
        u128::arbitrary()
    }

    pub fn any_usize() -> proptest::num::usize::Any {
        usize::arbitrary()
    }

    pub fn any_i8() -> proptest::num::i8::Any {
        i8::arbitrary()
    }

    pub fn any_i16() -> proptest::num::i16::Any {
        i16::arbitrary()
    }

    pub fn any_i32() -> proptest::num::i32::Any {
        i32::arbitrary()
    }

    pub fn any_i64() -> proptest::num::i64::Any {
        i64::arbitrary()
    }

    pub fn any_i128() -> proptest::num::i128::Any {
        i128::arbitrary()
    }

    pub fn any_isize() -> proptest::num::isize::Any {
        isize::arbitrary()
    }

    pub fn any_f32() -> proptest::num::f32::Any {
        f32::arbitrary()
    }

    pub fn any_f64() -> proptest::num::f64::Any {
        f64::arbitrary()
    }

    pub fn any_char() -> CharStrategy<'static> {
        char::arbitrary()
    }

    pub fn any_string() -> &'static str {
        String::arbitrary()
    }

    pub fn any_os_string() -> proptest::strategy::MapInto<&'static str, std::ffi::OsString> {
        std::ffi::OsString::arbitrary()
    }

    pub fn any_sample_selector() -> SelectorStrategy {
        Selector::arbitrary()
    }
}
pub use extra::*;

pub mod collection {
    pub use proptest::collection::{vec};
}

pub mod strategy {
    pub use proptest::strategy::{Just, Strategy, NewTree, ValueTree};
}

pub mod option {
    pub use proptest::option::{of};
}

pub mod test_runner {
    pub use proptest::test_runner::{FileFailurePersistence, TestRunner, TestCaseError};
}