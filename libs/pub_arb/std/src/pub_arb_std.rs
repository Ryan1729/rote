/// This crate contains ways to create `proptest` `Strategy` implementations, for types in the rust standard library.

use proptest::prelude::prop_compose;

prop_compose!{
    pub fn path_buf()(
        s in ".*\\.fakefile" 
    ) -> std::path::PathBuf {
        std::path::PathBuf::from(s)
    }
}

// The rope library we are using treats the excluded chars as line breaks, so we do too.
// See also https://www.unicode.org/reports/tr14/tr14-32.html
prop_compose!{
    pub fn non_line_break_char()(
        s in "[^\u{a}-\r\u{0085}\u{2028}\u{2029}]"
    ) -> char {
        s.chars().next().unwrap()
    }
}

prop_compose!{
    pub fn distinct_strings()(s1 in ".*", mut s2 in ".*") -> (String, String) {
        if s1 == s2 {
            s2.push('.');
        }
        (s1, s2)
    }
}

pub mod f32 {
    use proptest::num;
    use proptest::prelude::prop_compose;

    pub fn usual() -> num::f32::Any {
        num::f32::POSITIVE | num::f32::NEGATIVE | num::f32::NORMAL | num::f32::ZERO
    }

    prop_compose!{
        pub fn within_0_to_1()(f in num::f32::POSITIVE | num::f32::ZERO) -> f32 {
            if f > 1.0 {
               f / std::f32::MAX 
            } else {
                f
            }
        }
    }

    prop_compose!{
        pub fn rounded_non_negative()(n in 0..(1 << 24)) -> f32 {
            n as f32
        }
    }


    
}
pub use crate::f32::*;