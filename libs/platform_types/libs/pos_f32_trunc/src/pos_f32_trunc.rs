
pub enum PosF32Trunc{}

#[macro_export]
macro_rules! pos_f32_trunc {
    ($float: literal) => {{
        // const assertion 
        #[allow(unknown_lints, eq_op)]
        const _: [(); 0 - !{ $crate::is_at_least_one!($float) } as usize] = [];

        $crate::PosF32Trunc::new_saturating($float)
    }};
    ($float: expr) => {
        $crate::PosF32Trunc::new_saturating($float)
    };
}

pub mod tests {
    pub mod arb {
        pub fn pos_f32_trunc(){}
    }
}