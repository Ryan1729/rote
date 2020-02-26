#[macro_export]
macro_rules! arb_enum {
    ($vis: vis fn $fn_name: ident () -> $output: ty {$($variant: pat => $strategy: expr),+}) => {
        $vis fn $fn_name() -> $output {
            use $output::*;

            // If this compiles we produce them all
            fn produces_all_check(arb_enum: $output) {
                match arb_enum {
                    $($variant => {}),+
                }
            }

            proptest_oneof![
                $($strategy),+
            ]
        }
    }
}
