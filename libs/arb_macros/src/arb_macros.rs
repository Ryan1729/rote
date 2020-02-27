#[macro_export]
macro_rules! arb_enum {
    ($vis: vis fn $fn_name: ident ($($param: ident : $type: ty),* $(,)?) -> $output: path {$($variant: pat => $strategy: expr),+ $(,)? }) => {
        $vis fn $fn_name($($param : $type),*) -> impl proptest::prelude::Strategy<Value = $output> {
            use $output::*;
            use proptest::prelude::*;

            
            // If this compiles we produce them all
            #[allow(dead_code)]
            fn produces_all_check(arb_enum: $output) {
                match arb_enum {
                    $($variant => {}),+
                }
            }

            proptest::prelude::prop_oneof![
                $($strategy),+
            ]
        }
    }
}

#[test]
fn arb_enum_works_on_this_no_args_example() {
    #[derive(Clone, Copy, Debug)]
    enum E {
        Single,
        HasByte(u8),
    }

    arb_enum!{
        fn e() -> E
        {
            Single => Just(Single),
            HasByte(_) => proptest::prelude::any::<u8>().prop_map(HasByte),
        }
    }

    let _ = e(); 
}

#[test]
fn arb_enum_works_on_this_single_arg_example() {
    #[derive(Clone, Copy, Debug)]
    enum E {
        Single,
        HasByte(u8),
    }

    arb_enum!{
        fn e(max_byte: u8) -> E
        {
            Single => Just(Single),
            HasByte(_) => (0u8..=max_byte).prop_map(HasByte),
        }
    }

    let _ = e(127);
}