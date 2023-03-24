#[macro_export]
macro_rules! arb_enum {
    ($vis: vis fn $fn_name: ident ($($param: ident : $type: ty),* $(,)?) -> $output: path  {$($variant: pat => $strategy: expr),+ $(,)? }) => {
        arb_enum!{
            $vis fn $fn_name($($param : $type),*) -> $output as $output {
                $($variant => $strategy),+
            }
        }
    };
    ($vis: vis fn $fn_name: ident ($($param: ident : $type: ty),* $(,)?) -> $output: ty as $enum_path: path  {$($variant: pat => $strategy: expr),+ $(,)? }) => {
        $vis fn $fn_name($($param : $type),*) -> impl proptest::Strategy<Value = $output> {
            use $enum_path::*;
            use proptest::*;

            
            // If this compiles we produce them all
            #[allow(dead_code)]
            fn produces_all_check(arb_enum: $output) {
                match arb_enum {
                    $($variant => {}),+
                }
            }

            proptest::prop_oneof![
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
            HasByte(_) => proptest::any_u8().prop_map(HasByte),
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

#[test]
fn arb_enum_works_on_this_generic_no_args_example() {
    #[derive(Clone, Copy, Debug)]
    enum E<A> {
        Single,
        HasA(A),
    }

    arb_enum!{
        fn e() -> E<u8> as E
        {
            Single => Just(Single),
            HasA(_) => proptest::any_u8().prop_map(HasA),
        }
    }

    let _ = e(); 
}

#[test]
fn arb_enum_works_on_this_generic_single_arg_example() {
    #[derive(Clone, Copy, Debug)]
    enum E<A> {
        Single,
        HasA(A),
    }

    arb_enum!{
        fn e(max_byte: u8) -> E<u8> as E
        {
            Single => Just(Single),
            HasA(_) => (0u8..=max_byte).prop_map(HasA),
        }
    }

    let _ = e(127);
}