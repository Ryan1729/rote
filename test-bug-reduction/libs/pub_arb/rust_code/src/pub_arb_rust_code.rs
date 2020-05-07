use macros::{fmt_display};
use proptest::prelude::*;
use proptest::collection;

const FUNCTION_SIZE_ESTIMATE: usize = 128 + PARAM_SIZE_ESTIMATE;
const PARAM_SIZE_ESTIMATE: usize = 64;

prop_compose! {
    /// This should be produce Strings which are syntactically valid rust code
    /// but which will likely produce warnings about unused variable etc. if
    /// compiled.
    pub fn rust_code(max_size: usize)(
        functions in collection::vec(rust_function(max_size), 0..max_size),
    ) -> String {
        let mut s = String::with_capacity(max_size * FUNCTION_SIZE_ESTIMATE);

        for func in functions {
            s.push_str(&func);
            s.push('\n');
        }

        s
    }
}

prop_compose! {
    pub fn rust_function(max_size: usize)(
        name in rust_ident(),
        parameters in rust_params(max_size),
        (expression, expr_type) in rust_expression(max_size)
    ) -> String {
        let mut s = String::with_capacity(FUNCTION_SIZE_ESTIMATE);

        s.push_str("fn ");
        s.push_str(&name);
        s.push_str(&parameters);
        s.push_str(" -> ");
        s.push_str(&format!("{}", expr_type));
        s.push_str("{\n");
        s.push_str(&expression);
        s.push_str("\n}\n");

        s
    }
}
 
prop_compose! {
    pub fn rust_ident()(
        // according to https://doc.rust-lang.org/reference/identifiers.html
        // all of the keywords that are not allowed to be if you prefix with 
        // `r#` have a lowercase "e" in them
        s in "r#[a-df-zA-Z][a-df-zA-Z0-9_]*",
    ) -> String {
        s
    }
}

prop_compose! {
    pub fn rust_params(max_size: usize)(
        pairs in rust_ident_and_primitive_types(max_size),
    ) -> String {
        let mut s = String::with_capacity(PARAM_SIZE_ESTIMATE * pairs.len());

        s.push('(');
        
        for (ident, rust_type) in pairs {
            s.push_str(&ident);
            s.push_str(": ");
            s.push_str(&rust_type.to_string());
            s.push_str(", ");
        }

        s.push(')');

        s
    }
}

pub fn rust_ident_and_primitive_types(max_size: usize) -> impl Strategy<Value = Vec<(String, RustPrimitiveType)>> {
    collection::vec((rust_ident(), rust_primitive_type()), 0..max_size)
}

#[derive(Clone, Copy, Debug)]
pub enum RustPrimitiveType {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    F32,
    F64,
    Char,
    String,
    Never,
}
fmt_display!(for RustPrimitiveType : p in "{}", {
    use RustPrimitiveType::*;
    match p {
        Bool => "bool",
        U8 => "u8",
        U16 => "u16",
        U32 => "u32",
        U64 => "u64",
        U128 => "u128",
        Usize => "usize",
        I8 => "i8",
        I16 => "i16",
        I32 => "i32",
        I64 => "i64",
        I128 => "i128",
        Isize => "isize",
        F32 => "f32",
        F64 => "f64",
        Char => "char",
        String => "String",
        Never => "!",
    }
});

#[derive(Clone, Copy, Debug)]
pub enum RustType {
    Primitive(RustPrimitiveType)
}
fmt_display!(for RustType : t in "{}", match t {
    RustType::Primitive(p) => p
});

pub fn rust_expression(max_size: usize) -> impl Strategy<Value = (String, RustType)> {
    rust_type(max_size).prop_flat_map(rust_expression_from_type)
}

fn rust_expression_from_type(t: RustType) -> impl Strategy<Value = (String, RustType)> {
    match t.clone() {
        RustType::Primitive(p_t) => {
            rust_primitive_type_string(p_t)
            .prop_map(move |s| (
                s,
                RustType::Primitive(p_t)
            ))
        },
    }
}

pub fn rust_type(_max_size: usize) -> impl Strategy<Value = RustType> {
    rust_primitive_type().prop_map(RustType::Primitive)
}

pub fn rust_primitive_type() -> impl Strategy<Value = RustPrimitiveType> {
    use RustPrimitiveType::*;
    prop_oneof![
        Just(Bool),
        Just(U8),
        Just(U16),
        Just(U32),
        Just(U64),
        Just(U128),
        Just(Usize),
        Just(I8),
        Just(I16),
        Just(I32),
        Just(I64),
        Just(I128),
        Just(Isize),
        Just(F32),
        Just(F64),
        Just(Char),
        Just(String),
        Just(Never),
    ]
}

pub fn rust_primitive_type_string(p_t: RustPrimitiveType) -> impl Strategy<Value = String> {
    use RustPrimitiveType::*;
    match p_t {
        Bool => any::<bool>().prop_map(|x| x.to_string()).boxed(),
        U8 => any::<u8>().prop_map(|x| x.to_string()).boxed(),
        U16 => any::<u16>().prop_map(|x| x.to_string()).boxed(),
        U32 => any::<u32>().prop_map(|x| x.to_string()).boxed(),
        U64 => any::<u64>().prop_map(|x| x.to_string()).boxed(),
        U128 => any::<u128>().prop_map(|x| x.to_string()).boxed(),
        Usize => any::<usize>().prop_map(|x| x.to_string()).boxed(),
        I8 => any::<i8>().prop_map(|x| x.to_string()).boxed(),
        I16 => any::<i16>().prop_map(|x| x.to_string()).boxed(),
        I32 => any::<i32>().prop_map(|x| x.to_string()).boxed(),
        I64 => any::<i64>().prop_map(|x| x.to_string()).boxed(),
        I128 => any::<i128>().prop_map(|x| x.to_string()).boxed(),
        Isize => any::<isize>().prop_map(|x| x.to_string()).boxed(),
        F32 => any::<f32>().prop_map(|x| x.to_string()).boxed(),
        F64 => any::<f64>().prop_map(|x| x.to_string()).boxed(),
        Char => rust_char_string().boxed(),
        String => any::<std::string::String>().prop_map(|x| x.escape_unicode().collect()).boxed(),
        Never => "!".boxed(),
    }
}

pub fn rust_char_string() -> impl Strategy<Value = String> {
    any::<char>().prop_map(|x| x.escape_unicode().collect())
}

proptest!{
    #[test]
    fn rust_char_string_should_not_produce_strings_that_when_debug_formatted_contain_more_than_two_consecutive_backslashes(
        s in rust_char_string()
    ) {
        let debug_formatted = format!("{:?}", s);

        let mut count = 0;
        for c in debug_formatted.chars() {
            if c == '\\' {
                count += 1;
                assert!(count <= 2);
            } else {
                count = 0;
            }
        }
    }
}