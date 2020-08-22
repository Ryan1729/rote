#[macro_export]
macro_rules! println {
    (
        $($format_args:tt)+
    ) => {
        if_changed::println_if_changed(
            if_changed::key!(),
            format!($($format_args)*)
        )
    };
}

#[macro_export]
macro_rules! print {
    (
        $($format_args:tt)+
    ) => {
        if_changed::print_if_changed(
            if_changed::key!(),
            format!($($format_args)*)
        )
    };
}

#[macro_export]
macro_rules! dbg {
    () => {
        $crate::println!("[{}:{}]", file!(), line!());
    };
    (
        $value:expr
    ) => {
        match $value {
            temp => {
                $crate::println!(
                    "[{}:{}] {} = {:#?}",
                    file!(),
                    line!(),
                    stringify!($value),
                    &temp
                );
                temp
            }
        }
    };
}

#[macro_export]
macro_rules! key {
    () => {
        concat!(env!("CARGO_PKG_NAME"), ":", file!(), ":", line!(),)
    };
}

#[cfg(not(feature = "fast_hash"))]
use std::collections::HashMap;

#[cfg(feature = "fast_hash")]
use fast_hash::Map as HashMap;

lazy_static::lazy_static! {
    static ref PREVIOUSLY_PRINTED: std::sync::Mutex<HashMap<&'static str, String>> =
        std::sync::Mutex::new(HashMap::default());
}

macro_rules! p_if_changed {
    ($fn_name:ident ($printable:ident)$p:block) => {
        pub fn $fn_name(key: &'static str, $printable: String) {
            match PREVIOUSLY_PRINTED.try_lock() {
                Ok(mut map) => {
                    let print_and_save = match map.get(key) {
                        Some(previous) => *previous != $printable,
                        None => true,
                    };

                    if print_and_save {
                        $p;
                        map.insert(key, $printable);
                    }
                }
                Err(e) => {
                    if cfg!(feature = "invariant-checking") {
                        panic!("PREVIOUSLY_PRINTED already borrowed!? \n{}", e);
                    }
                }
            }
        }
    };
}

p_if_changed! {
    println_if_changed
    (printable)
    {
        std::println!("{}", printable)
    }
}

p_if_changed! {
    print_if_changed
    (printable)
    {
        std::print!("{}", printable)
    }
}
