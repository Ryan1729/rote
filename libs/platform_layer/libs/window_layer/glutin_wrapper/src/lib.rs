/// The purpose of this crate is to make is so that the glutin version is specified
/// in exactly one place.

// https://internals.rust-lang.org/t/cfg-keyword-for-if-multiple-more-than-one/15751/4
const _: () = {
    let enabled_features = {
        cfg!(feature = "glutin-0-32") as u32
        + cfg!(feature = "glutin-0-29") as u32
    };

    match enabled_features {
        0 => panic!("\nNone of the glutin_wrapper features were enabled. You must enable exactly one."),
        1 => {}
        2.. => panic!("\nMultiple of the glutin_wrapper features were enabled at once. You must enable exactly one."),
    }
};

#[cfg(feature = "glutin-0-32")]
pub use glutin_0_32::*;

#[cfg(feature = "glutin-0-29")]
pub use glutin_0_29::*;
