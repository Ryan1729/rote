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
#[cfg(feature = "glutin-0-32")]
pub mod winit_wrapper {
    pub use winit::*;
}
#[cfg(feature = "glutin-0-32")]
pub mod glutin_winit_wrapper {
    pub use glutin_winit::*;
}

#[cfg(feature = "glutin-0-32")]
pub mod gl_wrapper {
    pub use gl33::*;
    // A compatibility layer with the glutin example, which we plan to remove later {
    pub use gl33::{
        GlFns as Gl,
        GL_ARRAY_BUFFER as ARRAY_BUFFER,
        GL_COLOR_BUFFER_BIT as COLOR_BUFFER_BIT,
        GL_FLOAT as FLOAT,
        GL_FRAGMENT_SHADER as FRAGMENT_SHADER,
        GL_RENDERER as RENDERER,
        GL_TRIANGLES as TRIANGLES,
        GL_SHADING_LANGUAGE_VERSION as SHADING_LANGUAGE_VERSION,
        GL_STATIC_DRAW as STATIC_DRAW,
        GL_VERTEX_SHADER as VERTEX_SHADER,
        GL_VERSION as VERSION,
    };
    pub mod types {
        pub use gl33::{
            GLenum,
        };

        pub type GLuint = std::os::raw::c_uint;
        pub type GLsizei = std::os::raw::c_int;
        pub type GLsizeiptr = isize;
    }
    // }
}


#[cfg(feature = "glutin-0-29")]
pub use glutin_0_29::*;
