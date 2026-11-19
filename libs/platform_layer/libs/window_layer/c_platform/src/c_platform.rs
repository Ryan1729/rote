
mod sys;

use sys::platform_run_options;

pub use sys::{
    platform_ctx as Context,
    platform_cmd as Command,
    platform_cmd_PLATFORM_CMD_NONE as COMMAND_NONE,
    platform_cmd_PLATFORM_CMD_EXIT as COMMAND_EXIT,
};

pub use ::std::os::raw::c_void as Opaque;

pub fn run<F>(
    // TODO expose the rest of the platfomr_run options
    user_data: *mut Opaque,
    u_and_r: extern "C" fn(ctx: *mut Context) -> Command,
) -> ! 
where
    F: 'static + FnMut(Context),
{
    // SAFETY: platform_run_options is known to be valid when zeroed.
    let mut options = unsafe {
        std::mem::zeroed::<platform_run_options>()
    };

    options.user_data = user_data;

    // SAFETY: No special safety considerations are known, besides general C FFI ones.
    unsafe {
        sys::platform_run(Some(u_and_r), options);
    }

    println!("Hit fallback loop in c_platform.rs");

    loop {}
}