
mod sys;

use sys::platform_run_options;

pub fn run(
    // TODO Define ergonomic versions of the arguments to `run`
) -> ! {
    // SAFETY: platform_run_options is known to be valid when zeroed.
    let options = unsafe {
        std::mem::zeroed::<platform_run_options>()
    };

    // SAFETY: No special safety considerations are known, besides general C FFI ones.
    unsafe {
        //platform_run(u_and_r, options);
        // Let's see what happens if we pass None here
        sys::platform_run(None, options);
    }

    println!("Hit fallback loop in c_platform.rs");

    loop {}
}