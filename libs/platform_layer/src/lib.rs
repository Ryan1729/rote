// We might have different platform layer options later, so let's keep this separate.
use platform_types::UpdateAndRender;
pub fn run(update_and_render: UpdateAndRender) {
    let result = wimp::run(update_and_render);

    if let Err(e) = result {
        println!("wimp::run(update_and_render) error:\n{}", e);
    }
}
