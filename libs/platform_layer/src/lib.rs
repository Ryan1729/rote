// We might have different platform layer options later, so let's keep this separate.
use platform_types::UpdateAndRender;
pub fn run(update_and_render: UpdateAndRender) {
    let result = opengl::run(update_and_render);

    if let Err(e) = result {
        println!("opengl::run(update_and_render) error:\n{}", e);
    }
}
