// We might have different platform layer options later, so let's keep this separate.
pub fn run(update_and_render: platform_types::UpdateAndRender) {
    opengl::run(update_and_render);
}
