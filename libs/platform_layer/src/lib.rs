// We might have different platform layer options later, so let's keep this separate.
pub fn display(update_and_render: platform_types::UpdateAndRender) {
    opengl::display(update_and_render);
}
