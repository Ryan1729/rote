// We might have different platform layer options later, so let's keep this separate.
use platform_types::{Cmd, Input, UpdateAndRender, View};
pub fn run(update_and_render: UpdateAndRender) {
    opengl::run(update_and_render);
}
