use platform_types::{Cmd, Input, View};

static mut STATE: editor::State = editor::new();

fn update_and_render(input: Input) -> (View, Cmd) {
    let state: &mut editor::State = unsafe { &mut STATE };

    editor::update_and_render(state, input)
}

fn main() {
    platform_layer::display(update_and_render);
}
