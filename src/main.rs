use platform_types::{Cmd, Input, View};

fn update_and_render(input: Input) -> (View, Cmd) {
    static mut STATE: editor::State = editor::new();
    let state: &mut editor::State = unsafe { &mut STATE };

    editor::update_and_render(state, input)
}

fn main() {
    platform_layer::run(update_and_render);
}
