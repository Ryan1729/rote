use platform_types::{Cmd, Input, View};

pub struct State {}

pub const fn new() -> State {
    State {}
}

pub fn update_and_render(state: &mut State, input: Input) -> (View, Cmd) {
    (View {}, Cmd::NoCmd)
}
