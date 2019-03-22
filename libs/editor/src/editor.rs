use platform_types::{Cmd, Input, View};

pub struct State {
    frame_count: usize,
}

pub const fn new() -> State {
    State { frame_count: !0 }
}

pub fn update_and_render(state: &mut State, input: Input) -> (View, Cmd) {
    state.frame_count -= 1;

    (View {}, Cmd::NoCmd)
}
