use platform_types::{BufferView, Cmd, Input, View};

pub struct State {
    //TODO replace with a Gap Buffer
    buffer: String,
    scroll_x: f32,
    scroll_y: f32,
}

pub fn new() -> State {
    State {
        buffer: include_str!("../../../text/slipsum.txt").into(),
        scroll_x: 0.0,
        scroll_y: 0.0,
    }
}

pub fn update_and_render(state: &mut State, input: Input) -> (View, Cmd) {
    match input {
        Input::NoInput => {}
        Input::Insert(c) => {
            state.buffer.push(c);
        }
        Input::Delete => {
            state.buffer.pop();
        }
        Input::ResetScroll => {
            state.scroll_x = 0.0;
            state.scroll_y = 0.0;
        }
        Input::ScrollVertically(amount) => {
            state.scroll_y -= amount;
        }
        Input::ScrollHorizontally(amount) => {
            state.scroll_x += amount;
        }
    }

    use platform_types::BufferViewKind;
    (
        View {
            buffers: vec![
                BufferView {
                    kind: BufferViewKind::Edit,
                    screen_position: (state.scroll_x, state.scroll_y),
                    chars: state.buffer.clone(),
                },
                BufferView {
                    kind: BufferViewKind::StatusLine,
                    screen_position: (state.scroll_x, state.scroll_y),
                    chars: state.buffer.clone(),
                },
            ],
        },
        Cmd::NoCmd,
    )
}
