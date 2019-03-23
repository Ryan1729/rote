use platform_types::{BufferView, Cmd, Input, View};

pub struct State {
    //TODO replace with a Gap Buffer
    buffer: String,
}

pub fn new() -> State {
    State {
        buffer: include_str!("../../../text/slipsum.txt").into(),
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
    }

    (
        View {
            buffers: vec![BufferView {
                chars: state.buffer.clone(),
            }],
        },
        Cmd::NoCmd,
    )
}
