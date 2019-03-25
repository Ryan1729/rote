use platform_types::{BufferView, Cmd, Input, View};

pub struct State {
    //TODO replace with a Gap Buffer
    buffer: String,
    scroll_x: f32,
    scroll_y: f32,
    screen_w: f32,
    screen_h: f32,
    line_h: f32,
}

pub fn new() -> State {
    State {
        buffer: include_str!("../../../text/slipsum.txt").into(),
        scroll_x: 0.0,
        scroll_y: 0.0,
        screen_w: 0.0,
        screen_h: 0.0,
        line_h: 0.0,
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
        Input::ScrollVertically(amount) => {
            state.scroll_y -= amount;
        }
        Input::ScrollHorizontally(amount) => {
            state.scroll_x += amount;
        }
        Input::ResetScroll => {
            state.scroll_x = 0.0;
            state.scroll_y = 0.0;
        }
        Input::SetSizes(sizes) => {
            if let Some(screen_w) = sizes.screen_w {
                state.screen_w = screen_w;
            }
            if let Some(screen_h) = sizes.screen_h {
                state.screen_h = screen_h;
            }
            if let Some(line_h) = sizes.line_h {
                state.line_h = line_h;
            }
        }
    }

    let status_line_y = state.screen_h - state.line_h;

    use platform_types::BufferViewKind;
    (
        View {
            buffers: vec![
                BufferView {
                    kind: BufferViewKind::Edit,
                    screen_position: (state.scroll_x, state.scroll_y),
                    bounds: (state.screen_w, status_line_y - state.scroll_x),
                    color: [0.3, 0.3, 0.9, 1.0],
                    chars: state.buffer.clone(),
                },
                BufferView {
                    kind: BufferViewKind::StatusLine,
                    screen_position: (0.0, status_line_y),
                    bounds: (state.screen_w, state.line_h),
                    color: [0.3, 0.9, 0.3, 1.0],
                    chars: state.buffer.clone(),
                },
            ],
        },
        Cmd::NoCmd,
    )
}
