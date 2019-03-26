use platform_types::{d, BufferView, Cmd, Input, View};
use vec1::Vec1;

//TODO replace with a real Gap Buffer
type GapBuffer = String;

struct Buffer {
    gap_buffer: GapBuffer,
    cursors: Vec1<Cursor>,
}

impl Buffer {
    fn new() -> Buffer {
        Buffer {
            gap_buffer: include_str!("../../../text/slipsum.txt").into(),
            cursors: Vec1::new(Cursor {
                position: Position { line: 4, offset: 2 },
                ..d!()
            }),
        }
    }

    fn insert(&mut self, c: char) {
        self.gap_buffer.push(c);
    }

    fn delete(&mut self) {
        self.gap_buffer.pop();
    }
}

impl<'buffer> Buffer {
    fn chars(&'buffer self) -> impl Iterator<Item = char> + 'buffer {
        self.gap_buffer.chars()
    }
}

pub struct State {
    buffers: Vec1<Buffer>,
    current_burrer_index: usize,
    scroll_x: f32,
    scroll_y: f32,
    screen_w: f32,
    screen_h: f32,
    ///We are currently assuming the font is monospace!
    char_w: f32,
    line_h: f32,
}

impl State {
    fn current_buffer(&self) -> Option<&Buffer> {
        self.buffers.get(self.current_burrer_index)
    }
    fn current_buffer_mut(&mut self) -> Option<&mut Buffer> {
        self.buffers.get_mut(self.current_burrer_index)
    }
}

pub fn new() -> State {
    State {
        buffers: Vec1::new(Buffer::new()),
        current_burrer_index: 0,
        scroll_x: 0.0,
        scroll_y: 0.0,
        screen_w: 0.0,
        screen_h: 0.0,
        char_w: 0.0,
        line_h: 0.0,
    }
}

/// `offset` indicates a location before or after characters, not at the charaters.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Position {
    pub line: usize,
    pub offset: usize,
}

#[derive(Clone, Debug, Default)]
pub struct Cursor {
    pub position: Position,
    sticky_offset: usize,
}

pub fn update_and_render(state: &mut State, input: Input) -> (View, Cmd) {
    match input {
        Input::NoInput => {}
        Input::Insert(c) => {
            if let Some(b) = state.current_buffer_mut() {
                b.insert(c);
            }
        }
        Input::Delete => {
            if let Some(b) = state.current_buffer_mut() {
                b.delete();
            }
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
            macro_rules! set_if_present {
                ($field:ident) => {
                    if let Some($field) = sizes.$field {
                        state.$field = $field;
                    }
                };
            }
            set_if_present!(screen_w);
            set_if_present!(screen_h);
            set_if_present!(char_w);
            set_if_present!(line_h);
        }
    }

    let status_line_y = state.screen_h - state.line_h;

    use platform_types::BufferViewKind;
    (
        View {
            buffers: state.current_buffer().map_or_else(
                || {
                    vec![BufferView {
                        kind: BufferViewKind::StatusLine,
                        screen_position: (0.0, status_line_y),
                        bounds: (state.screen_w, state.line_h),
                        color: [0.9, 0.3, 0.3, 1.0],
                        chars: "No buffer selected.".to_owned(),
                    }]
                },
                |buffer| {
                    let mut views = vec![
                        BufferView {
                            kind: BufferViewKind::Edit,
                            screen_position: (state.scroll_x, state.scroll_y),
                            bounds: (std::f32::INFINITY, status_line_y - state.scroll_x),
                            color: [0.3, 0.3, 0.9, 1.0],
                            chars: buffer.chars().collect::<String>(),
                        },
                        BufferView {
                            kind: BufferViewKind::StatusLine,
                            screen_position: (0.0, status_line_y),
                            bounds: (state.screen_w, state.line_h),
                            color: [0.3, 0.9, 0.3, 1.0],
                            chars: buffer.chars().collect::<String>(),
                        },
                    ];

                    for position in buffer.cursors.iter().map(|c| c.position) {
                        views.push(BufferView {
                            kind: BufferViewKind::StatusLine,
                            screen_position: (
                                position.offset as f32 * state.char_w,
                                position.line as f32 * state.line_h,
                            ),
                            bounds: (state.screen_w, state.line_h),
                            color: [0.9, 0.3, 0.3, 1.0],
                            chars: "▏".to_string(),
                        });
                    }

                    views
                },
            ),
        },
        Cmd::NoCmd,
    )
}
