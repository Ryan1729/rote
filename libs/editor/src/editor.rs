use editor_types::{CursorState, Vec1};
use macros::{c, d};
use platform_types::{
    position_to_screen_space, push_highlights, screen_space_to_position, BufferView, CharDim, Cmd,
    Input, UpdateAndRenderOutput, View,
};
use text_buffer::TextBuffer;

#[derive(Default)]
pub struct State {
    buffers: Vec1<TextBuffer>,
    current_burrer_index: usize,
    scroll_x: f32,
    scroll_y: f32,
    screen_w: f32,
    screen_h: f32,
    text_char_dim: CharDim,
    status_char_dim: CharDim,
}

impl State {
    #[perf_viz::record]
    fn current_buffer(&self) -> Option<&TextBuffer> {
        self.buffers.get(self.current_burrer_index)
    }
    #[perf_viz::record]
    fn current_buffer_mut(&mut self) -> Option<&mut TextBuffer> {
        self.buffers.get_mut(self.current_burrer_index)
    }

    pub fn new() -> State {
        d!()
    }
}

pub fn new() -> State {
    d!()
}

impl From<String> for State {
    fn from(s: String) -> Self {
        let mut output: Self = d!();

        output.buffers = Vec1::new(TextBuffer::from(s));

        output
    }
}

impl From<&str> for State {
    fn from(s: &str) -> Self {
        let mut output: Self = d!();

        output.buffers = Vec1::new(TextBuffer::from(s));

        output
    }
}

#[perf_viz::record]
pub fn render_view(state: &State, view: &mut View) {
    use platform_types::BufferViewKind;
    let status_line_y = state.screen_h - state.status_char_dim.h;
    view.buffers.clear();

    let status_line_view = BufferView {
        kind: BufferViewKind::StatusLine,
        screen_position: (0.0, status_line_y),
        bounds: (state.screen_w, state.status_char_dim.h),
        ..d!()
    };

    match state.current_buffer() {
        Some(buffer) => {
            let cursors = buffer.cursors();
            const AVERAGE_SELECTION_LNES_ESTIMATE: usize = 4;
            let mut highlights =
                Vec::with_capacity(cursors.len() * AVERAGE_SELECTION_LNES_ESTIMATE);

            for c in cursors.iter() {
                let position = c.get_position();

                let screen_position = position_to_screen_space(
                    position,
                    state.text_char_dim,
                    (state.scroll_x, state.scroll_y),
                )
                .into();

                view.buffers.push(BufferView {
                    kind: BufferViewKind::Cursor,
                    screen_position,
                    bounds: (state.screen_w, state.text_char_dim.h),
                    color: match c.state {
                        CursorState::None => c![0.9, 0.3, 0.3],
                        CursorState::PressedAgainstWall => c![0.9, 0.9, 0.3],
                    },
                    chars: "▏".to_string(),
                    ..d!()
                });

                push_highlights(&mut highlights, position, c.get_highlight_position());
            }

            view.buffers.push(BufferView {
                kind: BufferViewKind::Edit,
                screen_position: (state.scroll_x, state.scroll_y),
                bounds: (std::f32::INFINITY, std::f32::INFINITY),
                color: c![0.3, 0.3, 0.9],
                chars: buffer.chars().collect::<String>(),
                highlights,
            });

            view.buffers.push(BufferView {
                color: c![0.3, 0.9, 0.3],
                chars: {
                    use std::fmt::Write;
                    let mut chars = String::with_capacity(state.screen_w as usize);

                    let _cannot_actually_fail = write!(
                        chars,
                        "c{:?} ",
                        (state.text_char_dim.w, state.text_char_dim.h)
                    );

                    chars = buffer.cursors().iter().fold(chars, |mut acc, c| {
                        let _cannot_actually_fail = write!(
                            acc,
                            "{} ({:?}|{:?})",
                            c,
                            buffer.find_index(c).and_then(|o| if o == 0 {
                                None
                            } else {
                                Some(o - 1)
                            }),
                            buffer.find_index(c),
                        );
                        acc
                    });

                    chars
                },
                ..status_line_view
            });
        }
        None => {
            view.buffers.push(BufferView {
                color: c![0.9, 0.3, 0.3],
                chars: "No buffer selected.".to_owned(),
                ..status_line_view
            });
        }
    };
}

macro_rules! set_if_present {
    ($source:ident => $target:ident.$field:ident) => {
        if let Some($field) = $source.$field {
            $target.$field = $field;
        }
    };
}

//#[check_or_no_panic::check_or_no_panic]
pub fn update_and_render(state: &mut State, input: Input) -> UpdateAndRenderOutput {
    update_and_render_inner(state, input)
}

// This extra fn is a workaround for the record attribute causing a "procedural macros cannot
// expand to macro definitions" error otherwise.According to issue #54727, this is because there
// is some worry that all the macro hygiene edge cases may not be handled.
fn update_and_render_inner(state: &mut State, input: Input) -> UpdateAndRenderOutput {
    macro_rules! buffer_call {
        ($buffer: ident . $($method_call:tt)*) => {
            buffer_call!($buffer {$buffer.$($method_call)*})
        };
        ($buffer: ident $tokens:block) => {
            if let Some($buffer) = state.current_buffer_mut() {
                $tokens;
            }
        }
    }
    perf_viz::record_guard!("update_and_render");

    if cfg!(debug_assertions) {
        if_changed::dbg!(input);
    }
    match input {
        Input::None => {}
        Input::Quit => {}
        Input::Insert(c) => buffer_call!(b.insert(c)),
        Input::Delete => buffer_call!(b.delete()),
        Input::Redo => buffer_call!(b.redo()),
        Input::Undo => buffer_call!(b.undo()),
        Input::MoveAllCursors(r#move) => buffer_call!(b.move_all_cursors(r#move)),
        Input::ExtendSelectionForAllCursors(r#move) => {
            buffer_call!(b.extend_selection_for_all_cursors(r#move))
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
            set_if_present!(sizes => state.screen_w);
            set_if_present!(sizes => state.screen_h);
            set_if_present!(sizes => state.text_char_dim);
            set_if_present!(sizes => state.status_char_dim);
        }
        Input::ReplaceCursors(xy) => {
            let position =
                screen_space_to_position(xy, state.text_char_dim, (state.scroll_x, state.scroll_y));
            buffer_call!(b.replace_cursors(position))
        }
        Input::DragCursors(xy) => {
            // In practice we currently expect this to be sent only immeadately after an
            // `Input::ReplaceCursors` input, so there will be only one cursor. But it seems like
            // we might as well just do it to all the cursors
            let position =
                screen_space_to_position(xy, state.text_char_dim, (state.scroll_x, state.scroll_y));
            buffer_call!(b.drag_cursors(position))
        }
    }

    let mut view = d!();

    render_view(state, &mut view);

    (view, Cmd::NoCmd)
}
