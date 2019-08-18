use editor_types::{Cursor, CursorState, Vec1};
use macros::{c, d};
use platform_types::{
    attempt_to_make_xy_visible, pos, position_to_screen_space, position_to_text_space,
    push_highlights, screen_space_to_position, text_to_screen, Apron, BufferView, CharDim, Cmd,
    Input, Move, Position, PositionRound, ReplaceOrAdd, ScreenSpaceXY, ScrollableScreen,
    TextSpaceXY, UpdateAndRenderOutput, View, VisibilityAttemptResult,
};

use std::collections::VecDeque;
use text_buffer::TextBuffer;

#[derive(Default)]
struct ClipboardHistory {
    entries: VecDeque<String>,
    index: usize,
}

const AVERAGE_SELECTION_SIZE_ESTIMATE: usize = 32;

impl ClipboardHistory {
    fn cut(&mut self, buffer: &mut TextBuffer) -> Option<String> {
        self.push_and_join_into_option(buffer.cut_selections())

    }
    fn copy(&mut self, buffer: &TextBuffer) -> Option<String> {
        self.push_and_join_into_option(buffer.copy_selections())
    }

    fn push_and_join_into_option(&mut self, strings: Vec<String>) -> Option<String> {
        if strings.is_empty() {
            None
        } else {
            let mut output = String::with_capacity(strings.len() * AVERAGE_SELECTION_SIZE_ESTIMATE);

            let mut sep = "";
            for s in strings {
                output.push_str(sep);

                output.push_str(&s);

                self.push_if_does_not_match_top(s);

                sep = "\n";
            }

            Some(output)
        }
    }

    fn push_if_does_not_match_top(&mut self, to_push: String) {
        match self.entries.get(self.index).map(|s| s != &to_push) {
            None => {
                self.entries.push_back(to_push);
            }
            Some(true) => {
                self.entries.push_back(to_push);
                self.index += 1;
            }
            Some(false) => {}
        }
    }

    fn paste(&mut self, buffer: &mut TextBuffer, possible_string: Option<String>) {
        if let Some(s) = possible_string {
            self.push_if_does_not_match_top(s)
        }

        if let Some(s) = dbg!(&self.entries).get(self.index) {
            buffer.insert_string(s.to_owned());
        }
    }
}

#[derive(Default)]
pub struct State {
    buffers: Vec1<TextBuffer>,
    current_buffer_index: usize,
    screen: ScrollableScreen,
    text_char_dim: CharDim,
    status_char_dim: CharDim,
    clipboard_history: ClipboardHistory,
}

fn xy_to_pos(state: &State, xy: ScreenSpaceXY, round: PositionRound) -> Position {
    screen_space_to_position(xy, state.text_char_dim, state.screen.scroll, round)
}

impl State {
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
    let status_line_y = state.screen.wh.h - state.status_char_dim.h;
    view.buffers.clear();

    let status_line_view = BufferView {
        kind: BufferViewKind::StatusLine,
        screen_position: ScreenSpaceXY {
            x: 0.0,
            y: status_line_y,
        },
        bounds: (state.screen.wh.w, state.status_char_dim.h),
        ..d!()
    };

    match state.buffers.get(state.current_buffer_index) {
        Some(buffer) => {
            let cursors = buffer.cursors();
            const AVERAGE_SELECTION_LNES_ESTIMATE: usize = 4;
            let mut highlights =
                Vec::with_capacity(cursors.len() * AVERAGE_SELECTION_LNES_ESTIMATE);

            for c in cursors.iter() {
                let position = c.get_position();

                let screen_position =
                    position_to_screen_space(position, state.text_char_dim, state.screen.scroll);

                view.buffers.push(BufferView {
                    kind: BufferViewKind::Cursor,
                    screen_position,
                    bounds: (state.screen.wh.w, state.text_char_dim.h),
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
                screen_position: text_to_screen(TextSpaceXY::default(), state.screen.scroll),
                bounds: (std::f32::INFINITY, std::f32::INFINITY),
                color: c![0.3, 0.3, 0.9],
                chars: buffer.chars().collect::<String>(),
                highlights,
            });

            fn display_option_compactly<A: ToString>(op: Option<A>) -> String {
                match op {
                    None => "N".to_string(),
                    Some(a) => a.to_string(),
                }
            }

            view.buffers.push(BufferView {
                color: c![0.3, 0.9, 0.3],
                chars: {
                    use std::fmt::Write;
                    let mut chars = String::with_capacity(state.screen.wh.w as usize);

                    let _cannot_actually_fail = write!(
                        chars,
                        "{}/{}",
                        state.current_buffer_index + 1,
                        state.buffers.len()
                    );

                    // debugging
                    let _cannot_actually_fail = write!(
                        chars,
                        "  ? t{} s{} w{}",
                        state.text_char_dim, state.screen.scroll, state.screen.wh
                    );

                    chars = buffer.cursors().iter().fold(chars, |mut acc, c| {
                        let _cannot_actually_fail = write!(
                            acc,
                            "{} {} ({}|{}), ",
                            c,
                            position_to_screen_space(
                                c.get_position(),
                                state.text_char_dim,
                                state.screen.scroll
                            ),
                            display_option_compactly(
                                buffer.find_index(c).and_then(|o| if o == 0 {
                                    None
                                } else {
                                    Some(o - 1)
                                })
                            ),
                            display_option_compactly(buffer.find_index(c)),
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

fn attempt_to_make_sure_at_least_one_cursor_is_visible(
    screen: &mut ScrollableScreen,
    text_char_dim: CharDim,
    status_char_dim: CharDim,
    cursors: &Vec1<Cursor>,
) -> VisibilityAttemptResult {
    let target_cursor = cursors.last();

    let mut apron: Apron = text_char_dim.into();
    // The status line is currently on the bottom, so adding the `h` here keeps the cursors
    // above it.
    apron.bottom_h += status_char_dim.h;

    attempt_to_make_xy_visible(
        screen,
        apron,
        dbg!(position_to_text_space(
            target_cursor.get_position(),
            text_char_dim
        )),
    )
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
            if let Some($buffer) = state.buffers.get_mut(state.current_buffer_index) {
                $tokens;
            }
        }
    }
    perf_viz::record_guard!("update_and_render");

    if cfg!(debug_assertions) {
        if_changed::dbg!(&input);
    }

    let mut cmd = Cmd::NoCmd;

    macro_rules! try_to_show_cursors {
        ($buffer: expr) => {
            attempt_to_make_sure_at_least_one_cursor_is_visible(
                &mut state.screen,
                state.text_char_dim,
                state.status_char_dim,
                &$buffer.cursors(),
            ); //TODO report non success result
        };
    }

    use Input::*;
    match input {
        Input::None => {}
        Quit => {}
        Insert(c) => buffer_call!(b{
            b.insert(c);
            try_to_show_cursors!(b);
        }),
        Delete => buffer_call!(b {
            b.delete();
            try_to_show_cursors!(b);
        }),
        Redo => buffer_call!(b {
            b.redo();
            try_to_show_cursors!(b);
        }),
        Undo => buffer_call!(b {
            b.undo();
            try_to_show_cursors!(b);
        }),
        MoveAllCursors(r#move) => {
            buffer_call!(b{
                b.move_all_cursors(r#move);
                try_to_show_cursors!(b);
            });
        }
        ExtendSelectionForAllCursors(r#move) => {
            buffer_call!(b{
                b.extend_selection_for_all_cursors(r#move);
                try_to_show_cursors!(b);
            });
        }
        SelectAll => {
            buffer_call!(b{
                b.set_cursor(pos!{}, ReplaceOrAdd::Replace);
                b.extend_selection_for_all_cursors(Move::ToBufferEnd);
                // We don't need to make sure a cursor is visible here since the user
                // will understand where the cursor is.
            });
        }
        ScrollVertically(amount) => {
            state.screen.scroll.y -= amount;
        }
        ScrollHorizontally(amount) => {
            state.screen.scroll.x += amount;
        }
        ResetScroll => {
            state.screen.scroll = d!();
        }
        SetSizes(sizes) => {
            if let Some(wh) = sizes.screen {
                state.screen.wh = wh;
            }
            set_if_present!(sizes => state.text_char_dim);
            set_if_present!(sizes => state.status_char_dim);
        }
        SetCursor(xy, replace_or_add) => {
            let position = xy_to_pos(state, xy, PositionRound::Up);
            buffer_call!(b.set_cursor(position, replace_or_add))
        }
        DragCursors(xy) => {
            let position = xy_to_pos(state, xy, PositionRound::Up);
            // In practice we currently expect this to be sent only immeadately after an
            // `Input::SetCursors` input, so there will be only one cursor. But it seems like
            // we might as well just do it to all the cursors
            buffer_call!(b.drag_cursors(position))
        }
        SelectCharTypeGrouping(xy, replace_or_add) => {
            // We want different rounding for selections so that if we trigger a selection on the
            // right side of a character, we select that character rather than the next character.
            let position = xy_to_pos(state, xy, PositionRound::TowardsZero);
            buffer_call!(b.select_char_type_grouping(position, replace_or_add));
        }
        Cut => buffer_call!(b {
            if let Some(s) = state.clipboard_history.cut(b) {
                cmd = Cmd::SetClipboard(s);
            }
            try_to_show_cursors!(b);
        }),
        Copy => buffer_call!(b {
            if let Some(s) = state.clipboard_history.copy(b) {
                cmd = Cmd::SetClipboard(s);
            }
            try_to_show_cursors!(b);
        }),
        Paste(op_s) => buffer_call!(b {
            state.clipboard_history.paste(b, op_s);
            try_to_show_cursors!(b);
        }),
        InsertNumbersAtCursors => buffer_call!(b {
            b.insert_at_each_cursor(|i| i.to_string());
            try_to_show_cursors!(b);
        }),
        LoadedFile(_path, str) => buffer_call!(b {
            // TODO after there is a UI for switching buffers, create a new buffer instead of
            // inserting into the current one.
            b.insert_string(str);

            try_to_show_cursors!(b);
        }),
    }

    let mut view = d!();

    render_view(state, &mut view);

    (view, cmd)
}
