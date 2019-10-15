use editor_types::{Cursor, Vec1};
use macros::{d, ok_or};
use platform_types::*;
use std::path::Path;
use std::path::PathBuf;

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

        if let Some(s) = self.entries.get(self.index) {
            buffer.insert_string(s.to_owned());
        }
    }
}

#[derive(Default)]
pub struct State {
    // TODO side by side visible buffers
    // visible_buffers: VisibleBuffers,
    buffers: Vec1<TextBuffer>,
    current_buffer_index: usize,
    find_replace_mode: FindReplaceMode,
    find: TextBuffer,
    replace: TextBuffer,
    screen: ScrollableScreen,
    font_info: FontInfo,
    clipboard_history: ClipboardHistory,
}

fn xy_to_pos(state: &State, xy: ScreenSpaceXY, round: PositionRound) -> Position {
    screen_space_to_position(
        xy,
        state.font_info.text_char_dim,
        state.screen.scroll,
        round,
    )
}

impl State {
    pub fn new() -> State {
        d!()
    }

    fn next_buffer(&mut self) {
        self.current_buffer_index = (self.current_buffer_index + 1) % self.buffers.len();
    }

    fn previous_buffer(&mut self) {
        self.current_buffer_index = if self.current_buffer_index == 0 {
            self.buffers.len() - 1
        } else {
            self.current_buffer_index - 1
        };
    }

    fn select_buffer(&mut self, i: usize) {
        if self.current_buffer_index < self.buffers.len() {
            self.current_buffer_index = i;
        }
    }

    fn add_or_select_buffer(&mut self, path: PathBuf, str: String) {
        let index = if let Some(index) = self.matching_buffer_index(path.as_ref()) {
            index
        } else {
            let index = self.buffers.len();
            self.buffers.push((BufferName::Path(path), str).into());
            index
        };

        self.current_buffer_index = index;
    }

    fn matching_buffer_index(&self, path: &Path) -> Option<usize> {
        for (i, buffer) in self.buffers.iter().enumerate() {
            match &buffer.name {
                BufferName::Path(p) => {
                    if ok_or!(p.canonicalize(), continue) == ok_or!(path.canonicalize(), continue) {
                        return Some(i);
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn next_scratch_buffer_number(&self) -> u32 {
        let mut output = 0;
        for b in self.buffers.iter() {
            match b.name {
                BufferName::Path(_) => continue,
                BufferName::Scratch(n) => {
                    output = std::cmp::max(output, n);
                }
            }
        }
        output.wrapping_add(1)
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

const AVERAGE_SELECTION_LNES_ESTIMATE: usize = 4;

fn to_buffer_view_data(buffer: &TextBuffer, selection_lines_estimate: usize) -> BufferViewData {
    let buffer_cursors = buffer.cursors();
    let cursors_len = buffer_cursors.len();
    let mut cursors = Vec::with_capacity(cursors_len);
    let mut highlights = Vec::with_capacity(cursors.len() * selection_lines_estimate);

    for c in buffer_cursors.iter() {
        let position = c.get_position();

        cursors.push(CursorView {
            position,
            state: c.state,
        });

        push_highlights(&mut highlights, position, c.get_highlight_position());
    }
    BufferViewData {
        chars: buffer.chars().collect::<String>(),
        cursors,
        highlights,
    }
}

#[perf_viz::record]
pub fn render_view(
    &State {
        ref buffers,
        ref screen,
        font_info: FontInfo { text_char_dim, .. },
        current_buffer_index,
        find_replace_mode,
        ref find,
        ref replace,
        ..
    }: &State,
    view: &mut View,
) {
    view.buffers.clear();

    for buffer in buffers.iter() {
        view.buffers.push(BufferView {
            name: buffer.name.clone(),
            name_string: buffer.name.to_string(),
            data: to_buffer_view_data(&buffer, AVERAGE_SELECTION_LNES_ESTIMATE),
        });
    }

    view.status_line.chars.clear();
    view.visible_buffers = d!();
    match buffers.get(current_buffer_index) {
        Some(buffer) => {
            view.visible_buffers[0] = Some(current_buffer_index);
            fn display_option_compactly<A: ToString>(op: Option<A>) -> String {
                match op {
                    None => "N".to_string(),
                    Some(a) => a.to_string(),
                }
            }

            use std::fmt::Write;
            let chars = &mut view.status_line.chars;

            let _cannot_actually_fail =
                write!(chars, "{}/{}", current_buffer_index + 1, buffers.len());

            // debugging
            let _cannot_actually_fail = write!(
                chars,
                "  ? t{} s{} w{}",
                text_char_dim, screen.scroll, screen.wh
            );

            for c in buffer.cursors().iter() {
                let _cannot_actually_fail = write!(
                    chars,
                    "{} {} ({}|{}), ",
                    c,
                    position_to_screen_space(c.get_position(), text_char_dim, screen.scroll),
                    display_option_compactly(buffer.find_index(c).and_then(|o| if o == 0 {
                        None
                    } else {
                        Some(o - 1)
                    })),
                    display_option_compactly(buffer.find_index(c)),
                );
            }
        }
        None => {
            view.status_line.chars.push_str(DEFAULT_STATUS_LINE_CHARS);
        }
    };

    const FIND_REPLACE_AVERAGE_SELECTION_LNES_ESTIMATE: usize = 1;

    view.find_replace = FindReplaceView {
        mode: find_replace_mode,
        find: to_buffer_view_data(&find, FIND_REPLACE_AVERAGE_SELECTION_LNES_ESTIMATE),
        replace: to_buffer_view_data(&replace, FIND_REPLACE_AVERAGE_SELECTION_LNES_ESTIMATE),
    };
    view.scroll = screen.scroll;
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
        position_to_text_space(target_cursor.get_position(), text_char_dim),
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
                state.font_info.text_char_dim,
                state.font_info.status_char_dim,
                &$buffer.cursors(),
            ); //TODO report non success result
        };
    }

    use Input::*;
    match input {
        Input::None => {}
        Quit => {}
        CloseMenuIfAny => match state.find_replace_mode {
            FindReplaceMode::CurrentFile => {
                state.find_replace_mode = FindReplaceMode::Hidden;
            }
            FindReplaceMode::Hidden => {}
        },
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
            buffer_call!(b.select_all());
            // We don't need to make sure a cursor is visible here since the user
            // will understand where the cursor is.
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
            set_if_present!(sizes => state.font_info);
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
        SetBufferPath(buffer_index, path) => {
            if let Some(b) = state.buffers.get_mut(buffer_index) {
                (*b).name = BufferName::Path(path);
            }
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
        LoadedFile(path, str) => {
            state.add_or_select_buffer(path, str);
            buffer_call!(b {
                try_to_show_cursors!(b);
            })
        }
        NewScratchBuffer => {
            let index = state.buffers.len();
            state
                .buffers
                .push((BufferName::Scratch(state.next_scratch_buffer_number()), "").into());

            state.current_buffer_index = index;
        }
        TabIn => {
            buffer_call!(b.tab_in());
        }
        TabOut => {
            buffer_call!(b.tab_out());
        }
        NextBuffer => {
            state.next_buffer();
        }
        PreviousBuffer => {
            state.previous_buffer();
        }
        SelectBuffer(i) => {
            state.select_buffer(i);
        }
        SetFindReplaceMode(mode) => {
            state.find_replace_mode = mode;
        }
    }

    let mut view = d!();

    render_view(state, &mut view);

    (view, cmd)
}
