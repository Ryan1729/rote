use editor_types::{Cursor, Vec1};
use macros::{d, ok_or};
use platform_types::{screen_positioning::*, *};
use std::path::Path;
use std::path::PathBuf;

use std::collections::VecDeque;
use text_buffer::{get_search_ranges, next_instance_of_selected, TextBuffer};

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
struct ScrollableBuffer {
    text_buffer: TextBuffer,
    scroll: ScrollXY,
}

#[derive(Default)]
struct EditorBuffer {
    scrollable: ScrollableBuffer,
    name: BufferName,
    search_results: SearchResults,
}

impl EditorBuffer {
    fn new<I: Into<TextBuffer>>(name: BufferName, s: I) -> Self {
        Self {
            name,
            scrollable: ScrollableBuffer {
                text_buffer: s.into(),
                ..d!()
            },
            ..d!()
        }
    }
}

#[derive(Default)]
struct SearchResults {
    needle: String,
    ranges: Vec<(Position, Position)>,
    current_range: usize,
}

fn update_search_results(needle: &TextBuffer, haystack: &mut EditorBuffer) {
    let ranges = get_search_ranges(
        needle.borrow_rope().full_slice(),
        &haystack.scrollable.text_buffer.borrow_rope(),
        d!(),
        d!(),
    );

    //TODO: Set `current_range` to something as close as possible to being on screen of haystack
    haystack.search_results = SearchResults {
        needle: needle.into(),
        ranges,
        current_range: 0,
    };
}

#[derive(Default)]
pub struct State {
    // TODO side by side visible buffers
    // visible_buffers: VisibleBuffers,
    buffers: Vec1<EditorBuffer>,
    buffer_xywh: TextBoxXYWH,
    current_buffer_id: BufferId,
    menu_mode: MenuMode,
    file_switcher: ScrollableBuffer,
    file_switcher_results: FileSwitcherResults,
    find_replace_mode: FindReplaceMode,
    find: ScrollableBuffer,
    find_xywh: TextBoxXYWH,
    replace: ScrollableBuffer,
    replace_xywh: TextBoxXYWH,
    font_info: FontInfo,
    clipboard_history: ClipboardHistory,
}

// this macro helps the borrow checker figure out that borrows are valid.
macro_rules! get_scrollable_buffer_mut {
    /* -> Option<&mut ScrollableBuffer> */
    ($state: expr) => {
        get_scrollable_buffer_mut!($state, $state.current_buffer_id)
    };
    ($state: expr, $id: expr) => {{
        let id = $id;
        match $id.kind {
            BufferIdKind::Text => $state.buffers.get_mut(id.index).map(|b| &mut b.scrollable),
            BufferIdKind::Find => Some(&mut $state.find),
            BufferIdKind::Replace => Some(&mut $state.replace),
            BufferIdKind::FileSwitcher => Some(&mut $state.file_switcher),
        }
    }};
}

macro_rules! set_indexed_id {
    ($name: ident, $variant: ident) => {
        impl State {
            fn $name(&mut self, i: usize) {
                if i < self.buffers.len() {
                    self.current_buffer_id = b_id!(BufferIdKind::$variant, i);
                }
            }
        }
    };
}

set_indexed_id! {
    set_text_id,
    Text
}
set_indexed_id! {
    set_find_id,
    Find
}
set_indexed_id! {
    set_replace_id,
    Replace
}
set_indexed_id! {
    set_file_switcher_id,
    FileSwitcher
}

impl State {
    pub fn new() -> State {
        d!()
    }

    fn next_buffer(&mut self) {
        self.set_text_id((self.current_buffer_id.index + 1) % self.buffers.len());
    }

    fn previous_buffer(&mut self) {
        let current_buffer_index = self.current_buffer_id.index;
        self.set_text_id(if current_buffer_index == 0 {
            self.buffers.len() - 1
        } else {
            current_buffer_index - 1
        });
    }

    fn set_id(&mut self, BufferId { kind, index: i }: BufferId) {
        match kind {
            BufferIdKind::Text => self.set_text_id(i),
            BufferIdKind::Find => self.set_find_id(i),
            BufferIdKind::Replace => self.set_replace_id(i),
            BufferIdKind::FileSwitcher => self.set_file_switcher_id(i),
        }
    }

    fn add_or_select_buffer(&mut self, path: PathBuf, str: String) {
        let index = if let Some(index) = self.matching_buffer_index(path.as_ref()) {
            index
        } else {
            let index = self.buffers.len();
            self.buffers
                .push(EditorBuffer::new(BufferName::Path(path), str));
            index
        };

        self.set_text_id(index);
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

    fn get_current_char_dim(&self) -> CharDim {
        use BufferIdKind::*;
        match self.current_buffer_id.kind {
            Text => self.font_info.text_char_dim,
            Find | Replace | FileSwitcher => self.font_info.find_replace_char_dim,
        }
    }

    fn try_to_show_cursors_on(&mut self, id: BufferId) -> Option<()> {
        use BufferIdKind::*;

        let buffer = get_scrollable_buffer_mut!(self, id)?;
        let kind = id.kind;
        let xywh = match kind {
            Text => self.buffer_xywh,
            Find => self.find_xywh,
            Replace => self.replace_xywh,
            FileSwitcher => self.find_xywh, // TODO customize
        };
        let attempt_result = attempt_to_make_sure_at_least_one_cursor_is_visible(
            &mut buffer.scroll,
            xywh,
            match kind {
                Text => self.font_info.text_char_dim,
                Find | Replace | FileSwitcher => self.font_info.find_replace_char_dim,
            },
            buffer.text_buffer.borrow_cursors_vec(),
        );

        match attempt_result {
            VisibilityAttemptResult::Succeeded => Some(()),
            _ => None,
        }
    }
}

pub fn new() -> State {
    d!()
}

impl From<String> for State {
    fn from(s: String) -> Self {
        let mut output: Self = d!();

        output.buffers = Vec1::new(EditorBuffer::new(d!(), s));

        output
    }
}

impl From<&str> for State {
    fn from(s: &str) -> Self {
        let mut output: Self = d!();

        output.buffers = Vec1::new(EditorBuffer::new(d!(), s));

        output
    }
}

const AVERAGE_SELECTION_LNES_ESTIMATE: usize = 4;

fn scrollable_to_buffer_view_data(
    scrollable: &ScrollableBuffer,
    selection_lines_estimate: usize,
) -> BufferViewData {
    let buffer = &scrollable.text_buffer;
    let buffer_cursors = buffer.borrow_cursors();
    let cursors_len = buffer_cursors.len();
    let mut cursors = Vec::with_capacity(cursors_len);
    let mut highlights = Vec::with_capacity(cursors.len() * selection_lines_estimate);

    for c in buffer_cursors.iter() {
        let position = c.get_position();

        cursors.push(CursorView {
            position,
            state: c.state,
        });

        push_highlights(&mut highlights, position, c.get_highlight_position(), d!());
    }

    BufferViewData {
        scroll: scrollable.scroll,
        chars: buffer.chars().collect::<String>(),
        cursors,
        highlights,
    }
}

fn editor_to_buffer_view_data(
    editor_buffer: &EditorBuffer,
    selection_lines_estimate: usize,
) -> BufferViewData {
    let mut buffer_view_data =
        scrollable_to_buffer_view_data(&editor_buffer.scrollable, selection_lines_estimate);

    let highlights = &mut buffer_view_data.highlights;
    let SearchResults {
        ref ranges,
        current_range,
        ..
    } = editor_buffer.search_results;
    for (i, &(p1, p2)) in ranges.iter().enumerate() {
        let kind = if i == current_range {
            HighlightKind::CurrentResult
        } else {
            HighlightKind::Result
        };
        push_highlights(highlights, p1, p2, kind);
    }

    buffer_view_data
}

#[perf_viz::record]
pub fn render_view(
    &State {
        ref buffers,
        font_info: FontInfo { text_char_dim, .. },
        current_buffer_id,
        menu_mode,
        ref file_switcher_results,
        ref file_switcher,
        find_replace_mode,
        ref find,
        ref replace,
        buffer_xywh: TextBoxXYWH {
            xy: text_box_pos, ..
        },
        ..
    }: &State,
    view: &mut View,
) {
    view.buffers.clear();

    for editor_buffer in buffers.iter() {
        let name = &editor_buffer.name;
        view.buffers.push(BufferView {
            name: name.clone(),
            name_string: name.to_string(),
            data: editor_to_buffer_view_data(&editor_buffer, AVERAGE_SELECTION_LNES_ESTIMATE),
        });
    }

    view.status_line.chars.clear();
    view.visible_buffers = d!();
    let current_buffer_index = current_buffer_id.index;
    match buffers.get(current_buffer_index) {
        Some(EditorBuffer {
            scrollable:
                ScrollableBuffer {
                    text_buffer: buffer,
                    scroll,
                },
            ..
        }) => {
            let scroll = *scroll;
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
            let _cannot_actually_fail = write!(chars, "  ? t{} s{}", text_char_dim, scroll);

            for c in buffer.borrow_cursors().iter() {
                let _cannot_actually_fail = write!(
                    chars,
                    "{} {} ({}|{}), ",
                    c,
                    position_to_screen_space(c.get_position(), text_char_dim, scroll, text_box_pos),
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

    const FIND_REPLACE_AVERAGE_SELECTION_LINES_ESTIMATE: usize = 1;

    view.menu = match menu_mode {
        MenuMode::Hidden => MenuView::None,
        MenuMode::FindReplace => MenuView::FindReplace(FindReplaceView {
            mode: find_replace_mode,
            find: scrollable_to_buffer_view_data(
                &find,
                FIND_REPLACE_AVERAGE_SELECTION_LINES_ESTIMATE,
            ),
            replace: scrollable_to_buffer_view_data(
                &replace,
                FIND_REPLACE_AVERAGE_SELECTION_LINES_ESTIMATE,
            ),
        }),
        MenuMode::FileSwitcher => {
            const FILE_SEARCH_SELECTION_LINES_ESTIMATE: usize = 1;
            MenuView::FileSwitcher(FileSwitcherView {
                search: scrollable_to_buffer_view_data(
                    &file_switcher,
                    FILE_SEARCH_SELECTION_LINES_ESTIMATE,
                ),
                results: file_switcher_results.clone(),
            })
        }
    };

    view.current_buffer_id = current_buffer_id;
}

fn attempt_to_make_sure_at_least_one_cursor_is_visible(
    scroll: &mut ScrollXY,
    xywh: TextBoxXYWH,
    text_char_dim: CharDim,
    cursors: &Vec1<Cursor>,
) -> VisibilityAttemptResult {
    let target_cursor = cursors.last();

    let apron: Apron = text_char_dim.into();

    attempt_to_make_xy_visible(
        scroll,
        xywh,
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
        ($buffer: ident $tokens:block) => {{
            if let Some($buffer) = get_scrollable_buffer_mut!(state) {
                $tokens;
            }
        }}
    }
    macro_rules! text_buffer_call {
        ($buffer: ident . $($method_call:tt)*) => {
            text_buffer_call!($buffer {$buffer.$($method_call)*})
        };
        ($buffer: ident $tokens:block) => {{
            if let Some($buffer) = get_scrollable_buffer_mut!(state).map(|b| &mut b.text_buffer) {
                $tokens;
            }
        }}
    }
    perf_viz::record_guard!("update_and_render");

    // if cfg!(debug_assertions)
    {
        if_changed::dbg!(&input);
    }

    let mut cmd = Cmd::NoCmd;

    macro_rules! try_to_show_cursors {
        () => {
            try_to_show_cursors!(state.current_buffer_id)
        };
        ($id: expr) => {
            state.try_to_show_cursors_on($id);
            // TODO trigger error popup based on result?
        };
    }

    macro_rules! post_edit_sync {
        () => {
            match state.menu_mode {
                MenuMode::Hidden => {}
                MenuMode::FileSwitcher => {
                    //TODO update file switcher results
                }
                MenuMode::FindReplace => {
                    let i = state.current_buffer_id.index;
                    if let Some(target_buffer) = state.buffers.get_mut(i) {
                        update_search_results(&state.find.text_buffer, target_buffer);
                    }
                }
            }
            try_to_show_cursors!();
        };
    }

    use Input::*;
    match input {
        Input::None => {}
        Quit => {}
        CloseMenuIfAny => {
            state.menu_mode = MenuMode::Hidden;
        }
        Insert(c) => buffer_call!(b{
            b.text_buffer.insert(c);
            post_edit_sync!();
        }),
        Delete => buffer_call!(b {
            b.text_buffer.delete();
            post_edit_sync!();
        }),
        Redo => buffer_call!(b {
            b.text_buffer.redo();
            post_edit_sync!();
        }),
        Undo => buffer_call!(b {
            b.text_buffer.undo();
            post_edit_sync!();
        }),
        MoveAllCursors(r#move) => {
            buffer_call!(b{
                b.text_buffer.move_all_cursors(r#move);
                try_to_show_cursors!();
            });
        }
        ExtendSelectionForAllCursors(r#move) => {
            buffer_call!(b{
                b.text_buffer.extend_selection_for_all_cursors(r#move);
                try_to_show_cursors!();
            });
        }
        SelectAll => {
            text_buffer_call!(b.select_all());
            // We don't need to make sure a cursor is visible here since the user
            // will understand where the cursor is.
        }
        ScrollVertically(amount) => buffer_call!(b{
            b.scroll.y -= amount;
        }),
        ScrollHorizontally(amount) => buffer_call!(b{
            b.scroll.x += amount;
        }),
        ResetScroll => buffer_call!(b{
            b.scroll = d!();
        }),
        SetSizeDependents(sds) => {
            set_if_present!(sds => state.font_info);
            set_if_present!(sds => state.buffer_xywh);
            set_if_present!(sds => state.find_xywh);
            set_if_present!(sds => state.replace_xywh);
        }
        SetCursor(xy, replace_or_add) => {
            let char_dim = state.get_current_char_dim();
            buffer_call!(b{
                let position = text_space_to_position(
                    text_box_to_text(xy, b.scroll),
                    char_dim,
                    PositionRound::Up,
                );

                b.text_buffer.set_cursor(position, replace_or_add);
            });
        }
        DragCursors(xy) => {
            let char_dim = state.get_current_char_dim();
            buffer_call!(b{
                let position = text_space_to_position(
                    text_box_to_text(xy, b.scroll),
                    char_dim,
                    PositionRound::Up,
                );
                // In practice we currently expect this to be sent only immeadately after an
                // `Input::SetCursors` input, so there will be only one cursor. But it seems like
                // we might as well just do it to all the cursors
                b.text_buffer.drag_cursors(position);
            })
        }
        SelectCharTypeGrouping(xy, replace_or_add) => {
            let char_dim = state.get_current_char_dim();
            buffer_call!(b{
                // We want different rounding for selections so that if we trigger a selection on the
                // right side of a character, we select that character rather than the next character.
                let position = text_space_to_position(
                    text_box_to_text(xy, b.scroll),
                    char_dim,
                    PositionRound::TowardsZero,
                );

                b.text_buffer.select_char_type_grouping(position, replace_or_add)
            })
        }
        ExtendSelectionWithSearch => {
            buffer_call!(b{
                let cursor = b.text_buffer.borrow_cursors().first().clone();
                match cursor.get_highlight_position() {
                    Option::None => {
                        b.text_buffer.select_char_type_grouping(cursor.get_position(), ReplaceOrAdd::Add);
                    }
                    Some(_) => {
                        dbg!();
                        if let Some(pair) = next_instance_of_selected(b.text_buffer.borrow_rope(), &cursor) {
                            dbg!();
                            b.text_buffer.set_cursor(pair, ReplaceOrAdd::Add);
                        }
                    }
                }
            });
        }
        SetBufferPath(buffer_index, path) => {
            if let Some(b) = state.buffers.get_mut(buffer_index) {
                (*b).name = BufferName::Path(path);
            }
        }
        Cut => buffer_call!(b {
            if let Some(s) = state.clipboard_history.cut(&mut b.text_buffer) {
                cmd = Cmd::SetClipboard(s);
            }
            post_edit_sync!();
        }),
        Copy => buffer_call!(b {
            if let Some(s) = state.clipboard_history.copy(&mut b.text_buffer) {
                cmd = Cmd::SetClipboard(s);
            }
            try_to_show_cursors!();
        }),
        Paste(op_s) => buffer_call!(b {
            state.clipboard_history.paste(&mut b.text_buffer, op_s);
            post_edit_sync!();
        }),
        InsertNumbersAtCursors => buffer_call!(b {
            b.text_buffer.insert_at_each_cursor(|i| i.to_string());
            post_edit_sync!();
        }),
        LoadedFile(path, str) => {
            state.add_or_select_buffer(path, str);
            post_edit_sync!();
        }
        NewScratchBuffer => {
            let index = state.buffers.len();
            state.buffers.push(EditorBuffer::new(
                BufferName::Scratch(state.next_scratch_buffer_number()),
                "",
            ));

            state.set_text_id(index);
        }
        TabIn => {
            text_buffer_call!(b.tab_in());
            post_edit_sync!();
        }
        TabOut => {
            text_buffer_call!(b.tab_out());
            post_edit_sync!();
        }
        NextBuffer => {
            state.next_buffer();
        }
        PreviousBuffer => {
            state.previous_buffer();
        }
        SelectBuffer(id) => {
            state.set_id(id);
        }
        SetFindReplaceMode(mode) => {
            let mut selections = d!();
            text_buffer_call!(b {
                selections = b.copy_selections();
            });
            state.set_find_id(state.current_buffer_id.index);
            state.find_replace_mode = mode;
            text_buffer_call!(b{
                if selections.len() == 1 {
                    b.insert_string(selections.swap_remove(0));
                }
                b.select_all()
                // We don't need to make sure a cursor is visible here since the user
                // will understand where the cursor is.
            });
            post_edit_sync!();
        }
        SubmitForm => match state.current_buffer_id.kind {
            BufferIdKind::Text => {}
            BufferIdKind::Find => match state.find_replace_mode {
                FindReplaceMode::CurrentFile => {
                    let i = state.current_buffer_id.index;
                    if let Some(haystack) = state.buffers.get_mut(i) {
                        let needle = &state.find.text_buffer;
                        let needle_string: String = needle.into();
                        if needle_string == haystack.search_results.needle {
                            if needle_string.len() > 0 {
                                let search_results = &mut haystack.search_results;
                                let len = search_results.ranges.len();
                                search_results.current_range += 1;
                                if search_results.current_range >= len {
                                    search_results.current_range = 0;
                                }

                                if let Some(pair) = haystack
                                    .search_results
                                    .ranges
                                    .get(haystack.search_results.current_range)
                                {
                                    let c: Cursor = pair.into();
                                    haystack
                                        .scrollable
                                        .text_buffer
                                        .set_cursor(c, ReplaceOrAdd::Replace);
                                    try_to_show_cursors!(b_id!(BufferIdKind::Text, i));
                                }
                            }
                        } else {
                            update_search_results(needle, haystack);
                        }
                        try_to_show_cursors!();
                    }
                }
            },
            BufferIdKind::Replace => {
                let i = state.current_buffer_id.index;
                dbg!("TODO BufferIdKind::Replace {}", i);
            }
            BufferIdKind::FileSwitcher => {
                let i = state.current_buffer_id.index;
                dbg!("TODO BufferIdKind::FileSwitcher {}", i);
            }
        },
    }

    let mut view = d!();

    render_view(state, &mut view);

    (view, cmd)
}
