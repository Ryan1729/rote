use editor_types::{Cursor, Vec1};
use macros::{d, ok_or, SaturatingAdd, SaturatingSub};
use platform_types::{screen_positioning::*, *};
use std::path::Path;
use std::path::PathBuf;

use std::collections::VecDeque;
use text_buffer::{get_search_ranges, next_instance_of_selected, TextBuffer};

mod editor_view;

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

#[derive(Debug, Default)]
struct ScrollableBuffer {
    text_buffer: TextBuffer,
    scroll: ScrollXY,
}

#[derive(Debug, Default)]
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

#[derive(Debug, Default)]
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

// We expect to have this function take a recursive directory traversal iterator later
fn find_in_paths<'path, I: Iterator<Item = &'path Path>>(
    paths: I,
    needle: &str,
) -> FileSwitcherResults {
    if needle.len() == 0 {
        return d!();
    }
    let len = {
        if let (_, Some(l)) = paths.size_hint() {
            l
        } else {
            128
        }
    };
    let mut output: FileSwitcherResults = Vec::with_capacity(len);

    for path in paths {
        let mut contains_needle = false;

        // This macro attempts to abstract over `Path::to_str` and `Path::to_string_lossy`
        // with a minimum of duplication
        macro_rules! cmp_match_indices {
            ($p1: expr, $p2: expr, $path_cmp: expr) => {{
                use std::cmp::Ordering::*;
                let mut p1_iter = $p1.match_indices(needle);
                let mut p2_iter = $p2.match_indices(needle);
                let mut backup_ordering = Equal;

                // TODO do we care that `match_indices` doesn't return overlapping matches?
                let mut p1_needle_count = 0;
                let mut p2_needle_count = 0;
                loop {
                    match (p1_iter.next(), p2_iter.next()) {
                        (Some((p1_i, _)), Some((p2_i, _))) => {
                            contains_needle = true;
                            backup_ordering = p1_i.cmp(&p2_i);

                            p1_needle_count += 1;
                            p2_needle_count += 1;
                        }
                        (Some(_), None) => {
                            contains_needle = true;
                            p1_needle_count += 1;
                            break;
                        }
                        (None, Some(_)) => {
                            p2_needle_count += 1;
                            break;
                        }
                        (None, None) => {
                            break;
                        }
                    }
                }

                p1_needle_count
                    .cmp(&p2_needle_count)
                    .then_with(|| backup_ordering)
                    .then_with(|| $path_cmp)
            }};
        }

        let i = if output.len() == 0 {
            // base case for when we don't havea second path to compare to.
            contains_needle = match path.to_str() {
                // Fast(er) path for valid unicde paths
                Some(s) => s.match_indices(needle).count() > 0,
                None => {
                    let s = path.to_string_lossy();
                    s.match_indices(needle).count() > 0
                }
            };
            0
        } else {
            output
                .binary_search_by(|p| {
                    match (path.to_str(), p.to_str()) {
                        // Fast(er) path for valid unicode paths
                        (Some(s1), Some(s2)) => cmp_match_indices!(s1, s2, p.as_path().cmp(path)),
                        (Some(s1), None) => {
                            let s2 = p.to_string_lossy();
                            cmp_match_indices!(s1, s2, p.as_path().cmp(path))
                        }
                        (None, Some(s2)) => {
                            let s1 = path.to_string_lossy();
                            cmp_match_indices!(s1, s2, p.as_path().cmp(path))
                        }
                        (None, None) => {
                            let s1 = path.to_string_lossy();
                            let s2 = p.to_string_lossy();
                            cmp_match_indices!(s1, s2, p.as_path().cmp(path))
                        }
                    }
                })
                .unwrap_or_else(|i| i)
        };

        if contains_needle {
            output.insert(i, path.to_path_buf());
        }
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_in_paths_works_on_this_small_example() {
        let needle = "b";

        let searched_paths = vec![
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\bartog.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beans.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\unrelated.txt"),
        ];

        let expected = vec![
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\bartog.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beans.txt"),
        ];

        assert_eq!(
            find_in_paths(searched_paths.iter().map(|p| p.as_path()), needle),
            expected
        );
    }

    #[test]
    fn find_in_paths_sorts_things_with_multiple_needles_higher() {
        let needle = "b";

        let searched_paths = vec![
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\basketball.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beans.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beebasketball.txt"),
        ];

        let expected = vec![
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beebasketball.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\basketball.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beans.txt"),
        ];

        assert_eq!(
            find_in_paths(searched_paths.iter().map(|p| p.as_path()), needle),
            expected
        );
    }
}

/// The collection of files opened for editing, and/or in-memory scratch buffers.
/// Guarenteed to have at least one buffer in it at all times.
#[derive(Default)]
struct EditorBuffers {
    buffers: Vec1<EditorBuffer>,
    index_state: g_i::State,
}

impl EditorBuffers {
    fn new(buffer: EditorBuffer) -> Self {
        Self {
            buffers: Vec1::new(buffer),
            ..d!()
        }
    }

    /// Since there is always at least one buffer, this always returns at least 1.
    fn len(&self) -> g_i::Length {
        debug_assert!(self.buffers.len() <= g_i::Length::max_value());
        g_i::Length::or_max(self.buffers.len())
    }

    /// The index of the last buffer
    fn last_index(&self) -> g_i::Index {
        let len: usize = self.len().into();
        self.index_state.new_index(g_i::IndexPart::or_max(len - 1))
    }

    fn get_mut(&mut self, index: g_i::Index) -> Option<&mut EditorBuffer> {
        index
            .get(self.index_state)
            .and_then(move |i| self.buffers.get_mut(i))
    }

    fn get(&self, index: g_i::Index) -> Option<&EditorBuffer> {
        index
            .get(self.index_state)
            .and_then(|i| self.buffers.get(i))
    }

    fn push(&mut self, buffer: EditorBuffer) {
        let will_fit = self.buffers.len() < g_i::Length::max_value();
        debug_assert!(will_fit);
        if will_fit {
            self.buffers.push(buffer);
        }
    }

    fn remove_if_present(&mut self, index: g_i::Index) -> Option<EditorBuffer> {
        if index < self.len() {
            dbg!(index.get(self.index_state)).and_then(|i| {
                let output = dbg!(self.buffers.try_remove(i).ok());

                if output.is_some() {
                    // No reason to update the index state if we didn't remove anything.
                    self.index_state.removed_at(index);
                    dbg!()
                }

                output
            })
        } else {
            None
        }
    }
}

struct IterWithIndexes<'iter> {
    index: g_i::Index,
    iter: std::slice::Iter<'iter, EditorBuffer>,
}

impl<'iter> Iterator for IterWithIndexes<'iter> {
    type Item = (g_i::Index, &'iter EditorBuffer);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|b| {
            let i = self.index.clone();

            self.index = self.index.saturating_add(1);

            (i, b)
        })
    }
}

impl EditorBuffers {
    fn iter(&self) -> std::slice::Iter<EditorBuffer> {
        self.buffers.iter()
    }

    fn iter_with_indexes(&self) -> IterWithIndexes {
        IterWithIndexes {
            index: d!(),
            iter: self.iter(),
        }
    }
}

#[derive(Default)]
pub struct State {
    // TODO side by side visible buffers
    // visible_buffers: VisibleBuffers,
    buffers: EditorBuffers,
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
            BufferIdKind::None => Option::None,
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
            #[allow(dead_code)]
            fn $name(&mut self, i: g_i::Index) {
                self.set_id(b_id!(BufferIdKind::$variant, i));
            }
        }
    };
}

set_indexed_id! {
    set_none_id,
    None
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
        self.set_id_index((self.current_buffer_id.index.saturating_add(1)) % self.buffers.len());
    }

    fn previous_buffer(&mut self) {
        let current_buffer_index = self.current_buffer_id.index;
        let i: usize = current_buffer_index.into();
        self.set_id_index(if i == 0 {
            self.buffers.last_index()
        } else {
            current_buffer_index.saturating_sub(1)
        });
    }

    fn set_id_index(&mut self, index: g_i::Index) {
        if index < self.buffers.len() {
            self.current_buffer_id.index = index;
        }
    }

    fn set_id(&mut self, id: BufferId) {
        if id.index < self.buffers.len() {
            self.current_buffer_id = id;

            if let Some(buffer) = get_scrollable_buffer_mut!(self) {
                // These need to be cleared so that the `platform_types::View` that is passed down
                // can be examined to detemine if the user wants to navigate away from the given
                // buffer
                buffer.text_buffer.reset_cursor_states();
            }
        }
    }

    fn add_or_select_buffer(&mut self, path: PathBuf, str: String) {
        let index = if let Some(index) = self.matching_buffer_index(path.as_ref()) {
            index
        } else {
            self.buffers
                .push(EditorBuffer::new(BufferName::Path(path), str));
            self.buffers.last_index()
        };

        self.set_text_id(index);
    }

    fn matching_buffer_index(&self, path: &Path) -> Option<g_i::Index> {
        for (i, buffer) in self.buffers.iter_with_indexes() {
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
        Self::char_dim_for_buffer_kind(&self.font_info, self.current_buffer_id.kind)
    }

    fn char_dim_for_buffer_kind(font_info: &FontInfo, kind: BufferIdKind) -> CharDim {
        use BufferIdKind::*;
        match kind {
            Text => font_info.text_char_dim,
            // None uses the same char_dim as the menus since it represents keybaord naviagaion in
            // the menus.
            None | Find | Replace | FileSwitcher => font_info.find_replace_char_dim,
        }
    }

    fn try_to_show_cursors_on(&mut self, id: BufferId) -> Option<()> {
        use BufferIdKind::*;

        let buffer = get_scrollable_buffer_mut!(self, id)?;
        let kind = id.kind;
        let xywh = match kind {
            None => return Option::None,
            Text => self.buffer_xywh,
            Find => self.find_xywh,
            Replace => self.replace_xywh,
            FileSwitcher => self.find_xywh, // TODO customize
        };
        let attempt_result = attempt_to_make_sure_at_least_one_cursor_is_visible(
            &mut buffer.scroll,
            xywh,
            Self::char_dim_for_buffer_kind(&self.font_info, kind),
            buffer.text_buffer.borrow_cursors_vec(),
        );

        match attempt_result {
            VisibilityAttemptResult::Succeeded => Some(()),
            _ => Option::None,
        }
    }

    fn set_menu_mode(&mut self, mode: MenuMode) {
        self.menu_mode = mode;
        match mode {
            MenuMode::FileSwitcher => {
                self.set_file_switcher_id(self.current_buffer_id.index);
            }
            MenuMode::FindReplace => {
                self.set_find_id(self.current_buffer_id.index);
            }
            MenuMode::Hidden => {
                dbg!(MenuMode::Hidden);
                self.set_text_id(self.current_buffer_id.index);
            }
        }
    }

    fn opened_paths(&self) -> Vec<&PathBuf> {
        let mut opened_paths: Vec<&PathBuf> = Vec::with_capacity(self.buffers.len().into());

        for buffer in self.buffers.iter() {
            match &buffer.name {
                BufferName::Path(p) => {
                    opened_paths.push(p);
                }
                BufferName::Scratch(_) => {}
            };
        }
        opened_paths
    }
}

pub fn new() -> State {
    d!()
}

impl From<String> for State {
    fn from(s: String) -> Self {
        let mut output: Self = d!();

        output.buffers = EditorBuffers::new(EditorBuffer::new(d!(), s));

        output
    }
}

impl From<&str> for State {
    fn from(s: &str) -> Self {
        let mut output: Self = d!();

        output.buffers = EditorBuffers::new(EditorBuffer::new(d!(), s));

        output
    }
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

    let mut edited_buffer_index: Option<g_i::Index> = Option::None;

    macro_rules! post_edit_sync {
        () => {
            match state.menu_mode {
                MenuMode::Hidden => {}
                MenuMode::FileSwitcher => {
                    let needle_string: String =
                        state.file_switcher.text_buffer.borrow_rope().into();
                    let needle_str: &str = &needle_string;
                    state.file_switcher_results =
                        find_in_paths(state.opened_paths().iter().map(|p| p.as_path()), needle_str);
                }
                MenuMode::FindReplace => {
                    let i = state.current_buffer_id.index;
                    if let Some(target_buffer) = state.buffers.get_mut(i) {
                        update_search_results(&state.find.text_buffer, target_buffer);
                    }
                }
            }
            try_to_show_cursors!();
            edited_buffer_index = Some(state.current_buffer_id.index);
        };
    }

    macro_rules! close_menu_if_any {
        () => {
            state.set_menu_mode(MenuMode::Hidden);
        };
    }

    use Input::*;
    match input {
        Input::None => {}
        Quit => {}
        CloseMenuIfAny => {
            close_menu_if_any!();
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
                        if let Some(pair) = next_instance_of_selected(b.text_buffer.borrow_rope(), &cursor) {
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
            state.buffers.push(EditorBuffer::new(
                BufferName::Scratch(state.next_scratch_buffer_number()),
                "",
            ));
            state.set_text_id(state.buffers.last_index());
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
            match id.kind {
                BufferIdKind::Text => {
                    close_menu_if_any!();
                }
                _ => {}
            }
        }
        OpenOrSelectBuffer(path) => {
            if let Some(id) = state
                .buffers
                .iter_with_indexes()
                .find(|(_, b)| match &b.name {
                    BufferName::Path(p) => *p == path,
                    BufferName::Scratch(_) => false,
                })
                .map(|(i, _)| b_id!(BufferIdKind::Text, i))
            {
                state.set_id(id);
                close_menu_if_any!();
            } else {
                cmd = Cmd::LoadFile(path);
            }
        }
        CloseBuffer(index) => {
            state.buffers.remove_if_present(index);

            let index_state = state.buffers.index_state;

            state.current_buffer_id.index = index_state
                .migrate(state.current_buffer_id.index)
                .or_else(|| index_state.migrate(state.current_buffer_id.index.saturating_sub(1)))
                .unwrap_or_else(||
                    // if the current index is zero and we remove it we end up pointing at the new
                    // first element. In this case, this is desired.
                    index_state.new_index(d!()));
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
        SetMenuMode(mode) => {
            state.set_menu_mode(mode);
        }
        SubmitForm => match state.current_buffer_id.kind {
            BufferIdKind::None | BufferIdKind::Text => {}
            BufferIdKind::Find => match state.find_replace_mode {
                FindReplaceMode::CurrentFile => {
                    let i = state.current_buffer_id.index;
                    if let Some(haystack) = state.buffers.get_mut(i) {
                        let needle = &state.find.text_buffer;
                        let needle_string: String = needle.into();
                        if needle_string == haystack.search_results.needle {
                            // advance to next search result
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
                post_edit_sync!();
            }
        },
    }
    dbg!(state.current_buffer_id);
    let mut view = d!();

    editor_view::render(state, &mut view);
    view.edited_buffer_index = edited_buffer_index;

    (view, cmd)
}
