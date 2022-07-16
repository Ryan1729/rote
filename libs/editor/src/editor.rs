#![deny(clippy::float_arithmetic)]

use macros::{d, dbg, fmt_debug, u, SaturatingSub};
use platform_types::{screen_positioning::*, *};
use parsers::{Parsers, ParserKind};
use vec1::{Vec1};

use std::{
    path::PathBuf,
    time::Instant,
};
use text_buffer::{
    Editedness,
    PossibleEditedTransition,
    ScrollAdjustSpec,
    TextBuffer,
    SizeInfo,
    ppel,
};

mod editor_view;
mod editor_buffers;
use editor_buffers::{
    EditorBuffers,
    EditorBuffer
};

mod clipboard_history {
    use super::*;
    use std::collections::VecDeque;

    #[derive(Debug, PartialEq, Eq)]
    enum Entry {
        /// This is the by far more common case, so making it separate often saves
        /// an allocation/level of indirection.
        Single(String),
        Multiple(Vec1<String>)
    }

    impl Entry {
        fn loose_equal(&self, other: &Entry) -> bool {
            use Entry::*;
            match (self, other) {
                (Single(single), Multiple(strings))
                | (Multiple(strings), Single(single)) => {
                    let mut str_index = 0;
                    let mut multi_chars = match strings.get(str_index) {
                        None => return single.is_empty(),
                        Some(s) => s.chars(),
                    };

                    for c in single.chars() {
                        if c == '\n' {
                            str_index += 1;
                            multi_chars = match strings.get(str_index) {
                                None => return false,
                                Some(s) => s.chars(),
                            };
                        } else {
                            let next_char = multi_chars.next();
                            if next_char != Some(c) {
                                return false;
                            }
                        }
                    }

                    str_index == strings.len() - 1
                },
                _ => self == other
            }
        }
    }

    #[test]
    fn loose_equal_works_on_these_examples() {
        use Entry::*;
        use vec1::{vec1};

        impl Entry {
            fn loose_equal_slow(&self, other: &Entry) -> bool {
                match (self, other) {
                    (Single(s), Multiple(strings))
                    | (Multiple(strings), Single(s)) => {
                        // The obviously correct but slow version.
                        &strings.join("\n") == s
                    },
                    _ => self == other
                }
            }
        }

        fn single(s: &'static str) -> Entry {
            Single(s.to_owned())
        }

        fn multiple(v1: Vec1<&'static str>) -> Entry {
            let v = v1.into_iter()
                    .map(|s| s.to_owned())
                    .collect::<Vec<_>>();

            Multiple(
                TryFrom::try_from(v).unwrap()
            )
        }

        // Short for assert. We can be this brief becasue this is lexically scoped.
        macro_rules! a {
            ($a: expr, $b: expr) => {
                let a = $a;
                let b = $b;
                assert_eq!(
                    a.loose_equal(&b),
                    a.loose_equal_slow(&b),
                );
            }
        }

        a!(single(""), single(""));
        a!(single(""), multiple(vec1![""]));
        a!(single("a"), single("a"));
        a!(multiple(vec1!["a", "b"]), multiple(vec1!["a", "b"]));
        a!(single("a\nb"), multiple(vec1!["a", "b"]));
        a!(single("ab"), multiple(vec1!["a", "b"]));
        a!(single("a\nb\n"), multiple(vec1!["a", "b"]));
        a!(single("\na\nb\n"), multiple(vec1!["a", "b"]));
        a!(single("\na\nb"), multiple(vec1!["a", "b"]));
    }

    #[derive(Debug, Default, PartialEq, Eq)]
    pub struct ClipboardHistory {
        entries: VecDeque<Entry>,
        index: usize,
    }

    const AVERAGE_SELECTION_SIZE_ESTIMATE: usize = 32;

    impl ClipboardHistory {
        pub fn cut(&mut self, buffer: &mut TextBuffer, listener: ppel!()) -> (Option<String>, PossibleEditedTransition) {
            let (selections, transition) = buffer.cut_selections(listener);

            let joined_selections = self.push_and_join_into_option(selections);

            (joined_selections, transition)
        }
        pub fn copy(&mut self, buffer: &TextBuffer) -> Option<String> {
            self.push_and_join_into_option(buffer.copy_selections())
        }
        pub fn paste(
            &mut self,
            buffer: &mut TextBuffer,
            possible_string: Option<String>,
            listener: ppel!(),
        ) -> Option<EditedTransition> {
            let mut output = None;

            if let Some(s) = possible_string {
                self.push_if_does_not_match_top(Entry::Single(s));
            }

            if let Some(entry) = self.entries.get(self.index) {
                use Entry::*;
                output = match entry {
                    Single(ref s) => buffer.insert_string(s.clone(), listener),
                    Multiple(ref strings) => {
                        let cursor_count = buffer.borrow_cursors().len();
                        let len = strings.len();

                        if len == cursor_count {
                            buffer.insert_at_each_cursor(
                                // We just checked the len matches the cursor count
                                // so this should never panic.
                                |index| strings[index].clone(),
                                listener
                            )
                        } else {
                            buffer.insert_string(strings.join("\n"), listener)
                        }
                    }
                };
            }

            output
        }

        fn push_and_join_into_option(
            &mut self,
            strings: Vec<String>
        ) -> Option<String> {
            let res: Result<Vec1<_>, _> = TryFrom::try_from(strings);
            match res {
                Err(_) => None,
                Ok(strings) => {
                    let mut output = String::with_capacity(
                        strings.len() * AVERAGE_SELECTION_SIZE_ESTIMATE
                    );

                    let mut sep = "";
                    for s in &strings {
                        output.push_str(sep);

                        output.push_str(s);

                        self.push_if_does_not_match_top(Entry::Single(s.clone()));

                        sep = "\n";
                    }

                    self.push_if_does_not_match_top(Entry::Multiple(strings));

                    Some(output)
                }
            }
        }

        fn push_if_does_not_match_top(&mut self, to_push: Entry) {
            match self.entries
                .get(self.index)
                .map(
                    |e| !Entry::loose_equal(e, &to_push)
                )
            {
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
    }
}
use clipboard_history::ClipboardHistory;

#[derive(Default)]
pub struct State {
    buffers: EditorBuffers,
    buffer_xywh: TextBoxXYWH,
    current_buffer_kind: BufferIdKind,
    menu_mode: MenuMode,
    file_switcher: TextBuffer,
    file_switcher_results: FileSwitcherResults,
    find: TextBuffer,
    find_xywh: TextBoxXYWH,
    replace: TextBuffer,
    replace_xywh: TextBoxXYWH,
    go_to_position: TextBuffer,
    go_to_position_xywh: TextBoxXYWH,
    font_info: FontInfo,
    clipboard_history: ClipboardHistory,
    parsers: parsers::Parsers,
    view: View,
}

fmt_debug!(
    collapse default for State: me {
        blank_if_default!(buffers);
        blank_if_default!(buffer_xywh);
        blank_if_default!(current_buffer_kind);
        blank_if_default!(menu_mode);
        blank_if_default!(file_switcher);
        blank_if_default!(
            file_switcher_results,
            me.file_switcher_results == <Vec<PathBuf> as Default>::default()
        );
        blank_if_default!(find);
        blank_if_default!(find_xywh);
        blank_if_default!(replace);
        blank_if_default!(replace_xywh);
        blank_if_default!(go_to_position);
        blank_if_default!(go_to_position_xywh);
        blank_if_default!(font_info);
        blank_if_default!(clipboard_history);
        blank_if_default!(
            parsers,
            matches!(me.parsers, Parsers::NotInitializedYet)
        );
        blank_if_default!(view);
    }
);

// this macro helps the borrow checker figure out that borrows are valid.
macro_rules! get_text_buffer_mut {
    /* -> Option<&mut TextBuffer> */
    ($state: expr) => {{
        get_text_buffer_mut!($state, $state.current_buffer_kind)
    }};
    ($state: expr, $kind: expr) => {{
        u!{BufferIdKind}
        match $kind {
            None => Option::None,
            Text => Some(&mut $state.buffers.get_current_buffer_mut().text_buffer),
            Find => Some(&mut $state.find),
            Replace => Some(&mut $state.replace),
            FileSwitcher => Some(&mut $state.file_switcher),
            GoToPosition => Some(&mut $state.go_to_position),
        }
    }};
    /* -> Option<(&mut TextBuffer, PossibleParserEditListener)> */
    ($state: expr ; listener) => {{
        u!{BufferIdKind}
        match $state.current_buffer_kind {
            None => Option::None,
            Text => {
                let buffer = $state.buffers.get_current_buffer_mut();
                let parser_kind = buffer.parser_kind.unwrap_or_else(||
                    ParserKind::default_from_name(&buffer.name)
                );
                Some((
                    &mut buffer.text_buffer,
                    Some(text_buffer::ParserEditListener {
                        buffer_name: &buffer.name,
                        parser_kind,
                        parsers: &mut $state.parsers,
                    })
                ))
            },
            Find => Some((&mut $state.find, Option::None)),
            Replace => Some((&mut $state.replace, Option::None)),
            FileSwitcher => Some((&mut $state.file_switcher, Option::None)),
            GoToPosition => Some((&mut $state.go_to_position, Option::None)),
        }
    }}
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
set_indexed_id! {
    set_go_to_position_id,
    GoToPosition
}

fn direct_scroll_from(r#move: Move) -> Option<ScrollXY> {
    u!{Move}
    match r#move {
        Up |
        Down |
        Right |
        ToLineEnd |
        ToBufferEnd |
        ToNextLikelyEditLocation |
        Left |
        ToPreviousLikelyEditLocation |
        ToLineStart => None,
        ToBufferStart => {
            Some(d!())
        }
    }
}

impl State {
    pub fn new() -> State {
        d!()
    }

    fn close_buffer(&mut self, index: g_i::Index) {
        self.buffers.close_buffer(index, &mut self.parsers);
    }

    fn set_id(&mut self, id: BufferId) {
        if kind_editable_during_mode(id.kind, self.menu_mode) {
            self.current_buffer_kind = id.kind;

            if let Some(buffer) = get_text_buffer_mut!(self) {
                // These need to be cleared so that the `platform_types::View` that is passed down
                // can be examined to detemine if the user wants to navigate away from the given
                // buffer. We do this with each buffer, even though a client might only care about
                // buffers of a given menu kind, since a different client might care about different
                // ones, including plain `Text` buffers.
                buffer.reset_cursor_states();
            }

            self.buffers.set_current_index(id.index);
        }
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
        Self::char_dim_for_buffer_kind(&self.font_info, self.current_buffer_kind)
    }

    fn char_dim_for_buffer_kind(font_info: &FontInfo, kind: BufferIdKind) -> CharDim {
        u!{BufferIdKind}
        match kind {
            Text => font_info.text_char_dim,
            // None uses the same char_dim as the menus since it represents keyboard navigation in
            // the menus.
            None | Find | Replace | FileSwitcher | GoToPosition => font_info.find_replace_char_dim,
        }
    }

    fn size_info(&self, kind: BufferIdKind) -> Option<SizeInfo> {
        u!{BufferIdKind}
        let char_dim = State::char_dim_for_buffer_kind(&self.font_info, kind);

        let xywh = match kind {
            None => return Option::None,
            Text => self.buffer_xywh,
            Find => self.find_xywh,
            Replace => self.replace_xywh,
            FileSwitcher => self.find_xywh, // TODO customize
            GoToPosition => self.go_to_position_xywh,
        };

        Some(SizeInfo{ char_dim, xywh })
    }

    fn try_to_show_cursors_on(&mut self, kind: BufferIdKind) -> Option<()> {
        let size_info = self.size_info(kind)?;
        let buffer = get_text_buffer_mut!(self, kind)?;

        let attempt_result = buffer.try_to_show_cursors_on(
            ScrollAdjustSpec::Calculate(
                size_info,
                buffer.borrow_cursors().last().get_position()
            )
        );
        match attempt_result {
            VisibilityAttemptResult::Succeeded => Some(()),
            // TODO remove VisibilityAttemptResult once we are sure we don't need it.
            #[allow(unreachable_patterns)]
            _ => Option::None,
        }
    }

    fn set_menu_mode(&mut self, mode: MenuMode) {
        let current_index = self.buffers.current_index();

        self.menu_mode = mode;
        match mode {
            MenuMode::GoToPosition => {
                self.set_go_to_position_id(current_index);
            }
            MenuMode::FileSwitcher => {
                self.set_file_switcher_id(current_index);
            }
            MenuMode::FindReplace(_) => {
                self.set_find_id(current_index);
            }
            MenuMode::Hidden => {
                self.set_text_id(current_index);
            }
        }
    }

    fn find_replace_mode(&mut self) -> Option<FindReplaceMode> {
        match self.menu_mode {
            MenuMode::FindReplace(mode) => {
                Some(mode)
            }
            MenuMode::GoToPosition | MenuMode::FileSwitcher | MenuMode::Hidden => {
                None
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

fn parse_for_go_to_position(input: &str) -> Result<Position, std::num::ParseIntError> {
   input.parse::<Position>().map(|p| {
        // The largest use of jumping to a position is to jump to line numbers emitted
        // by an error message. Most line numbers like this start at one, so we need
        // to counteract that.
        pos!{
            l p.line.saturating_sub(1),
            o p.offset.saturating_sub(1).0
        }
   })
}

macro_rules! set_if_present {
    ($source:ident => $target:ident.$field:ident) => {
        if let Some($field) = $source.$field {
            $target.$field = $field;
        }
    };
}

pub fn load_buffer_view(
    State {
        ref buffers,
        ref mut view,
        ref mut parsers,
        ..
    }: &mut State,
    buffer_name: &BufferName
) -> Result<BufferView, LoadBufferViewError> {
    buffers
        .index_with_name(&buffer_name)
        .ok_or_else(||
            format!(
                "Could not find index for buffer named \"{}\" in {} buffers",
                buffer_name,
                buffers.len()
            )
        )
        .and_then(|index| {
            buffers.buffers().get(index).ok_or_else(||
                format!(
                    "Got bad index {} for buffer named \"{}\" in {} buffers",
                    index,
                    buffer_name,
                    buffers.len()
                )
            )
        })
        .map(|buffer| {
            let view_stats = &mut view.stats;

            BufferView {
                label: BufferLabel {
                    name: buffer.name.clone(),
                    name_string: buffer.name.to_string()
                },
                data: editor_view::editor_to_buffer_view_data(
                    view_stats,
                    parsers,
                    buffer
                )
            }
        })
}

pub fn update_and_render(state: &mut State, input: Input) -> UpdateAndRenderOutput {
    perf_viz::record_guard!("update_and_render");
    let start_time = Instant::now();
    state.view.stats = d!();

    macro_rules! try_to_show_cursors {
        () => {
            try_to_show_cursors!(state.current_buffer_kind);
        };
        ($kind: expr) => {
            state.try_to_show_cursors_on($kind);
            // TODO trigger error popup based on result?
        };
        ($kind: expr, $scroll_op: expr) => {
            if let Some(scroll) = $scroll_op {
                if let Some(buffer) = get_text_buffer_mut!(state, $kind) {
                    buffer.try_to_show_cursors_on(ScrollAdjustSpec::Direct(scroll));
                }
            } else {
                try_to_show_cursors!($kind);
            }
        };
    }

    macro_rules! buffer_view_sync {
        () => {{
            perf_viz::record_guard!("buffer_view_sync");
            match state.menu_mode {
                MenuMode::Hidden => {
                    state.buffers.get_current_buffer_mut().refresh_search_results(
                        (&state.find).into(),
                    );
                }
                MenuMode::FindReplace(_) => {
                    state.buffers.get_current_buffer_mut().advance_or_refresh_search_results(
                        (&state.find).into(),
                    );
                }
                MenuMode::FileSwitcher => {
                    let needle_string: String =
                        state.file_switcher.borrow_rope().into();
                    let needle_str: &str = &needle_string;
                    let opened_paths = state.opened_paths();
                    state.file_switcher_results =
                        if needle_str.is_empty() {
                            opened_paths.into_iter().map(|p| p.to_owned()).collect()
                        } else {
                            paths::find_in(opened_paths.iter().map(|p| p.as_path()), needle_str)
                        };
                }
                MenuMode::GoToPosition => {}
            }
            try_to_show_cursors!();
        }};
    }

    macro_rules! post_edit_sync {
        () => {{
            perf_viz::record_guard!("post_edit_sync");
            buffer_view_sync!();
        }};
    }

    macro_rules! editor_buffer_call {
        (sync $buffer: ident . $($method_call:tt)*) => {
            editor_buffer_call!($buffer . $($method_call)*);
            post_edit_sync!();
        };
        ($buffer: ident . $($method_call:tt)*) => {
            editor_buffer_call!($buffer {$buffer.$($method_call)*})
        };
        (sync $buffer: ident $tokens:block) => {{
            editor_buffer_call!($buffer $tokens)
            post_edit_sync!();
        }};
        ($buffer: ident $tokens:block) => {{
            let $buffer = state.buffers.get_current_buffer_mut();
            $tokens;
        }}
    }

    macro_rules! text_buffer_call {
        (sync $buffer: ident . $($method_call:tt)*) => {
            text_buffer_call!($buffer . $($method_call)*);
            post_edit_sync!();
        };
        ($buffer: ident . $($method_call:tt)*) => {
            text_buffer_call!($buffer {$buffer.$($method_call)*})
        };
        (sync $buffer: ident $(,)? $listener: ident $tokens:block) => {{
            if let Some((
                $buffer,
                $listener
            )) = get_text_buffer_mut!(state; listener) {
                $tokens;
            }
            post_edit_sync!();
        }};
        ($buffer: ident $tokens:block) => {{
            if let Some($buffer) = get_text_buffer_mut!(state) {
                $tokens;
            }
        }}
    }

    if cfg!(feature = "extra-prints")
    {
        if_changed::dbg!(&input);
    }

    let mut cmd = Cmd::None;

    macro_rules! close_menu_if_any {
        () => {
            state.set_menu_mode(MenuMode::Hidden);
        };
    }

    state.view.edited_transitions.clear();

    u!{EditedTransition, Editedness};

    macro_rules! mark_edited_transition {
        (current, $transition: expr) => {
            mark_edited_transition!(
                state.buffers.current_index(),
                $transition
            )
        };
        (append, $transition: expr) => {
            mark_edited_transition!(
                state.buffers.append_index(),
                $transition
            )
        };
        ($index: expr, $transition: expr) => {{
            // Since this may be an expression with side effects,
            // we want this to be evaluated whether or not we want
            // to record the transition.
            let transition = $transition;
            if state.current_buffer_kind == BufferIdKind::Text {
                let transition: Option<EditedTransition> = transition.into();

                if let Some(transition) = transition {
                    state.view.edited_transitions.push((
                        $index,
                        transition,
                    ));
                }
            }
        }};
    }

    macro_rules! new_scratch_buffer {
        ($data_option: expr) => {
            let e_b = EditorBuffer::new(
                BufferName::Scratch(state.next_scratch_buffer_number()),
                $data_option.unwrap_or_default(),
            );

            let editedness = e_b.text_buffer.editedness();

            state.buffers.push_and_select_new(e_b);
            state.current_buffer_kind = BufferIdKind::Text;

            mark_edited_transition!(current, match editedness {
                Edited => ToEdited,
                Unedited => ToUnedited,
            });
        }
    }

    macro_rules! add_or_select {
        ($name: expr, $str: expr) => {{
            let name = $name;
            let str = $str;
            perf_viz::record_guard!("AddOrSelectBuffer");
            let edited_transition_opt = state.buffers.add_or_select_buffer(name, str);
            state.current_buffer_kind = BufferIdKind::Text;

            buffer_view_sync!();
            if let Some(edited_transition) = edited_transition_opt {
                // We need to announce this so that the user can just track the
                // transitions and have an accurate notion of which buffers exist.
                mark_edited_transition!(current, edited_transition);
            }
        }}
    }

    macro_rules! go_to_position {
        ($position: expr) => {{
            state.buffers.get_current_buffer_mut().text_buffer.set_cursor($position, ReplaceOrAdd::Replace);
            state.set_menu_mode(MenuMode::Hidden);
            try_to_show_cursors!();
        }}
    }

    state.view.stats.latest_update_time_span = TimeSpan::start();

    u!{Input}
    match input {
        Input::None => {}
        Quit => {}
        Escape => {
            if state.menu_mode == MenuMode::Hidden {
                // Note that we only want to collapse cursors if the menu is already
                // closed.
                text_buffer_call!(sync b, _l {
                    b.collapse_cursors(
                        state.font_info.text_char_dim,
                        state.buffer_xywh
                    )
                })
            } else {
                close_menu_if_any!();
            }
        }
        Insert(c) => text_buffer_call!(sync b, l {
            mark_edited_transition!(current, b.insert(c, l));
        }),
        Delete => text_buffer_call!(sync b, l {
            mark_edited_transition!(current, b.delete(l));
        }),
        DeleteLines => text_buffer_call!(sync b, l {
            mark_edited_transition!(current, b.delete_lines(l));
        }),
        Redo => text_buffer_call!(sync b, l {
            mark_edited_transition!(current, b.redo(l));
        }),
        Undo => text_buffer_call!(sync b, l {
            mark_edited_transition!(current, b.undo(l));
        }),
        MoveAllCursors(r#move) => {
            text_buffer_call!(b{
                b.move_all_cursors(r#move);
                try_to_show_cursors!(
                    state.current_buffer_kind,
                    direct_scroll_from(r#move)
                );
            });
        }
        ExtendSelectionForAllCursors(r#move) => {
            text_buffer_call!(b{
                b.extend_selection_for_all_cursors(r#move);
                try_to_show_cursors!(
                    state.current_buffer_kind,
                    direct_scroll_from(r#move)
                );
            });
        }
        SelectAll => {
            text_buffer_call!(b.select_all());
            // We don't need to make sure a cursor is visible here since the user
            // will understand where the cursor is.
        }
        ScrollVertically(amount) => text_buffer_call!(b{
            b.scroll.y -= amount;
        }),
        ScrollHorizontally(amount) => text_buffer_call!(b{
            b.scroll.x += amount;
        }),
        ResetScroll => text_buffer_call!(b{
            b.scroll = d!();
        }),
        SetSizeDependents(sds) => {
            set_if_present!(sds => state.font_info);
            set_if_present!(sds => state.buffer_xywh);
            set_if_present!(sds => state.find_xywh);
            set_if_present!(sds => state.replace_xywh);
            set_if_present!(sds => state.go_to_position_xywh);
        }
        SetCursor(xy, replace_or_add) => {
            let char_dim = state.get_current_char_dim();
            text_buffer_call!(b{
                let position = text_space_to_position(
                    text_box_to_text(xy, b.scroll),
                    char_dim,
                    PositionRound::Up,
                );

                b.set_cursor(position, replace_or_add);
            });
        }
        DragCursors(xy) => {
            let char_dim = state.get_current_char_dim();
            text_buffer_call!(b{
                let position = text_space_to_position(
                    text_box_to_text(xy, b.scroll),
                    char_dim,
                    PositionRound::Up,
                );
                // In practice we currently expect this to be sent only immeadately after an
                // `Input::SetCursors` input, so there will be only one cursor. But it seems like
                // we might as well just do it to all the cursors
                b.drag_cursors(position);
            })
        }
        SelectCharTypeGrouping(xy, replace_or_add) => {
            let char_dim = state.get_current_char_dim();
            text_buffer_call!(b{
                b.select_char_type_grouping(
                    b.xy_to_position(
                        char_dim,
                        xy,
                    ),
                    replace_or_add
                )
            })
        }
        ExtendSelectionWithSearch => {
            if let Some(size_info) = state.size_info(state.current_buffer_kind) {
                text_buffer_call!(b.extend_selection_with_search(
                    size_info
                ));
            }
        }
        ExtendSelectionMaximallyWithSearch => {
            if let Some(size_info) = state.size_info(state.current_buffer_kind) {
                text_buffer_call!(b.extend_selection_maximally_with_search(
                    size_info
                ));
            }
        }
        SavedAs(buffer_index, path) => {
            if let Some(()) = state.buffers.saved_as(buffer_index, path) {
                mark_edited_transition!(buffer_index, ToUnedited);
            }
        }
        Cut => text_buffer_call!(sync b, l {
            let (s, transition) = state.clipboard_history.cut(b, l);
            if let Some(s) = s {
                cmd = Cmd::SetClipboard(s);
            }

            if let Some(transition) = transition {
                mark_edited_transition!(current, transition);
            }
        }),
        Copy => text_buffer_call!(b {
            if let Some(s) = state.clipboard_history.copy(b) {
                cmd = Cmd::SetClipboard(s);
            }
            try_to_show_cursors!();
        }),
        Paste(op_s) => text_buffer_call!(sync b, l {
            mark_edited_transition!(
                current,
                state.clipboard_history.paste(b, op_s, l)
            );
        }),
        InsertNumbersAtCursors => text_buffer_call!(sync b, l {
            mark_edited_transition!(
                current,
                b.insert_at_each_cursor(|i| i.to_string(), l)
            );
        }),
        NewScratchBuffer(data_op) => {
            new_scratch_buffer!(data_op);
        }
        TabIn => {
            text_buffer_call!(sync b, l {
                mark_edited_transition!(current, b.tab_in(l));
            });
        }
        TabOut => {
            text_buffer_call!(sync b, l {
                mark_edited_transition!(current, b.tab_out(l));
            });
        }
        StripTrailingWhitespace => {
            text_buffer_call!(sync b, l {
                mark_edited_transition!(current, b.strip_trailing_whitespace(l));
            });
        }
        AddOrSelectBuffer(name, str) => {
            add_or_select!(name, str);
        }
        AddOrSelectBufferThenGoTo(name, str, position) => {
            add_or_select!(name, str);

            go_to_position!(position);
        }
        AdjustBufferSelection(adjustment) => {
            dbg!(&state.buffers);
            dbg!(&state.view.buffers);
            state.buffers.adjust_selection(adjustment);

            close_menu_if_any!();
        }
        SelectBuffer(id) => {
            state.set_id(id);
            if let BufferIdKind::Text = id.kind {
                close_menu_if_any!();
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
                // No need to mark the edited transition here since we will do
                // that in the `AddOrSelectBuffer` case when the file actualy arrives.
            }
        }
        CloseBuffer(index) => {
            state.close_buffer(index);
        }
        SetMenuMode(mode) => {
            if mode == MenuMode::Hidden {
                state.set_menu_mode(mode);
            } else {
                let mut selections = d!();

                text_buffer_call!(b {
                    selections = b.copy_selections();
                });

                let mut selection: Option<String> = if selections.len() == 1 {
                    Some(selections.swap_remove(0))
                } else {
                    Option::None
                };

                state.set_menu_mode(mode);

                selection = match (selection, mode) {
                    (Option::None, _) => {Option::None}
                    (Some(selection), MenuMode::GoToPosition) => {
                        if parse_for_go_to_position(&selection).is_err() {
                            Option::None
                        } else {
                            Some(selection)
                        }
                    }
                    (s, _) => {s}
                };

                // We know that this is a non-editor buffer, so we don't need to
                // care about edited transitions at the current time.
                text_buffer_call!(b {
                    if let Some(selection) = selection {
                        // For the small menu buffers I'd rather keep all the history
                        // even if the history order is confusing, since the text
                        // entered is usually small ... kind of like a shell prompt.
                        const UPPER_LOOP_BOUND: usize = 1024;
                        // Use a bound like this just in case, since we do actually
                        // proiritize a sufficently quick response over a perfect history
                        for _ in 0..UPPER_LOOP_BOUND {
                            // Since we know this is a non-editor buffer, we also
                            // know the parsers don't need to hear about changes
                            // to it. That's why the listener parameter can be None.
                            if b.redo(Option::None).ran_out_of_history() {
                                break;
                            }
                        }

                        b.select_all();
                        // See note above about listener parameter.
                        b.insert_string(selection, Option::None);
                    }

                    b.select_all();
                    // We don't need to make sure a cursor is visible here since the user
                    // will understand where the cursor is.
                });
            }
        }
        NextLanguage => {
            editor_buffer_call!(b.next_language());
        }
        PreviousLanguage => {
            editor_buffer_call!(b.previous_language());
        }
        ToggleSingleLineComments => {
            text_buffer_call!(sync b, l {
                mark_edited_transition!(current, b.toggle_single_line_comments(l));
            });
        }
        ToggleCase => {
            text_buffer_call!(sync b, l {
                mark_edited_transition!(current, b.toggle_case(l));
            });
        }
        DuplicateLines => {
            text_buffer_call!(sync b, l {
                mark_edited_transition!(current, b.duplicate_lines(l));
            });
        }
        SubmitForm => match state.current_buffer_kind {
            BufferIdKind::None | BufferIdKind::Text => {}
            BufferIdKind::Find => match state.find_replace_mode() {
                Option::None => {
                    debug_assert!(false, "state.find_replace_mode() returned None");
                }
                Some(FindReplaceMode::CurrentFile) => {
                    state.buffers.get_current_buffer_mut().advance_or_refresh_search_results(
                        (&state.find).into(),
                    );

                    try_to_show_cursors!(BufferIdKind::Text);
                    try_to_show_cursors!();
                }
            },
            BufferIdKind::Replace => {
                let i = state.buffers.current_index();
                dbg!("TODO BufferIdKind::Replace {}", i);
            }
            BufferIdKind::GoToPosition => {
                text_buffer_call!(b{
                    let input: String = b.into();
                    if let Ok(position) = parse_for_go_to_position(&input) {
                        go_to_position!(position);
                    }
                    // TODO: show Err case
                });
            }
            BufferIdKind::FileSwitcher => {
                post_edit_sync!();
            }
        },
        ShowError(error) => {
            let mut saw_same_error = false;

            for editor_buffer in state.buffers.iter() {
                if let BufferName::Scratch(_) = &editor_buffer.name {
                    if editor_buffer.text_buffer.borrow_rope() == &error {
                        saw_same_error = true;
                        break;
                    }
                }
            }

            if !saw_same_error {
                new_scratch_buffer!(Some(error));
            }
        }
    }

    // We expect this to always be the case
    state.view.stats.latest_update_time_span = TimeSpan::end_if_started(
        state.view.stats.latest_update_time_span
    );

    state.view.stats.latest_render_time_span = TimeSpan::start();

    // updates the view
    editor_view::render(state);

    state.view.stats.latest_render_time_span = TimeSpan::end_if_started(
        state.view.stats.latest_render_time_span
    );

    let mut cloned_view = state.view.clone();
    // We want to measure the cloning time.
    cloned_view.stats.latest_overall_time_span = TimeSpan::Ended(Instant::now() - start_time);

    (cloned_view, cmd)
}

#[cfg(test)]
mod tests;