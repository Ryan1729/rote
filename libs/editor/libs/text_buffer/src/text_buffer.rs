#![deny(unused)]
use cursors::Cursors;
use edit::{Change, CursoredRope, Edit, change};
use editor_types::{Cursor, SetPositionAction};
use macros::{d, dbg, u};
use move_cursor::{forward, get_next_selection_point, get_previous_selection_point};
use panic_safe_rope::{ByteIndex, Rope, RopeSlice};
use parsers::{Parsers, ParserKind};
use platform_types::{*, screen_positioning::*};
use rope_pos::{
    AbsoluteCharOffsetRange,
    in_cursor_bounds,
    pos_to_char_offset,
    offset_pair,
    nearest_valid_position_on_same_line,
};

use std::{
    borrow::Borrow,
};

/// Not `Eq` because of the `ScrollXY` field.
#[derive(Clone, Debug, PartialEq)]
pub struct TextBuffer<
    const EDIT_COUNT: usize = {history::DEFAULT_EDIT_COUNT}
> {
    /// We keep the `CursoredRope` private, and only allow non-mut borrows
    /// so we know that the history contains all the changes made
    /// to the rope.
    rope: CursoredRope,
    history: History<EDIT_COUNT>,
    unedited: Rope,
    // TODO The scroll stuff is kinda unrelated to everything else. Consider moving
    // it out of this struct.
    pub scroll: ScrollXY,
}

impl <const EDIT_COUNT: usize> Default for TextBuffer<EDIT_COUNT> {
    fn default() -> Self {
        let rope: Rope = d!();
        TextBuffer {
            unedited: rope.clone(),
            rope: rope.into(),
            history: d!(),
            scroll: d!(),
        }
    }
}

impl <const EDIT_COUNT: usize> TextBuffer<EDIT_COUNT> {
    #[perf_viz::record]
    pub fn non_rope_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use std::hash::Hash;
        self.borrow_cursors().hash(state);
        perf_viz::start_record!("history hash");
        self.history.hash(state);
        perf_viz::end_record!("history hash");
        self.scroll.hash(state);
    }

    #[perf_viz::record]
    pub fn rope_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use std::hash::Hash;
        for c in self.borrow_rope().chunks() {
            c.hash(state);
        }

        // TODO is it worth it to have a third hash level?
        for c in self.unedited.chunks() {
            c.hash(state);
        }
    }

    // TODO write a test that fails when we add a new field that isn't counted here.
    // A compile-time assert would be preferable, of course.
    #[perf_viz::record]
    pub fn size_in_bytes(&self) -> usize {
        use core::mem;

        let mut output = 0;

        let rope = self.borrow_rope();
        output += mem::size_of_val(rope);
        output += usize::from(rope.len_bytes().0);
        output += self.borrow_cursors().size_in_bytes();
        output += self.history.size_in_bytes();
        output += mem::size_of_val(&self.unedited);
        output += usize::from(self.unedited.len_bytes().0);
        output += mem::size_of_val(&self.scroll);

        output
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SizeInfo {
    pub char_dim: CharDim, 
    pub xywh: TextBoxXYWH,
}

#[derive(Clone, Copy, Debug)]
pub enum ScrollAdjustSpec {
    Calculate(SizeInfo, Position),
    Direct(ScrollXY)
}

impl <const EDIT_COUNT: usize> TextBuffer<EDIT_COUNT> {
    #[must_use]
    pub fn borrow_rope(&self) -> &Rope {
        self.rope.borrow_rope()
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.borrow_rope().chars().count()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.borrow_rope().len_bytes() == 0
    }

    #[must_use]
    pub fn clone_rope(&self) -> Rope {
        self.borrow_rope().clone()
    }

    #[must_use]
    pub fn borrow_cursors(&self) -> &Cursors {
        &self.rope.borrow_cursors()
    }

    pub fn reset_cursor_states(&mut self) {
        self.rope.reset_cursor_states();
    }

    pub fn try_to_show_cursors_on(
        &mut self,
        spec: ScrollAdjustSpec,
    ) -> VisibilityAttemptResult {
        u!{ScrollAdjustSpec, VisibilityAttemptResult};

        let scroll = &mut self.scroll;

        match spec {
            Direct(s) => {
                *scroll = s;
                Succeeded
            }
            Calculate(SizeInfo { char_dim, xywh}, position) => {
                let text_space = position_to_text_space(
                    position,
                    char_dim
                );

                // We try first with this smaller xywh to make the cursor appear
                // in the center more often.
                let small_xywh = xywh;
                //let mut small_xywh = xywh.clone();
                //small_xywh.xy.x += small_xywh.wh.w / 4.0;
                //small_xywh.wh.w /= 2.0;
                //small_xywh.xy.y += small_xywh.wh.h / 4.0;
                //small_xywh.wh.h /= 2.0;
                let x_ratio = char_dim.w.get() / small_xywh.wh.w.get();
                let y_ratio = 3.0 * char_dim.h.get() / small_xywh.wh.h.get();
                let apron = apron!(
                    0.0,
                    x_ratio,
                    0.0,
                    y_ratio,
                );

                let mut attempt_result;
                attempt_result = attempt_to_make_xy_visible(
                    scroll,
                    small_xywh,
                    apron,
                    text_space,
                );

                if attempt_result != Succeeded {
                    attempt_result = attempt_to_make_xy_visible(
                        scroll,
                        xywh,
                        apron,
                        text_space,
                    );
                }
                dbg!(attempt_result);
                attempt_result
            }
        }
    }
}

impl From<String> for TextBuffer {
    fn from(s: String) -> Self {
        let mut output: Self = d!();

        output.rope = CursoredRope::from(s);
        output.set_unedited();

        output
    }
}

impl From<&str> for TextBuffer {
    fn from(s: &str) -> Self {
        let mut output: Self = d!();

        output.rope = CursoredRope::from(s);
        output.set_unedited();

        output
    }
}

impl From<&TextBuffer> for String {
    fn from(t_b: &TextBuffer) -> Self {
        t_b.borrow_rope().clone().into()
    }
}

impl From<&mut TextBuffer> for String {
    fn from(t_b: &mut TextBuffer) -> Self {
        t_b.borrow_rope().clone().into()
    }
}

impl <'t_b> From<&'t_b TextBuffer> for RopeSlice<'t_b> {
    fn from(t_b: &'t_b TextBuffer) -> Self {
        t_b.borrow_rope().full_slice()
    }
}

impl <'t_b> From<&'t_b mut TextBuffer> for RopeSlice<'t_b> {
    fn from(t_b: &'t_b mut TextBuffer) -> Self {
        t_b.borrow_rope().full_slice()
    }
}

fn next_instance_of_selected(rope: &Rope, cursor: &Cursor) -> Option<(Position, Position)> {
    match offset_pair(rope, cursor) {
        (Some(p_offset), Some(h_offset)) => {
            let range = AbsoluteCharOffsetRange::new(p_offset, h_offset);
            let selected_text = rope.slice(range)?;

            search::get_ranges(
                selected_text,
                rope,
                Some(AbsoluteCharOffsetRange::new(range.max(), rope.len_chars())),
                std::num::NonZeroUsize::new(1),
            )
            .pop()
            .or_else(|| {
                search::get_ranges(
                    selected_text,
                    rope,
                    Some(AbsoluteCharOffsetRange::new(d!(), range.min())),
                    std::num::NonZeroUsize::new(1),
                )
                .pop()
            })
        }
        _ => None,
    }
}

enum AllOrOne {
    All,
    Index(usize),
}

enum MoveOrSelect {
    Move,
    Select,
}

struct CursorMoveSpec {
    what: MoveOrSelect,
    how_many: AllOrOne,
}

fn char_to_string(c: char) -> String {
    let mut buf = [0; 4];
    c.encode_utf8(&mut buf).to_owned()
}

pub type PossibleEditedTransition = Option<EditedTransition>;

pub struct ParserEditListener<'name, 'parsers> {
    pub buffer_name: &'name BufferName,
    pub parser_kind: ParserKind,
    pub parsers: &'parsers mut Parsers,
}

pub type PossibleParserEditListener<'name, 'parsers> = Option<
    ParserEditListener<'name, 'parsers>
>;

// This macro will make it easier to change this if we ever need to,
// since most methods will just need to pass it through.
#[macro_export]
macro_rules! ppel {
    () => {
        $crate::PossibleParserEditListener<'_, '_>
    }
}

mod history {
    use std::collections::VecDeque;
    use edit::{Change, Edit, change};
    use super::{EditedTransition, Editedness};
    use macros::u;

    pub const DEFAULT_EDIT_COUNT: usize = 4096;

    #[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
    pub struct History<const EDIT_COUNT: usize = DEFAULT_EDIT_COUNT> {
        edits: VecDeque<Edit>,
        index: usize,
    }

    impl <const EDIT_COUNT: usize> History<EDIT_COUNT> {
        pub fn redo(
            &mut self,
            callback: impl FnOnce(&Edit) -> Change<Editedness>
        ) -> NavOutcome {
            if let Some(edit) = self.edits.get(self.index) {
                self.index += 1;

                callback(edit).into()
            } else {
                NavOutcome::RanOutOfHistory
            }
        }

        pub fn undo(
            &mut self,
            callback: impl FnOnce(&Edit) -> Change<Editedness>
        ) -> NavOutcome {
            let opt = self.index.checked_sub(1)
                .and_then(|new_index|
                    self.edits.get(new_index).map(|e| (new_index, e))
                );

            if let Some((new_index, edit)) = opt {
                self.index = new_index;

                callback(&!edit).into()
            } else {
                NavOutcome::RanOutOfHistory
            }
        }

        pub fn record_edit(&mut self, edit: Edit) {
            self.edits.truncate(self.index);
            if self.edits.len() < self.max_len() {
                self.index += 1;
            } else {
                // This makes the current index point to the empty space after the
                // newest edit. So we don't need to adjust the index here.
                self.edits.pop_front();
            }
            self.edits.push_back(edit);
        }

        pub fn index(&self) -> usize {
            self.index
        }

        pub fn len(&self) -> usize {
            self.edits.len()
        }

        pub fn is_empty(&self) -> bool {
            self.edits.is_empty()
        }

        // TODO enforce this limit once we are sure it is a good enough size.
        pub fn max_len(&self) -> usize {
            EDIT_COUNT
        }

        pub fn clear(&mut self) {
            self.edits.clear();
            self.index = 0;
        }

        pub fn size_in_bytes(&self) -> usize {
            let mut output = 0;
            // TODO Does this take long enough that we should memoize this method?
            // Maybe caching further upstream would be better?
            for edit in self.edits.iter() {
                output += edit.size_in_bytes();
            }

            output += (
                self.edits.capacity() -
                // Don't double count the struct bytes from the `edit.size_in_bytes()`
                // calls above.
                self.edits.len()
            ) * core::mem::size_of::<Edit>();
            output += core::mem::size_of_val(&self.index);

            output
        }
    }

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub enum NavOutcome {
        NoTransition,
        Transition(EditedTransition),
        RanOutOfHistory,
    }

    impl From<Change<Editedness>> for NavOutcome {
        fn from(c: Change<Editedness>) -> Self {
            u!{NavOutcome, Editedness, EditedTransition}
            match c {
                change!(Edited, Edited) | change!(Unedited, Unedited) => NoTransition,
                change!(Edited, Unedited) => Transition(ToUnedited),
                change!(Unedited, Edited) => Transition(ToEdited),
            }
        }
    }

    impl From<NavOutcome> for Option<EditedTransition> {
        fn from(hno: NavOutcome) -> Self {
            u!{NavOutcome}
            match hno {
                RanOutOfHistory | NoTransition => None,
                Transition(t) => Some(t)
            }
        }
    }

    impl NavOutcome {
        pub fn ran_out_of_history(&self) -> bool {
            u!{NavOutcome}
            matches!(self, RanOutOfHistory)
        }
    }
}
use history::{History, NavOutcome as HistoryNavOutcome};

impl <const EDIT_COUNT: usize> TextBuffer<EDIT_COUNT> {
    #[perf_viz::record]
    pub fn insert(&mut self, c: char, listener: ppel!()) -> PossibleEditedTransition {
        dbg!("\n\n\ninsert\n\n\n");
        self.insert_string(char_to_string(c), listener)
    }

    #[perf_viz::record]
    pub fn insert_string(
        &mut self,
        s: String,
        listener: ppel!()
    ) -> PossibleEditedTransition {
        let o = self.record_edit(
            edit::get_insert_edit(&self.rope, |_| s.clone()),
            listener
        );
        dbg!(&self);
        o
    }

    pub fn insert_at_each_cursor<F>(
        &mut self,
        func: F,
        listener: ppel!()
    ) -> PossibleEditedTransition
    where
        F: Fn(usize) -> String,
    {
        self.record_edit(
            edit::get_insert_edit(&self.rope, func),
            listener,
        )
    }

    #[perf_viz::record]
    pub fn delete(&mut self, listener: ppel!()) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_delete_edit(&self.rope),
            listener,
        )
    }

    #[perf_viz::record]
    pub fn delete_lines(&mut self, listener: ppel!()) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_delete_lines_edit(&self.rope),
            listener,
        )
    }

    pub fn move_all_cursors(&mut self, r#move: Move) {
        self.move_cursors(
            CursorMoveSpec {
                what: MoveOrSelect::Move,
                how_many: AllOrOne::All,
            },
            r#move,
        );
    }

    pub fn extend_selection_for_all_cursors(&mut self, r#move: Move) {
        self.move_cursors(
            CursorMoveSpec {
                what: MoveOrSelect::Select,
                how_many: AllOrOne::All,
            },
            r#move,
        );
    }

    pub fn copy_selections(&self) -> Vec<String> {
        let mut selections = self.get_selections_and_cut_edit().0;

        selections.reverse();

        selections
    }

    pub fn cut_selections(&mut self, listener: ppel!()) -> (Vec<String>, PossibleEditedTransition) {
        let (strings, edit) = self.get_selections_and_cut_edit();

        (strings, self.record_edit(edit, listener))
    }

    fn get_selections_and_cut_edit(&self) -> (Vec<String>, Edit) {
        let edit = edit::get_cut_edit(&self.rope);

        (edit.selected(), edit)
    }

    #[perf_viz::record]
    pub fn set_cursor<C: Into<Cursor>>(
        &mut self,
        cursor: C,
        replace_or_add: ReplaceOrAdd
    ) {
        if let NewCursorsOutcome::Success(cursors) = get_new_cursors(
            &self.rope,
            cursor,
            replace_or_add
        ) {
            self.apply_cursor_only_edit(cursors);
        }
    }

    pub fn select_all(&mut self) {
        self.set_cursor(pos! {}, ReplaceOrAdd::Replace);
        self.extend_selection_for_all_cursors(Move::ToBufferEnd);
    }

    fn get_cloned_cursors(&self) -> Vec1<Cursor> {
        self.borrow_cursors().get_cloned_cursors()
    }

    #[perf_viz::record]
    pub fn drag_cursors<P: Borrow<Position>>(&mut self, position: P) {
        let position = position.borrow();

        if let Some(p) = nearest_valid_position_on_same_line(
            self.borrow_rope(),
            position
        ) {
            let mut new = self.get_cloned_cursors();
            for c in new.iter_mut() {
                c.set_position_custom(p, SetPositionAction::OldPositionBecomesHighlightIfItIsNone);
            }
            self.apply_cursor_only_edit(new);
        }
    }

    pub fn xy_to_position(
        &self,
        char_dim: CharDim,
        xy: TextBoxSpaceXY,
    ) -> Position {
        text_space_to_position(
            text_box_to_text(xy, self.scroll),
            char_dim,
            // We want different rounding for selections so that if we trigger a selection on the
            // right side of a character, we select that character rather than the next character.
            PositionRound::TowardsZero,
        )
    }

    /// Selects a grouping of characters with a single character type, where the character types
    /// are as follows:
    /// * Word characters, as defined by the `regex` crate
    /// * Whitspace characters, again as defined by the `regex` crate
    /// * everything else, which we will call "Punctuation"
    /// (see get_offsets in move_cursor.rs for details)
    ///
    /// If it helps, you can think of it as "select_word" if the cursor is on a word, and other
    /// stuff otherwise
    pub fn select_char_type_grouping(
        &mut self,
        position: Position,
        replace_or_add: ReplaceOrAdd,
    ) {
        if let NewCursorsOutcome::Success(mut new) = get_new_cursors(
            &self.rope,
            position,
            replace_or_add
        ) {
            let c = new.last_mut();

            let rope = &self.borrow_rope();
            let old_position = c.get_position();

            let highlight_position = get_previous_selection_point(
                rope,
                // Both of these `.unwrap_or(old_position)` calls are necessary.
                // If we were to use an `and_then` call instead, then we would default to
                // `old_position` too often
                forward(rope, old_position).unwrap_or(old_position),
            )
            .unwrap_or(old_position);
            c.set_highlight_position(highlight_position);

            let pos = get_next_selection_point(rope, old_position)
                .unwrap_or(old_position);
            c.set_position_custom(
                pos,
                SetPositionAction::ClearHighlightOnlyIfItMatchesNewPosition,
            );

            self.apply_cursor_only_edit(new);
        }
    }

    #[perf_viz::record]
    pub fn move_cursor(&mut self, index: usize, r#move: Move) {
        self.move_cursors(
            CursorMoveSpec {
                what: MoveOrSelect::Move,
                how_many: AllOrOne::Index(index),
            },
            r#move,
        );
    }

    #[perf_viz::record]
    pub fn extend_selection(&mut self, index: usize, r#move: Move) {
        self.move_cursors(
            CursorMoveSpec {
                what: MoveOrSelect::Select,
                how_many: AllOrOne::Index(index),
            },
            r#move,
        );
    }

    pub fn extend_selection_with_search(
        &mut self,
        // Needed in order to scroll
        size_info: SizeInfo
    ) {
        // We use this to specify what mutation to do after the loop since the
        // borrow checker can't currently figure out that it would be fine to
        // mutate the rope inside the loop, if we return right afterwards. :/
        enum Mutation {
            Nop,
            Select(Position),
            SetCursors(Vec1<Cursor>, Position),
        }

        let mut mutation = Mutation::Nop;

        for cursor in self.rope.borrow_cursors().iter() {
            match cursor.get_highlight_position() {
                Option::None => {
                    mutation = Mutation::Select(cursor.get_position());
                    break
                }
                Some(_) => {
                    if let Some(pos_pair) = next_instance_of_selected(
                        self.borrow_rope(),
                        cursor
                    ) {
                        let new_cursor = Cursor::from(pos_pair);

                        match get_new_cursors(
                            &self.rope,
                            new_cursor,
                            ReplaceOrAdd::Add,
                        ) {
                            NewCursorsOutcome::Success(cs) => {
                                mutation = Mutation::SetCursors(
                                    cs,
                                    new_cursor.get_position(),
                                );
                                break
                            },
                            // TODO bubble up an error to the user? Does this
                            // actually happen in practice?
                            NewCursorsOutcome::InvalidPosition => return,
                            NewCursorsOutcome::AlreadyPresent => {
                                // Try the next cursor, if any.
                            },
                        }
                    }
                }
            }
        }

        match mutation {
            Mutation::Nop => {},
            Mutation::Select(position) => {
                self.select_char_type_grouping(
                    position,
                    ReplaceOrAdd::Add
                );

                self.try_to_show_cursors_on(
                    ScrollAdjustSpec::Calculate(size_info, position)
                );
            },
            Mutation::SetCursors(cs, position) => {
                self.apply_cursor_only_edit(cs);

                self.try_to_show_cursors_on(
                    ScrollAdjustSpec::Calculate(size_info, position)
                );
            },
        }
    }

    fn move_cursors(&mut self, spec: CursorMoveSpec, r#move: Move) -> Option<()> {
        let mut new = self.get_cloned_cursors();

        let action: for<'r, 's> fn(&'r Rope, &'s mut Cursor, Move) = match spec.what {
            MoveOrSelect::Move => move_cursor::or_clear_highlights,
            MoveOrSelect::Select => move_cursor::and_extend_selection,
        };

        match spec.how_many {
            AllOrOne::All => {
                for cursor in new.iter_mut() {
                    action(&self.borrow_rope(), cursor, r#move);
                }
            }
            AllOrOne::Index(index) => {
                action(&self.borrow_rope(), new.get_mut(index)?, r#move);
            }
        };

        self.apply_cursor_only_edit(new);

        Some(())
    }

    /// It is important that all edits that only involve cursor changes go through here
    /// This is because they require special handling regarding undo/redo.
    fn apply_cursor_only_edit(
        &mut self,
        new: Vec1<Cursor>,
    ) {
        let change = edit::Change {
            old: self.rope.borrow_cursors().clone(),
            new: Cursors::new(self.rope.borrow_rope(), new),
        };

        // We don't record cursor movements for undo purposes
        // so you can undo, select, copy and then redo.
        apply_edit(
            &mut self.rope,
            &change.into(),
            // Since we know that this edit only involves cursors, we know the
            // parsers won't care about it.
            None,
        );
    }

    pub fn tab_in(&mut self, listener: PossibleParserEditListener) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_tab_in_edit(&self.rope),
            listener
        )
    }

    pub fn tab_out(&mut self, listener: PossibleParserEditListener) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_tab_out_edit(&self.rope),
            listener,
        )
    }

    pub fn strip_trailing_whitespace(&mut self, listener: PossibleParserEditListener) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_strip_trailing_whitespace_edit(&self.rope),
            listener,
        )
    }

    pub fn toggle_single_line_comments(&mut self, listener: PossibleParserEditListener) -> PossibleEditedTransition {
        // At some point we may want to support languages with different comment
        // syntaxes. At that point we have at least two options here:
        // * Have this method require a `ParserKind`.
        // * Change `PossibleParserEditListener` to always contain a `ParserKind`.
        // Given a suffient number of methods that care about the `ParserKind`,
        // the second option seems like it may become attractive.
        self.record_edit(
            edit::get_toggle_single_line_comments_edit(&self.rope),
            listener,
        )
    }

    pub fn toggle_case(&mut self, listener: PossibleParserEditListener) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_toggle_case_edit(&self.rope),
            listener,
        )
    }

    pub fn duplicate_lines(&mut self, listener: PossibleParserEditListener) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_duplicate_lines_edit(&self.rope),
            listener,
        )
    }

    #[perf_viz::record]
    fn record_edit(&mut self, edit: Edit, listener: PossibleParserEditListener) -> PossibleEditedTransition {
        u!{Editedness, EditedTransition}
        let old_editedness = self.editedness();
        dbg!(old_editedness);

        apply_edit(&mut self.rope, &edit, listener);

        self.history.record_edit(edit);

        dbg!(self.editedness());
        match change!(old_editedness, self.editedness()) {
            change!(Edited, Edited) | change!(Unedited, Unedited) => None,
            change!(Edited, Unedited) => Some(ToUnedited),
            change!(Unedited, Edited) => Some(ToEdited),
        }
    }

    pub fn collapse_cursors(&mut self, char_dim: CharDim, text_box_xywh: TextBoxXYWH) {
        let rect = text_box_xywh.into();

        let mut cursor_index = 0;
        for (i, c) in self.rope.borrow_cursors().iter().enumerate() {
            let screen_xy = position_to_screen_space(
                c.get_position(),
                char_dim,
                self.scroll,
                text_box_xywh.xy,
            );

            if inside_rect(screen_xy, rect) {
                cursor_index = i;
                break
            }
        }

        self.rope.collapse_cursors_to(cursor_index);
    }

    // some of these are convenience methods for tests
    #[cfg(test)]
    fn set_cursors(&mut self, new: Cursors) {
        self.rope.set_cursors(new);
    }

    #[cfg(any(test, feature = "pub_arb"))]
    fn set_cursors_from_vec1(&mut self, cursors: Vec1<Cursor>) {
        self.rope.set_cursors_from_vec1(cursors);
    }
}

enum NewCursorsOutcome {
    Success(Vec1<Cursor>),
    InvalidPosition,
    AlreadyPresent,
}

fn get_new_cursors<C: Into<Cursor>>(
    rope: &CursoredRope,
    cursor: C,
    replace_or_add: ReplaceOrAdd,
) -> NewCursorsOutcome {
    let unclamped_cursor = cursor.into();

    let cursor: Cursor = match (
        nearest_valid_position_on_same_line(
            rope.borrow_rope(),
            unclamped_cursor.get_position()
        ),
        unclamped_cursor
            .get_highlight_position()
            .and_then(|h| nearest_valid_position_on_same_line(
                rope.borrow_rope(),
                h
            )),
    ) {
        (Some(p), Some(h)) => (p, h).into(),
        (Some(p), None) => p.into(),
        (None, _) => return NewCursorsOutcome::InvalidPosition,
    };

    NewCursorsOutcome::Success(match replace_or_add {
        ReplaceOrAdd::Replace => Vec1::new(cursor),
        ReplaceOrAdd::Add => {
            let mut cursors = rope.borrow_cursors().get_cloned_cursors();

            for c in &cursors {
                if c == &cursor {
                    return NewCursorsOutcome::AlreadyPresent;
                }
            }
            cursors.push(cursor);

            cursors
        }
    })
}

#[perf_viz::record]
fn apply_edit(
    rope: &mut CursoredRope,
    edit: &Edit,
    listener: ppel!(),
) {
    if let Some(listener) = listener {
        listener.parsers.acknowledge_edit(
            listener.buffer_name,
            listener.parser_kind,
            edit,
            rope.borrow_rope()
        );
    }

    rope.apply(edit);
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Editedness {
    Edited,
    Unedited
}

impl <const EDIT_COUNT: usize> TextBuffer<EDIT_COUNT> {
    pub fn editedness(&self) -> Editedness {
        editedness(&self.unedited, self.rope.borrow_rope())
    }

    pub fn set_unedited(&mut self) {
        self.unedited = self.borrow_rope().clone();
    }
}

fn editedness(unedited: &Rope, current: &Rope) -> Editedness {
    u!{Editedness}

    dbg!(unedited, current);
    if unedited == current {
        Unedited
    } else {
        Edited
    }
}

impl <const EDIT_COUNT: usize> TextBuffer<EDIT_COUNT> {
    pub fn redo(&mut self, listener: ppel!()) -> HistoryNavOutcome {
        let old_editedness = self.editedness();

        self.history.redo(|edit| {
            apply_edit(&mut self.rope, edit, listener);

            change!(
                old_editedness,
                editedness(&self.unedited, self.rope.borrow_rope())
            )
        })
    }

    pub fn undo(&mut self, listener: ppel!()) -> HistoryNavOutcome {
        let old_editedness = self.editedness();

        self.history.undo(|edit| {
            apply_edit(&mut self.rope, edit, listener);

            change!(
                old_editedness,
                editedness(&self.unedited, self.rope.borrow_rope())
            )
        })
    }

    pub fn has_no_edits(&self) -> bool {
        debug_assert_eq!(self.editedness(), Editedness::Unedited);
        self.history.is_empty()
    }

    pub fn clear_history(&mut self) {
        self.history.clear();
    }
}

pub struct HistoryStats {
    pub index: usize,
    pub len: usize,
    pub max_len: usize,
}

impl <const EDIT_COUNT: usize> TextBuffer<EDIT_COUNT> {
    pub fn history_stats(&self) -> HistoryStats {
        HistoryStats {
            index: self.history.index(),
            len: self.history.len(),
            max_len: self.history.max_len(),
        }
    }
}

//
// View rendering
//

impl <const EDIT_COUNT: usize> TextBuffer<EDIT_COUNT> {
    pub fn in_bounds<P: Borrow<Position>>(&self, position: P) -> bool {
        in_cursor_bounds(self.borrow_rope(), position)
    }

    #[perf_viz::record]
    pub fn find_index<P: Borrow<Position>>(&self, p: P) -> Option<ByteIndex> {
        let rope = self.borrow_rope();
        pos_to_char_offset(rope, p.borrow()).and_then(|o| rope.char_to_byte(o))
    }
}

#[cfg(any(test, feature = "pub_arb"))]
#[macro_use]
pub mod test_macros;
#[cfg(any(test, feature = "pub_arb"))]
#[macro_use]
pub mod tests;
