#![deny(unused)]
use cursors::Cursors;
use edit::{Applier, Change, Edit, change};
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
    collections::VecDeque,
};

#[derive(Clone, Debug, PartialEq)]
pub struct TextBuffer {
    /// We keep the rope private, and only allow non-mut borrows
    /// so we know that the history contains all the changes made
    /// to the rope.
    rope: Rope,
    cursors: Cursors,
    history: VecDeque<Edit>,
    history_index: usize,
    unedited: Rope,
    pub scroll: ScrollXY,
}

d!(for TextBuffer: {
    let rope: Rope = d!();
    TextBuffer {
        unedited: rope.clone(),
        rope,
        cursors: d!(),
        history: d!(),
        history_index: d!(),
        scroll: d!(),
    }
});

impl TextBuffer {
    #[perf_viz::record]
    pub fn non_rope_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use std::hash::Hash;
        self.cursors.hash(state);
        perf_viz::start_record!("history hash");
        self.history.hash(state);
        perf_viz::end_record!("history hash");
        self.history_index.hash(state);
        self.scroll.hash(state);
    }

    #[perf_viz::record]
    pub fn rope_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use std::hash::Hash;
        for c in self.rope.chunks() {
            c.hash(state);    
        }

        // TODO is it worth it to have a third hash level?
        for c in self.unedited.chunks() {
            c.hash(state);    
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ScrollAdjustSpec {
    Calculate(CharDim, TextBoxXYWH),
    Direct(ScrollXY)
}

impl TextBuffer {
    #[must_use]
    pub fn len(&self) -> usize {
        self.borrow_rope().chars().count()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.rope.len_bytes() == 0
    }

    #[must_use]
    pub fn borrow_rope(&self) -> &Rope {
        &self.rope
    }

    #[must_use]
    pub fn clone_rope(&self) -> Rope {
        self.rope.clone()
    }

    #[must_use]
    pub fn borrow_cursors(&self) -> &Cursors {
        &self.cursors
    }

    pub fn reset_cursor_states(&mut self) {
        self.cursors.reset_states();
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
            Calculate(char_dim, xywh) => {
                let text_space = position_to_text_space(dbg!(self.cursors.last().get_position()), char_dim);

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

        output.rope = Rope::from(s);
        output.set_unedited();

        output
    }
}

impl From<&str> for TextBuffer {
    fn from(s: &str) -> Self {
        let mut output: Self = d!();

        output.rope = Rope::from(s);
        output.set_unedited();

        output
    }
}

impl From<&TextBuffer> for String {
    fn from(t_b: &TextBuffer) -> Self {
        t_b.rope.clone().into()
    }
}

impl From<&mut TextBuffer> for String {
    fn from(t_b: &mut TextBuffer) -> Self {
        t_b.rope.clone().into()
    }
}

impl <'t_b> From<&'t_b TextBuffer> for RopeSlice<'t_b> {
    fn from(t_b: &'t_b TextBuffer) -> Self {
        t_b.rope.full_slice()
    }
}

impl <'t_b> From<&'t_b mut TextBuffer> for RopeSlice<'t_b> {
    fn from(t_b: &'t_b mut TextBuffer) -> Self {
        t_b.rope.full_slice()
    }
}

fn next_instance_of_selected(rope: &Rope, cursor: &Cursor) -> Option<(Position, Position)> {
    match offset_pair(rope, cursor) {
        (Some(p_offset), Some(h_offset)) => {
            let range = AbsoluteCharOffsetRange::new(p_offset, h_offset);
            let selected_text = rope.slice(range.range())?;

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
enum ApplyKind {
    Record,
    Playback,
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

impl TextBuffer {
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
            edit::get_insert_edit(&self.rope, &self.cursors, |_| s.clone()),
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
            edit::get_insert_edit(&self.rope, &self.cursors, func),
            listener,
        )
    }

    #[perf_viz::record]
    pub fn delete(&mut self, listener: ppel!()) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_delete_edit(&self.rope, &self.cursors),
            listener,
        )
    }

    #[perf_viz::record]
    pub fn delete_lines(&mut self, listener: ppel!()) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_delete_lines_edit(&self.rope, &self.cursors),
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
        self.get_selections_and_cut_edit().0
    }

    pub fn cut_selections(&mut self, listener: ppel!()) -> (Vec<String>, PossibleEditedTransition) {
        let (strings, edit) = self.get_selections_and_cut_edit();

        (strings, self.record_edit(edit, listener))
    }

    fn get_selections_and_cut_edit(&self) -> (Vec<String>, Edit) {
        let edit = edit::get_cut_edit(&self.rope, &self.cursors);

        (edit.selected(), edit)
    }

    #[perf_viz::record]
    pub fn set_cursor<C: Into<Cursor>>(
        &mut self,
        cursor: C,
        replace_or_add: ReplaceOrAdd
    ) {
        if let Some(cursors) = self.get_new_cursors(cursor, replace_or_add) {
            self.apply_cursor_only_edit(cursors);
        }
    }

    pub fn select_all(&mut self) {
        self.set_cursor(pos! {}, ReplaceOrAdd::Replace);
        self.extend_selection_for_all_cursors(Move::ToBufferEnd);
    }

    fn get_new_cursors<C: Into<Cursor>>(
        &self,
        cursor: C,
        replace_or_add: ReplaceOrAdd,
    ) -> Option<Vec1<Cursor>> {
        let unclamped_cursor = cursor.into();

        let cursor: Option<Cursor> = match (
            nearest_valid_position_on_same_line(&self.rope, unclamped_cursor.get_position()),
            unclamped_cursor
                .get_highlight_position()
                .and_then(|h| nearest_valid_position_on_same_line(&self.rope, h)),
        ) {
            (Some(p), Some(h)) => Some((p, h).into()),
            (Some(p), None) => Some(p.into()),
            (None, _) => None,
        };

        cursor.map(|c| match replace_or_add {
            ReplaceOrAdd::Replace => Vec1::new(c),
            ReplaceOrAdd::Add => {
                let mut cursors = self.cursors.get_cloned_cursors();
                cursors.push(c);
                cursors
            }
        })
    }

    #[perf_viz::record]
    pub fn drag_cursors<P: Borrow<Position>>(&mut self, position: P) {
        let position = position.borrow();

        if let Some(p) = nearest_valid_position_on_same_line(&self.rope, position) {
            let mut new = self.cursors.get_cloned_cursors();
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
        if let Some(mut new) = self.get_new_cursors(position, replace_or_add) {
            let c = new.last_mut();

            let rope = &self.rope;
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

            let pos = get_next_selection_point(rope, old_position).unwrap_or(old_position);
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

    pub fn extend_selection_with_search(&mut self) {
        let cursor = self.borrow_cursors().first().clone();
        match cursor.get_highlight_position() {
            Option::None => {
                self.select_char_type_grouping(
                    cursor.get_position(),
                    ReplaceOrAdd::Add
                );
            }
            Some(_) => {
                if let Some(pair) = next_instance_of_selected(self.borrow_rope(), &cursor) {
                    self.set_cursor(pair, ReplaceOrAdd::Add);
                }
            }
        }
    }

    fn move_cursors(&mut self, spec: CursorMoveSpec, r#move: Move) -> Option<()> {
        let mut new = self.cursors.get_cloned_cursors();

        let action: for<'r, 's> fn(&'r Rope, &'s mut Cursor, Move) = match spec.what {
            MoveOrSelect::Move => move_cursor::or_clear_highlights,
            MoveOrSelect::Select => move_cursor::and_extend_selection,
        };

        match spec.how_many {
            AllOrOne::All => {
                for cursor in new.iter_mut() {
                    action(&self.rope, cursor, r#move);
                }
            }
            AllOrOne::Index(index) => {
                action(&self.rope, new.get_mut(index)?, r#move);
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
            old: self.cursors.clone(),
            new: Cursors::new(&self.rope, new),
        };

        self.apply_edit(
            change.into(),
            // We don't record cursor movements for undo purposes
            // so you can undo, select, copy and then redo.
            ApplyKind::Playback,
            // Since we know that this edit only involves cursors, we know the
            // parsers won't care about it.
            None,
        );
    }

    pub fn tab_in(&mut self, listener: PossibleParserEditListener) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_tab_in_edit(&self.rope, &self.cursors),
            listener
        )
    }

    pub fn tab_out(&mut self, listener: PossibleParserEditListener) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_tab_out_edit(&self.rope, &self.cursors),
            listener,
        )
    }

    #[perf_viz::record]
    fn record_edit(&mut self, edit: Edit, listener: PossibleParserEditListener) -> PossibleEditedTransition {
        u!{Editedness, EditedTransition}
        let old_editedness = self.editedness();
        dbg!(old_editedness);

        self.apply_edit(edit, ApplyKind::Record, listener);

        dbg!(self.editedness());
        match change!(old_editedness, self.editedness()) {
            change!(Edited, Edited) | change!(Unedited, Unedited) => None,
            change!(Edited, Unedited) => Some(ToUnedited),
            change!(Unedited, Edited) => Some(ToEdited),
        }
    }

    #[perf_viz::record]
    fn apply_edit(
        &mut self,
        edit: Edit,
        kind: ApplyKind,
        listener: ppel!(),
    ) {
        if let Some(listener) = listener {
            listener.parsers.acknowledge_edit(
                listener.buffer_name,
                listener.parser_kind,
                &edit,
                &self.rope
            );
        }

        let applier = Applier::new(
            &mut self.rope,
            &mut self.cursors
        );
        edit::apply(applier, &edit);

        match kind {
            ApplyKind::Record => {
                self.history.truncate(self.history_index);
                self.history.push_back(edit);
                self.history_index += 1;
            }
            ApplyKind::Playback => {}
        }
    }

    // some of these are convenience methods for tests
    #[allow(dead_code)]
    fn set_cursors(&mut self, new: Cursors) {
        cursors::set_cursors(&self.rope, &mut self.cursors, new);
    }

    #[allow(dead_code)]
    fn set_cursors_from_vec1(&mut self, cursors: Vec1<Cursor>) {
        self.cursors = Cursors::new(&self.rope, cursors);
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Editedness {
    Edited,
    Unedited
}

impl TextBuffer {
    pub fn editedness(&self) -> Editedness {
        u!{Editedness}

        if self.unedited == self.rope {
            Unedited
        } else {
            Edited
        }
    }

    pub fn set_unedited(&mut self) {
        self.unedited = self.rope.clone();
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HistoryNavOutcome {
    NoTransition,
    Transition(EditedTransition),
    RanOutOfHistory,
}

impl From<Change<Editedness>> for HistoryNavOutcome {
    fn from(c: Change<Editedness>) -> Self {
        u!{HistoryNavOutcome, Editedness, EditedTransition}
        match c {
            change!(Edited, Edited) | change!(Unedited, Unedited) => NoTransition,
            change!(Edited, Unedited) => Transition(ToUnedited),
            change!(Unedited, Edited) => Transition(ToEdited),
        }
    }
}

impl From<HistoryNavOutcome> for Option<EditedTransition> {
    fn from(hno: HistoryNavOutcome) -> Self {
        u!{HistoryNavOutcome}
        match hno {
            RanOutOfHistory | NoTransition => None,
            Transition(t) => Some(t)
        }
    }
}

impl HistoryNavOutcome {
    pub fn ran_out_of_history(&self) -> bool {
        u!{HistoryNavOutcome}
        matches!(self,RanOutOfHistory)
    }
}

impl TextBuffer {
    pub fn redo(&mut self, listener: ppel!()) -> HistoryNavOutcome {
        u!{HistoryNavOutcome}
        let old_editedness = self.editedness();

        if let Some(edit) = self.history.get(self.history_index).cloned() {
            self.apply_edit(edit, ApplyKind::Playback, listener);
            self.history_index += 1;

            change!(old_editedness, self.editedness()).into()
        } else {
            RanOutOfHistory
        }
    }

    pub fn undo(&mut self, listener: ppel!()) -> HistoryNavOutcome {
        u!{HistoryNavOutcome}
        let old_editedness = self.editedness();

        let opt = self.history_index.checked_sub(1)
            .and_then(|new_index|
                self.history.get(new_index).cloned().map(|e| (new_index, e))
            );

        if let Some((new_index, edit)) = opt {
            self.apply_edit(!edit, ApplyKind::Playback, listener);
            self.history_index = new_index;

            change!(old_editedness, self.editedness()).into()
        } else {
            RanOutOfHistory
        } 
    }

    pub fn has_no_edits(&self) -> bool {
        debug_assert_eq!(self.editedness(), Editedness::Unedited);
        self.history.is_empty()
    }
}

//
// View rendering
//

impl TextBuffer {
    pub fn in_bounds<P: Borrow<Position>>(&self, position: P) -> bool {
        in_cursor_bounds(&self.rope, position)
    }

    #[perf_viz::record]
    pub fn find_index<P: Borrow<Position>>(&self, p: P) -> Option<ByteIndex> {
        let rope = &self.rope;
        pos_to_char_offset(rope, p.borrow()).and_then(|o| rope.char_to_byte(o))
    }
}

#[cfg(any(test, feature = "pub_arb"))]
#[macro_use]
pub mod test_macros;
#[cfg(any(test, feature = "pub_arb"))]
#[macro_use]
pub mod tests;
