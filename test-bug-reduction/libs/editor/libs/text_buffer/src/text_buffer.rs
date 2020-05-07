#![deny(dead_code)]
#![deny(unused_imports)]
#![deny(unused_macros)]

use editor_types::{Cursor, SetPositionAction};
use macros::{d, dbg, u};
use panic_safe_rope::{Rope, RopeSlice};
use platform_types::{*, screen_positioning::*};
use rope_pos::{clamp_position};

use std::cmp::{max, min};
use std::collections::VecDeque;

mod edit;
use edit::{Applier, Change, Edit};
use rope_pos::{strict_offset_pair};

mod cursors;
use cursors::Cursors;

#[derive(Clone, Debug, Default)]
pub struct TextBuffer {
    /// We keep the rope private, and only allow non-mut borrows
    /// so we know that the history contains all the changes made
    /// to the rope.
    rope: Rope,
    cursors: Cursors,
    history: VecDeque<Edit>,
    history_index: usize,
    unedited_index: usize,
    pub scroll: ScrollXY,
}

impl TextBuffer {
    pub fn len(&self) -> usize {
        self.borrow_rope().chars().count()
    }

    pub fn borrow_rope(&self) -> &Rope {
        &self.rope
    }

    pub fn clone_rope(&self) -> Rope {
        self.rope.clone()
    }
}

impl From<String> for TextBuffer {
    fn from(s: String) -> Self {
        let mut output: Self = d!();

        output.rope = Rope::from(s);

        output
    }
}

impl From<&str> for TextBuffer {
    fn from(s: &str) -> Self {
        let mut output: Self = d!();

        output.rope = Rope::from(s);

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
enum ApplyKind {
    Record
}

pub type PossibleEditedTransition = Option<EditedTransition>;

impl TextBuffer {
    #[perf_viz::record]
    pub fn delete_lines(&mut self) -> PossibleEditedTransition {
        self.record_edit(
            edit::get_delete_lines_edit(&self.rope, &self.cursors)
        )
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

    #[perf_viz::record]
    fn record_edit(&mut self, edit: Edit) -> PossibleEditedTransition {
        let old_editedness = self.editedness();
        dbg!(old_editedness);

        self.apply_edit(edit, ApplyKind::Record);

        dbg!(self.editedness());
        change!(old_editedness, self.editedness()).into()
    }

    #[perf_viz::record]
    fn apply_edit(&mut self, edit: Edit, kind: ApplyKind) {
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
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Editedness {
    Edited,
    Unedited
}

impl From<Change<Editedness>> for Option<EditedTransition> {
    fn from(c: Change<Editedness>) -> Self {
        u!{Editedness, EditedTransition}
        match c {
            change!(Edited, Edited) | change!(Unedited, Unedited) => None,
            change!(Edited, Unedited) => Some(ToUnedited),
            change!(Unedited, Edited) => Some(ToEdited),
        }
    }
}

impl TextBuffer {
    fn editedness(&mut self) -> Editedness {
        u!{Editedness}
        // TODO can we make this faster by like, defining an algebra for edits or something,
        // that allows us to compose edits together, and then check if we go the zero/identity
        // edit when we combine all of the ones from the current index to the edited index?
        
        let mut rope_copy = self.rope.clone();
        let mut cursors_copy = self.cursors.clone();

        let mut history_index = self.history_index;

        while let Some((i, edit)) = history_index.checked_sub(1)
            .and_then(|new_index|
                self.history.get(new_index).cloned().map(|e| (new_index, e))
            ) {
            dbg!(i, &edit);

            let applier = Applier::new(
                &mut rope_copy,
                &mut cursors_copy
            );
            edit::apply(applier, &(!edit));

            history_index = i;
        }

        if rope_copy == self.rope {
            self.unedited_index = self.history_index;
            Unedited
        } else { 
            Edited
        }
    }
}

