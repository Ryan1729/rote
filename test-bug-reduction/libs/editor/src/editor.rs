/*#![deny(dead_code)]
#![deny(unused_imports)]
#![deny(unused_macros)]*/
//#![deny(unused_variables)]

use rope_pos::{clamp_position, AbsoluteCharOffsetRange};

use editor_types::{Cursor, cur};
use macros::{d, dbg, u};
use panic_safe_rope::{Rope, RopeSlice};
use platform_types::{*};


use std::collections::VecDeque;

use g_i::SelectableVec1;

#[derive(Clone, Debug, Default)]
struct EditorBuffer {
    text_buffer: TextBuffer,
    name: BufferName,
}

impl From<&EditorBuffer> for String {
    fn from(e_b: &EditorBuffer) -> Self {
        (&e_b.text_buffer).into()
    }
}

impl From<&mut EditorBuffer> for String {
    fn from(e_b: &mut EditorBuffer) -> Self {
        (&e_b.text_buffer).into()
    }
}

impl EditorBuffer {
    fn new<I: Into<TextBuffer>>(name: BufferName, s: I) -> Self {
        Self {
            name,            text_buffer: s.into(),
            ..d!()
        }
    }
}

/// The collection of files opened for editing, and/or in-memory scratch buffers.
/// Guaranteed to have at least one buffer in it at all times.
#[derive(Clone, Debug, Default)]
struct EditorBuffers {
    buffers: SelectableVec1<EditorBuffer>,
}

impl EditorBuffers {
    /// Since there is always at least one buffer, this always returns at least 1.
    fn len(&self) -> g_i::Length {
        self.buffers.len()
    }

    /// The index of the currectly selected buffer.
    fn current_index(&self) -> g_i::Index {
        self.buffers.current_index()
    }

    fn get_current_buffer_mut(&mut self) -> &mut EditorBuffer {
        self.buffers.get_current_element_mut()
    }

    fn append_index(&self) -> g_i::Index {
        self.buffers.append_index()
    }

    fn index_with_name(&self, name: &BufferName) -> Option<g_i::Index> {
        let mut index = None;
        for (i, buffer) in self.buffers.iter_with_indexes() {
            if buffer.name == *name {
                index = Some(i);
                break;
            }
        }
        index
    }

    fn buffers(&self) -> &SelectableVec1<EditorBuffer> {
        &self.buffers
    }
}

#[derive(Debug, Default)]
struct State {    buffers: EditorBuffers,
    view: View,
}

fn update_and_render(state: &mut State, input: Input) -> UpdateAndRenderOutput {
    perf_viz::record_guard!("update_and_render");

    macro_rules! text_buffer_call {
        (sync $buffer: ident . $($method_call:tt)*) => {
            text_buffer_call!($buffer . $($method_call)*);
        };
        ($buffer: ident . $($method_call:tt)*) => {
            text_buffer_call!($buffer {$buffer.$($method_call)*})
        };
        (sync $buffer: ident $tokens:block) => {{
            text_buffer_call!($buffer $tokens);
        }};
        ($buffer: ident $tokens:block) => {{
            let $buffer = &mut state.buffers.get_current_buffer_mut().text_buffer;
                $tokens;
        }}
    }

    dbg!(&input);

    let mut cmd = Cmd::NoCmd;
    
    state.view.edited_transitions.clear();

    u!{EditedTransition};

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
            // Since this may be an expression with sidee effects, 
            // we want this to be evaluated whether or not we want
            // to record the transition.
            let transition = $transition;

                let transition: Option<EditedTransition> = transition.into();
    
                if let Some(transition) = transition {
                    state.view.edited_transitions.push((
                        $index,
                        transition,
                    ));
                }
            
        }};
    }

    u!{Input}
    match input {
        AddOrSelectBuffer(name, str) => {
            state.buffers.buffers.push_and_select_new(EditorBuffer::new(name, str));

            mark_edited_transition!(current, ToUnedited);
        }
        AdjustBufferSelection(adjustment) => {
            state.buffers.buffers.adjust_selection(adjustment);
        }
        DeleteLines => text_buffer_call!(sync b {
            mark_edited_transition!(current, b.delete_lines());
        }),
        _ => {}
    }

    (state.view.clone(), cmd)
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
struct Cursors {
    // Should be sorted in reverse order (positions later in a test file will have lower indexes)
    // and no two cursors should be overlapping or have overlapping highlight regions. The order
    // ensures that the edit don't screw the indexes up, and the lack of overlaps ensures that
    // edits don't overwrite each other in a single action.
    cursors: Vec1<Cursor>,
}

#[macro_export]
macro_rules! curs {
    ($rope: expr, $($cursor_elements: expr),+ $(,)?) => (
        Cursors::new(&$rope, vec1![$($cursor_elements)+])
    );
}

impl Cursors {
    /// We require a rope parameter only so we can make sure the cursors are within the given
    /// rope's bounds.
    fn new(rope: &Rope, mut cursors: Vec1<Cursor>) -> Self {
        cursors.sort();
        cursors.reverse();

        Self::clamp_vec_to_rope(&mut cursors, rope);

        Self { cursors }
    }


    fn clamp_vec_to_rope(cursors: &mut Vec1<Cursor>, rope: &Rope) {
        for cursor in cursors.iter_mut() {
            if let Some(h) = cursor.get_highlight_position() {
                cursor.set_highlight_position(clamp_position(rope, h));
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
struct TextBuffer {
    /// We keep the rope private, and only allow non-mut borrows
    /// so we know that the history contains all the changes made
    /// to the rope.
    rope: Rope,
    cursors: Cursors,
    history: VecDeque<Edit>,
    history_index: usize,
    unedited_index: usize,
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

type PossibleEditedTransition = Option<EditedTransition>;

impl TextBuffer {
    #[perf_viz::record]
    fn delete_lines(&mut self) -> PossibleEditedTransition {
        let edit = {
            let range_edits = Vec1::new({
                let range = AbsoluteCharOffsetRange::new(AbsoluteCharOffset(0), AbsoluteCharOffset(1));
                let chars = "anything non-empty".to_owned();

                let range_edit = RangeEdit { chars, range };
                
                RangeEdits {
                    delete_range: Some(range_edit),
                   ..d!()
                }
            });


            Edit {
                range_edits,
                cursors: Change {
                    new: self.cursors.clone(),
                    old: self.cursors.clone(),
                },
            }
        };
        
        let old_editedness = self.editedness();
        dbg!(old_editedness);

        apply(&mut self.rope, &edit);

        self.history.truncate(self.history_index);
        self.history.push_back(edit);
        self.history_index += 1;

        dbg!(self.editedness());
        change!(old_editedness, self.editedness()).into()
    }}

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

        let mut history_index = self.history_index;

        while let Some((i, edit)) = history_index.checked_sub(1)
            .and_then(|new_index|
                self.history.get(new_index).cloned().map(|e| (new_index, e))
            ) {
            dbg!(i, &edit);

            apply(&mut rope_copy, &(!edit));

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

fn apply<'rope, 'cursors>(rope: &mut Rope, edit: &Edit) {
    // we assume that the edits are in the proper order so we won't mess up our indexes with our
    // own inserts and removals. I'm not positive that there being a single order that works
    // is possible for all possible edits, but in practice I think the edits we will actually
    // produce will work out. The tests should tell us if we're wrong!
    for range_edit in edit.range_edits.iter() {
        range_edit.apply(rope);
    }
}

#[derive(Debug)]
enum SpecialHandling {
    None,
}
d!(for SpecialHandling: SpecialHandling::None);

#[derive(Debug)]
enum PostDeltaShift {
    None,
}
d!(for PostDeltaShift: PostDeltaShift::None);

#[derive(Debug, Default)]
struct CursorPlacementSpec {
    offset: AbsoluteCharOffset,
    delta: isize,
    special_handling: SpecialHandling,
    post_delta_shift: PostDeltaShift,
}

/// `range_edits` and the two `Vec1`s in `cursors` must all be the same length.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct Edit {
    range_edits: Vec1<RangeEdits>,
    cursors: Change<Cursors>,
}

impl std::ops::Not for Edit {
    type Output = Edit;

    fn not(self) -> Self::Output {
        let reversed = self
            .range_edits
            .mapped(|r_e| !r_e)
            .into_vec()
            .into_iter()
            .rev()
            .collect::<Vec<_>>();
        Edit {
            // This unwrap is fine because `self.range_edits` was a `Vec1`.
            range_edits: Vec1::try_from_vec(reversed).unwrap(),
            cursors: !self.cursors,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct RangeEdit {
    chars: String,
    range: AbsoluteCharOffsetRange,
}

/// Some seemingly redundant information is stored here, for example the insert's range's maximum
/// is never read. But that information is needed if the edits are ever negated, which swaps the
/// insert and delete ranges.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
struct RangeEdits {
    /// The characters to insert and a range that only the minimum of which is used as the
    /// insertion point. Applied second.
    insert_range: Option<RangeEdit>,
    /// The characters to delete and where to delete them from. Applied first.
    delete_range: Option<RangeEdit>,
}

impl RangeEdits {
    fn apply(&self, rope: &mut Rope) {
        if let Some(RangeEdit { range, .. }) = self.delete_range {
            rope.remove(range.range());
        }

        if let Some(RangeEdit {
            ref chars, range, ..
        }) = self.insert_range
        {
            rope.insert(range.min(), chars);
        }
    }
}

impl std::ops::Not for RangeEdits {
    type Output = RangeEdits;

    fn not(self) -> Self::Output {
        RangeEdits {
            insert_range: self.delete_range,
            delete_range: self.insert_range,
        }
    }
}

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
struct Change<T> {
    old: T,
    new: T,
}

impl<T> std::ops::Not for Change<T> {
    type Output = Change<T>;

    fn not(self) -> Self::Output {
        let Change { old, new } = self;

        Change { old: new, new: old }
    }
}

#[macro_export]
macro_rules! change {
    //
    // Pattern matching
    //
    ($old: ident, $new: ident) => {
        Change { old: $old, new: $new }
    };
    (_, $new: ident) => {
        Change { old: _, new: $new }
    };
    ($old: ident, _) => {
        Change { old: $old, new: _ }
    };
    //
    // Initialization
    //
    ($old: expr, $new: expr) => {
        Change { old: $old, new: $new }
    };
}
fn copy_string(rope: &Rope, range: AbsoluteCharOffsetRange) -> String {
    rope.slice(range.range())
        .map(|slice| {
            let s: String = slice.into();
            s
        })
        .unwrap_or_default()
}

#[cfg(test)]
mod tests;