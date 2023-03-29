use super::{*, assert_eq};

use pub_arb_edit::edit_with_cursors;

#[allow(unused)] //used in macro below.
fn arb_edit_from_buffer(text_buffer: TextBuffer) -> impl Strategy<Value = Edit> {
    edit_with_cursors(text_buffer.rope)
}

prop_compose! {
    fn arb_no_history_text_buffer_and_edit()
    (text_buffer in arb::no_history_text_buffer())
    (edit in arb_edit_from_buffer(deep_clone(&text_buffer)), t_b in Just(text_buffer)) -> (TextBuffer, Edit) {
        (t_b, edit)
    }
}

// After some thought I am unable to establish a relationship between this property holding and
// the property we actually care about, undo/redo working. It seemed intuitive that either this
// property would imply undo/redo works or vice versa. But the closest I have come to
// demonstrating a link requires assuming that there is only one edit that produces a given rope
// to rope transition, which is clearly false, (sometimes moving the cursor one spec doen the same
// thing as Home/End.) So, at this time it does not seem worth it to try to make this property
// hold. But it feels like it might make sense to do this later, and it also feels like without
// a reminder of this happening before, it might happen again so I will leave this commented out.
/*
proptest! {
    #[test]
    fn edits_double_negate_properly(edit in edit_arb::edit()) {
        let initial = edit.clone();

        assert_eq!(!!edit, initial);
    }
}
*/

#[allow(dead_code)]
fn negated_edit_undo_redos_properly(initial_buffer: TextBuffer, edit: Edit) {
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    apply_edit(&mut buffer.rope, &edit, None);

    let modified_buffer = deep_clone(&buffer);

    apply_edit(&mut buffer.rope, &(!&edit), None);

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

    apply_edit(&mut buffer.rope, &edit, None);

    assert_text_buffer_eq_ignoring_history!(buffer, modified_buffer);
}

// I am more confident that this weaker theorem follows directly from undo/redo
// working. It is essentially the statement that undo/redo works for a single action.
// However,it is complicated to generate valid edits for this, whereas the method
// used in `undo_redo::works::on_these_edits_and_index` (seemingly?) generates valid
// edits every time.
// So let's skip these for now.
proptest! {
    //#[test]
    fn negated_edits_undo_redo_properly(
        (initial_buffer, edit) in arb_no_history_text_buffer_and_edit()
    ) {
        negated_edit_undo_redos_properly(initial_buffer, edit)
    }
}

//#[test]
// fn negated_edits_undo_redo_this_delete_edit() {
//     negated_edit_undo_redos_properly(
//         d!(),
//         Edit::Delete(
//             Vec1::new(CharEdit { s: "0".to_owned(), offsets: (Some(AbsoluteCharOffset(0)), None) }),
//             d!()
//         )
//     )
// }

#[test]
fn negated_edits_undo_redo_this_edit_that_only_changes_the_sticky_offset() {
    let new_cursor = {
        let mut c: Cursor = d!();
        c.sticky_offset = CharOffset(1);
        c
    };

    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    let edit: Edit = Change {
        // If the first old change does not correspond to the initial buffer, then undoing to that
        // state can fail to match the initila buffer.
        old: buffer.borrow_cursors().clone(),
        new: Cursors::new(&buffer.borrow_rope(), Vec1::new(new_cursor.clone())),
    }
    .into();

    apply_edit(&mut buffer.rope, &edit, None);

    let modified_buffer = deep_clone(&buffer);

    assert_eq!(modified_buffer.borrow_cursors().first(), &new_cursor);

    let undo_edit = !(&edit);

    match (undo_edit.cursors(), edit.cursors()) {
        (u, e) => {
            assert_eq!(u.old, e.new);
            assert_eq!(u.new, e.old);
        }
    }

    apply_edit(&mut buffer.rope, &undo_edit, None);

    assert_eq!(buffer.borrow_cursors().first(), initial_buffer.borrow_cursors().first());

    apply_edit(&mut buffer.rope, &edit, None);

    assert_eq!(buffer.borrow_cursors().first(), modified_buffer.borrow_cursors().first());
}

#[test]
fn undo_undoes() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    buffer.insert('a', None);
    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn redo_redoes() {
    let mut buffer: TextBuffer = d!();
    buffer.insert('a', None);

    let final_buffer: TextBuffer = deep_clone(&buffer);

    buffer.undo(None);

    buffer.redo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);
}

proptest! {
    #[test]
    fn undo_redo_is_a_no_op_if_there_are_no_valid_edits(
        s in ".*"
    ) {
        let initial_buffer: TextBuffer = t_b!(s);
        let mut buffer: TextBuffer = deep_clone(&initial_buffer);

        // Redo with no redos left should be a no-op
        for _ in 0..3 {
            buffer.redo(None);
            assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
        }

        // undo with no undos left should be a no-op
        for _ in 0..3 {
            buffer.undo(None);
            assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
        }
    }
}