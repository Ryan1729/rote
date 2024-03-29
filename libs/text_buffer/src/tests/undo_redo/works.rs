use super::{*, assert_eq, dbg};

use arb::text_buffer_with_valid_cursors;

// Historical note: This function preceded `on_these_edits_and_index_regarding_ropes`
// but eventually we decided that caring about the cursor position in certain cases was too much work
// for too little benefit, so we started only caring about the rope data. But in many cases the cursor
// stuff did line up and so those places were left alone since there was not a pressing reason to
// loosen those requirements. And we might find it useful to know if future changes affect the results
// of those tests?
fn on_these_edits_and_index<TestEdits: Borrow<[TestEdit]>>(
    edits: TestEdits,
    index: usize,
) {
    let edits = edits.borrow();

    //TODO generate initial buffer?
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    let mut expected_buffer_at_index: Option<TextBuffer> = None;

    macro_rules! record_if_index_matches {
        // Things like moving cursors that don't exist are, and are expected to be, no-ops
        // that do not get added to the history. So the `edits` len may be different than the
        //  history len.
        () => {
            if buffer.history.len().checked_sub(1) == Some(index) {
                expected_buffer_at_index = Some(deep_clone(&buffer));
            }
        };
    }

    record_if_index_matches!();

    for edit in edits.iter() {
        TestEdit::apply(&mut buffer, (*edit).clone());

        if edit.is_recordable() {
            record_if_index_matches!();
        }
    }

    let expected_buffer_at_index = if let Some(b) = expected_buffer_at_index {
        b
    } else {
        // We expect to get here only if either the index is higher than the amount of valid edits,
        // which includes the case that there are no valid edits at all.

        // The cases where there are no valid edits in the history should be covered by
        // `undo_redo_is_a_no_op_if_there_are_no_valid_edits` so we can just simplify the code
        // here by just letting that case pass.

        // For the cases where there are some valid edits but the index is just too high, the same
        // set of edits with a lower index should be tested by this test.
        return;
    };

    let final_buffer = deep_clone(&buffer);

    let len = buffer.history.len();

    if len != 0 {
        for _ in 0..dbg!(dbg!(len - 1) - index) {
            dbg!();
            buffer.undo(None);
        }
    }

    assert_text_buffer_eq_ignoring_history!(buffer, expected_buffer_at_index);

    for _ in 0..len {
        buffer.redo(None);
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);

    // Redo with no redos left should be a no-op
    for _ in 0..10 {
        dbg!();
        buffer.redo(None);
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);

    for _ in 0..len {
        dbg!();
        dbg!(&mut buffer).undo(None);
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

    // undo with no undos left should be a no-op
    for _ in 0..10 {
        dbg!();
        buffer.undo(None);
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

fn on_these_edits_and_index_regarding_ropes<TestEdits: Borrow<[TestEdit]>>(
    edits: TestEdits,
    index: usize,
) {
    on_these_edits_and_index_regarding_ropes_with_this_buffer(edits, index, d!())
}

fn on_these_edits_and_index_regarding_ropes_with_this_buffer<TestEdits: Borrow<[TestEdit]>>(
    edits: TestEdits,
    index: usize,
    initial_buffer: TextBuffer
) {
    let edits = edits.borrow();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    let mut expected_buffer_at_index: Option<TextBuffer> = None;

    macro_rules! record_if_index_matches {
        // Things like moving cursors that don't exist are, and are expected to be, no-ops
        // that do not get added to the history. So the `edits` len may be different than the
        //  history len.
        () => {
            if buffer.history.len().checked_sub(1) == Some(index) {
                expected_buffer_at_index = Some(deep_clone(&buffer));
            }
        };
    }

    record_if_index_matches!();

    for edit in edits.iter() {
        TestEdit::apply(&mut buffer, (*edit).clone());

        if edit.is_recordable() {
            record_if_index_matches!();
        }
    }

    let expected_buffer_at_index = if let Some(b) = expected_buffer_at_index {
        b
    } else {
        // We expect to get here only if either the index is higher than the amount of valid edits,
        // which includes the case that there are no valid edits at all.

        // The cases where there are no valid edits in the history should be covered by
        // `undo_redo_is_a_no_op_if_there_are_no_valid_edits` so we can just simplify the code
        // here by just letting that case pass.

        // For the cases where there are some valid edits but the index is just too high, the same
        // set of edits with a lower index should be tested by this test.
        return;
    };

    let final_buffer = deep_clone(&buffer);

    let len = buffer.history.len();

    if len != 0 {
        for _ in 0..dbg!(dbg!(len - 1) - index) {
            dbg!();
            buffer.undo(None);
        }
    }

    assert_text_buffer_rope_eq!(buffer, expected_buffer_at_index);

    for _ in 0..len {
        buffer.redo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, final_buffer);

    // Redo with no redos left should be a no-op
    for _ in 0..10 {
        dbg!();
        buffer.redo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, final_buffer);

    for _ in 0..len {
        dbg!();
        dbg!(&mut buffer).undo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, initial_buffer);

    // undo with no undos left should be a no-op
    for _ in 0..10 {
        dbg!();
        buffer.undo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, initial_buffer);
}

fn on_all_these_edits<TestEdits: Borrow<[TestEdit]>>(
    edits: TestEdits,
) {
    let edits = edits.borrow();

    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    let mut expected_buffers: Vec<TextBuffer> = Vec::with_capacity(edits.len());

    for edit in edits.iter() {
        TestEdit::apply(&mut buffer, (*edit).clone());

        expected_buffers.push(deep_clone(&buffer));
    }

    let final_buffer = deep_clone(&buffer);

    let len = buffer.history.len();

    // preconditon
    assert_eq!(
        expected_buffers.len(),
        len,
        "wrong amount of expected_buffers"
    );

    if len != 0 {
        for i in (0..len).rev() {
            assert_text_buffer_eq_ignoring_history!(buffer, expected_buffers[i]);
            buffer.undo(None);
        }
    }

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

    for i in 0..len {
        buffer.redo(None);
        assert_text_buffer_eq_ignoring_history!(buffer, expected_buffers[i]);
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);

    // Redo with no redos left should be a no-op
    for _ in 0..10 {
        dbg!();
        buffer.redo(None);
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);

    for _ in 0..len {
        dbg!();
        dbg!(&mut buffer).undo(None);
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

    // undo with no undos left should be a no-op
    for _ in 0..10 {
        dbg!();
        buffer.undo(None);
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

proptest! {
    #[test]
    fn regarding_ropes((edits, index) in arb::test_edits_and_index(SOME_AMOUNT, TestEditSpec::All)) {
        on_these_edits_and_index_regarding_ropes(edits, index);
    }

    #[test]
    fn on_inserts((edits, index) in arb::test_edits_and_index(SOME_AMOUNT, TestEditSpec::Insert)) {
        on_these_edits_and_index(edits, index);
    }

    #[test]
    fn on_all_inserts(edits in arb::test_edits(SOME_AMOUNT, TestEditSpec::Insert)) {
        on_all_these_edits(edits);
    }

    #[test]
    fn on_non_control_inserts((edits, index) in arb::test_edits_and_index(SOME_AMOUNT, TestEditSpec::RegexInsert("\\PC"))) {
        on_these_edits_and_index(edits, index);
    }

    #[test]
    fn on_non_cr_inserts((edits, index) in arb::test_edits_and_index(SOME_AMOUNT, TestEditSpec::RegexInsert("[^\r]"))) {
        on_these_edits_and_index(edits, index);
    }

    #[test]
    fn on_set_cursor_heavy_edits_regarding_ropes((edits, index) in arb::test_edits_and_index(SOME_AMOUNT, TestEditSpec::SetCursorHeavy)) {
        on_these_edits_and_index_regarding_ropes(edits, index);
    }

    #[test]
    fn on_tab_in_out_heavy_edits_regarding_ropes((edits, index) in arb::test_edits_and_index(SOME_AMOUNT, TestEditSpec::TabInOutHeavy)) {
        on_these_edits_and_index_regarding_ropes(edits, index);
    }

    #[test]
    fn on_delete_and_tab_in_out_heavy_edits_regarding_ropes((edits, index) in arb::test_edits_and_index(SOME_AMOUNT, TestEditSpec::DeleteAndTabInOutHeavy)) {
        on_these_edits_and_index_regarding_ropes(edits, index);
    }

    #[test]
    fn on_delete_then_tab_in_edits_regarding_ropes(
        (edits, index) in arb::test_edit_delete_then_tab_out_vec_and_index(),
        buffer in text_buffer_with_valid_cursors()
    ) {
        on_these_edits_and_index_regarding_ropes_with_this_buffer(edits, index, buffer);
    }

    #[test]
    fn with_heavy_delete_lines_regarding_ropes((edits, index) in arb::test_edits_and_index(SOME_AMOUNT, TestEditSpec::DeleteLinesHeavy)) {
        on_these_edits_and_index_regarding_ropes(edits, index);
    }

    #[test]
    fn on_toggle_case_heavy_edits_regarding_ropes((edits, index) in arb::test_edits_and_index(SOME_AMOUNT, TestEditSpec::ToggleCaseHeavy)) {
        on_these_edits_and_index_regarding_ropes(edits, index);
    }
}

#[test]
fn on_this_near_minimal_set_of_edits() {
    on_these_edits_and_index(
        vec![
            TestEdit::Insert('\u{0}'),
        ],
        0,
    );
}

#[test]
fn on_this_set_of_edits() {
    on_these_edits_and_index(
        vec![
            TestEdit::Insert('\u{b}'),
            TestEdit::Insert('a'),
            TestEdit::Insert('\n'),
        ],
        0,
    );
}

#[test]
fn on_this_toggle_case_set_of_edits() {
    on_these_edits_and_index_regarding_ropes(
        vec![TestEdit::InsertNumbersAtCursors, TestEdit::ToggleCase, TestEdit::ToggleCase],
        0,
    );
}

// Historical note: As of this writing, this is an example of a test which, without the
// rope only loosening, fails.
#[test]
fn on_this_set_of_edits_including_cursor_movement_regarding_ropes() {
    use TestEdit::*;
    on_these_edits_and_index_regarding_ropes(
        vec![
            Insert('a'),
            MoveAllCursors(Move::Left),
        ],
        0,
    );
}

#[test]
fn on_this_set_of_edits_with_a_cut_regarding_ropes() {
    on_these_edits_and_index_regarding_ropes(
        vec![TestEdit::Insert('¡'), TestEdit::Cut, TestEdit::MoveAllCursors(Move::ToLineEnd)],
        0,
    );
}

#[test]
fn on_this_set_of_edits_with_a_cut_and_a_tab_out_regarding_ropes() {
    use TestEdit::*;
    on_these_edits_and_index_regarding_ropes(
        vec![Insert('A'), Cut, Insert('¡'), TabOut],
        0,
    );
}

#[test]
fn on_this_reduced_set_of_edits_with_a_cut_and_a_tab_out_regarding_ropes() {
    use TestEdit::*;
    let initial_buffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, Insert('A'));

    let expected_buffer_at_index = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, Cut);
    TestEdit::apply(&mut buffer, Insert('¡'));
    TestEdit::apply(&mut buffer, TabOut);

    let final_buffer = deep_clone(&buffer);

    let len = buffer.history.len();

    for _ in 0..dbg!(dbg!(len - 1)) {
        dbg!(&buffer);
        buffer.undo(None);
    }

    assert_text_buffer_rope_eq!(buffer, expected_buffer_at_index);

    for _ in 0..len {
        buffer.redo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, final_buffer);

    // Redo with no redos left should be a no-op
    for _ in 0..10 {
        dbg!();
        buffer.redo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, final_buffer);

    for _ in 0..len {
        dbg!();
        dbg!(&mut buffer).undo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, initial_buffer);

    // undo with no undos left should be a no-op
    for _ in 0..10 {
        dbg!();
        buffer.undo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, initial_buffer);
}

#[test]
fn on_this_set_of_edits_with_a_movement_and_a_tab_out_regarding_ropes() {
    use TestEdit::*;
    on_these_edits_and_index_regarding_ropes(
        vec![Insert('A'), MoveAllCursors(Move::ToLineStart), Insert('¡'), TabOut],
        0,
    );
}

#[test]
fn on_this_reduced_set_of_edits_with_a_movement_and_a_tab_out_regarding_ropes() {
    use TestEdit::*;
    let initial_buffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, Insert('A'));

    // precondition
    assert_text_buffer_rope_eq!(buffer, t_b!("A"));

    let expected_buffer_at_index = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, MoveAllCursors(Move::ToLineStart));

    // precondition
    assert_text_buffer_rope_eq!(buffer, t_b!("A"));

    TestEdit::apply(&mut buffer, Insert('¡'));

    // precondition
    assert_text_buffer_rope_eq!(buffer, t_b!("¡A"));

    TestEdit::apply(&mut buffer, TabOut);

    // precondition
    assert_text_buffer_rope_eq!(buffer, t_b!("¡A"));

    let final_buffer = deep_clone(&buffer);

    let len = buffer.history.len();

    for _ in 0..dbg!(dbg!(len - 1)) {
        dbg!(&buffer);
        buffer.undo(None);
    }

    assert_text_buffer_rope_eq!(buffer, expected_buffer_at_index);

    for _ in 0..len {
        buffer.redo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, final_buffer);

    // Redo with no redos left should be a no-op
    for _ in 0..10 {
        dbg!();
        buffer.redo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, final_buffer);

    for _ in 0..len {
        dbg!();
        dbg!(&mut buffer).undo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, initial_buffer);

    // undo with no undos left should be a no-op
    for _ in 0..10 {
        dbg!();
        buffer.undo(None);
    }

    dbg!();
    assert_text_buffer_rope_eq!(buffer, initial_buffer);
}

#[test]
fn in_this_reduced_scenario() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, TestEdit::Insert('\u{b}'));

    let expected_final_buffer = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::Insert('a'));

    let expected_mid_buffer = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::Insert('\n'));

    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, expected_mid_buffer);

    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, expected_final_buffer);
}

#[test]
fn on_this_single_movement_edit() {
    on_these_edits_and_index(vec![TestEdit::MoveAllCursors(Move::Up)], 0);
}

#[test]
fn on_a_wall_hitting_movement_after_an_insert() {
    on_these_edits_and_index(
        vec![TestEdit::Insert('a'), TestEdit::MoveAllCursors(Move::Up)],
        0,
    );
}

#[test]
fn on_this_manually_invented_case() {
    on_these_edits_and_index(
        vec![
            TestEdit::Insert('a'),
            TestEdit::MoveAllCursors(Move::Left),
            TestEdit::Insert('b'),
            TestEdit::MoveAllCursors(Move::Right),
        ],
        2,
    );
}

#[test]
fn on_this_previously_panicking_case() {
    on_these_edits_and_index(
        vec![
            TestEdit::Insert('a'),
            TestEdit::ExtendSelectionForAllCursors(Move::Left),
            TestEdit::Delete,
        ],
        2,
    );
}

#[test]
fn on_this_insert_then_select_case_regarding_ropes() {
    u!{TestEdit}
    on_these_edits_and_index_regarding_ropes(
        vec![Insert(' '), SelectAll],
        0
    );
}

#[test]
fn on_this_cr_lf_case() {
    // sigh
    on_these_edits_and_index(
        vec![TestEdit::Insert('\r'), TestEdit::Insert('\n')],
        0,
    );
}

#[test]
fn on_this_line_end_case() {
    on_these_edits_and_index(
        vec![
            TestEdit::Insert('a'),
            TestEdit::MoveAllCursors(Move::ToLineEnd),
        ],
        0,
    );
}

#[test]
fn in_this_familiar_scenario() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, TestEdit::Insert('1'));

    let buffer_with_1 = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::Insert('2'));

    let buffer_with_2 = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::Insert('3'));

    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_with_2);

    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_with_1);

    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn on_this_backslash_example() {
    /* we want the text buffer to be like this just before the delete lines:
    t_b!("\n\\\n\n\\a", vec1![
        cur!{l 3 o 2},
        cur!{l 3 o 1}
    ]);
    */

    on_these_edits_and_index(
        vec![
            TestEdit::Insert('\n'),
            TestEdit::Insert('\\'),
            TestEdit::Insert('\n'),
            TestEdit::Insert('\n'),
            TestEdit::Insert('\\'),
            TestEdit::Insert('a'),
            TestEdit::SetCursor(pos!{l 3 o 2}, ReplaceOrAdd::Replace),
            TestEdit::SetCursor(pos!{l 3 o 1}, ReplaceOrAdd::Add),
            TestEdit::DeleteLines
        ],
        0
    );
}

#[test]
fn on_this_simple_insert_delete_case() {
    on_these_edits_and_index(vec![TestEdit::Insert('a'), TestEdit::Delete], 0);
}

/*
The API changed, and this is just a reduction anyway
#[test]
fn on_this_reduced_simple_insert_delete_case() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    let inserted_char = 'a';

    TestEdit::apply(&mut buffer, TestEdit::Insert(inserted_char));

    let buffer_with_a = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::Delete);

    dbg!();
    let delete_edit = buffer
        .history
        .get(buffer.history_index.checked_sub(1).unwrap())
        .unwrap();

    match &delete_edit.range_edits().first().delete_range {
        Some(r_e) => assert_eq!(r_e.chars, crate::char_to_string(inserted_char)),
        _ => assert!(false),
    }

    dbg!();
    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_with_a);
    dbg!();
    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}*/



#[test]
fn on_this_insert_numbers_then_move_case_regarding_ropes() {
    on_these_edits_and_index_regarding_ropes(
        vec![TestEdit::InsertNumbersAtCursors, TestEdit::MoveAllCursors(Move::ToPreviousLikelyEditLocation)],
        0,
    );
}

#[test]
fn on_this_reduced_insert_numbers_then_move_case_regarding_ropes() {
    on_these_edits_and_index_regarding_ropes(
        vec![TestEdit::Insert('0'), TestEdit::MoveAllCursors(Move::Left)],
        0,
    );
}

#[test]
fn on_this_move_to_line_start_case_regarding_ropes() {
    on_these_edits_and_index_regarding_ropes(
        vec![
            TestEdit::Insert('¡'),
            TestEdit::ExtendSelectionForAllCursors(Move::ToLineStart),
            TestEdit::Delete,
        ],
        0,
    );
}

#[test]
fn on_this_case_involving_moving_a_missing_cursor() {
    use TestEdit::*;
    on_these_edits_and_index([Insert('0'), MoveCursors(1, Move::Up)], 0);
}

#[test]
fn on_this_case_involving_two_characters_at_once() {
    on_these_edits_and_index([InsertString!("¡A")], 0);
}

#[test]
fn on_this_reduced_case_involving_two_characters_at_once() {
    //TODO generate initial buffer?
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, InsertString!("¡A"));

    dbg!(&mut buffer).undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

    // undo with no undos left should be a no-op
    for _ in 0..3 {
        dbg!();
        buffer.undo(None);
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn on_this_case_involving_two_characters_at_once_then_a_newline_then_dragging_regarding_ropes() {
    on_these_edits_and_index_regarding_ropes(
        [
            InsertString!("Aa"),
            InsertString!("\n"),
            TestEdit::DragCursors(pos! {l 0 o 0}),
        ],
        0,
    );
}

#[test]
fn on_this_case_involving_a_select_bewtween_char_type_grouping_and_non_ascii_chars_regarding_ropes()
{
    on_these_edits_and_index_regarding_ropes(
        [
            InsertString!("ࠀ\u{e000}㐀"),
            TestEdit::SelectCharTypeGrouping(pos! {l 0 o 0}, ReplaceOrAdd::Add),
            InsertString!("a¡"),
        ],
        0,
    );
}

#[test]
fn on_this_smaller_case_involving_a_select_bewtween_char_type_grouping_and_non_ascii_chars_regarding_ropes(
) {
    on_these_edits_and_index_regarding_ropes(
        [
            InsertString!("¡㐀"),
            TestEdit::SelectCharTypeGrouping(pos! {l 0 o 0}, ReplaceOrAdd::Add),
            InsertString!("a¡"),
        ],
        0,
    );
}

#[test]
fn on_this_reduced_case_involving_a_select_bewtween_char_type_grouping_and_non_ascii_chars(
) {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, InsertString!("¡㐀"));

    TestEdit::apply(
        &mut buffer,
        TestEdit::SelectCharTypeGrouping(pos! {l 0 o 0}, ReplaceOrAdd::Add),
    );

    let expected_buffer_after_second_edit = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, InsertString!("a¡"));

    dbg!(&mut buffer);

    buffer.undo(None);

    dbg!(&mut buffer);

    assert_text_buffer_eq_ignoring_history!(buffer, expected_buffer_after_second_edit);
}

#[test]
fn in_this_simple_found_case() {
    let mut buffer: TextBuffer = d!();

    let buffer_before_1 = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::Insert('1'));

    // precondition
    assert_eq!(buffer.borrow_rope().to_string(), "1");

    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_before_1);
    assert_eq!(buffer.borrow_rope().to_string(), "");
}

#[test]
fn as_expected_for_a_small_edit_count_in_this_set_of_examples() {
    const SMALL: u8 = 4;
    const PAST_SMALL: u8 = SMALL + 2;
    let mut buffer: TextBuffer<{SMALL as usize}> = d!();

    for i in 1..=SMALL {
        buffer.insert(std::char::from_digit(i as _, 10).unwrap(), None);
    }

    // precondition
    assert_eq!(buffer.borrow_rope().to_string(), "1234");

    for _ in 0..SMALL {
        buffer.undo(None);
    }

    for i in 1..=PAST_SMALL {
        buffer.insert(std::char::from_digit(i as _, 10).unwrap(), None);
    }

    // precondition
    assert_eq!(buffer.borrow_rope().to_string(), "123456");

    for _ in 0..SMALL {
        buffer.undo(None);
    }

    assert_eq!(buffer.borrow_rope().to_string(), "12");
    assert_eq!(buffer.editedness(), Editedness::Edited);

    // Undo with no history left is a no-op.
    buffer.undo(None);

    assert_eq!(buffer.borrow_rope().to_string(), "12");
    assert_eq!(buffer.editedness(), Editedness::Edited);

    for _ in 0..SMALL {
        buffer.redo(None);
    }

    assert_eq!(buffer.borrow_rope().to_string(), "123456");

    // Redo with no edits left is a no-op.
    buffer.redo(None);

    assert_eq!(buffer.borrow_rope().to_string(), "123456");

    for i in (1..=PAST_SMALL).rev() {
        buffer.insert(std::char::from_digit(i as _, 10).unwrap(), None);
    }

    assert_eq!(buffer.borrow_rope().to_string(), "123456654321");

    for _ in 0..SMALL {
        buffer.undo(None);
    }

    assert_eq!(buffer.borrow_rope().to_string(), "12345665");
    assert_eq!(buffer.editedness(), Editedness::Edited);

    buffer.undo(None);

    assert_eq!(buffer.borrow_rope().to_string(), "12345665");
    assert_eq!(buffer.editedness(), Editedness::Edited);
}

#[test]
fn in_this_sequential_case() {
    let mut buffer: TextBuffer = d!();

    TestEdit::apply(&mut buffer, TestEdit::Insert('1'));
    TestEdit::apply(&mut buffer, TestEdit::Insert('2'));
    TestEdit::apply(&mut buffer, TestEdit::Insert('3'));

    let buffer_after_3 = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::Insert('4'));
    TestEdit::apply(&mut buffer, TestEdit::Insert('5'));

    // precondition
    assert_eq!(buffer.borrow_rope().to_string(), "12345");

    buffer.undo(None);
    buffer.undo(None);

    // precondition
    assert_text_buffer_eq_ignoring_history!(buffer, buffer_after_3);
    assert_eq!(buffer.borrow_rope().to_string(), "123");

    TestEdit::apply(&mut buffer, TestEdit::Insert('6'));

    // precondition
    assert_eq!(buffer.borrow_rope().to_string(), "1236");
    let buffer_after_6 = deep_clone(&buffer);

    buffer.redo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_after_6);
    assert_eq!(buffer.borrow_rope().to_string(), "1236");
}

#[test]
fn when_pasting_in_this_case() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, TestEdit::Insert('1'));

    TestEdit::apply(&mut buffer, TestEdit::InsertString("234".to_string()));

    let buffer_after_paste: TextBuffer = deep_clone(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::Insert('5'));

    // precondition
    assert_eq!(buffer.borrow_rope().to_string(), "12345");

    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_after_paste);

    buffer.undo(None);
    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}