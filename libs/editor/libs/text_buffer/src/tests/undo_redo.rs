// This module is inside `tests`
use super::*;

prop_compose! {
    fn arb_no_history_text_buffer()
    (rope in arb_rope())
    (cursors in arb_cursors(rope.chars().count()), r in Just(rope)) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

prop_compose! {
    fn arb_range_edit(max_len: usize)
    (chars in ".*", range in arb_absolute_char_offset_range(max_len)) -> RangeEdit {
        RangeEdit {
            chars,
            range
        }
    }
}

prop_compose! {
    fn arb_range_edits(max_len: usize)
    (insert_range in option::of(arb_range_edit(max_len)), delete_range in option::of(arb_range_edit(max_len))) -> RangeEdits {
        RangeEdits {
            insert_range,
            delete_range,
        }
    }
}

prop_compose! {
    fn arb_edit()
    (len in 1..SOME_AMOUNT)
    (range_edits in vec1(arb_range_edits(len), len), cursors in arb_change!(arb_cursors(len))) -> Edit {
        Edit {
            range_edits,
            cursors,
        }
    }
}

fn arb_edit_from_buffer(text_buffer: TextBuffer) -> impl Strategy<Value = Edit> {
    let cs = text_buffer.cursors.clone();
    arb_edit()
    .prop_map(move |mut edit| {
        edit.cursors.old = cs.clone();
        edit
    })
}

prop_compose! {
    fn arb_no_history_text_buffer_and_edit()
    (text_buffer in arb_no_history_text_buffer())
    (edit in arb_edit_from_buffer(deep_clone(&text_buffer)), t_b in Just(text_buffer)) -> (TextBuffer, Edit) {
        (t_b, edit)
    }
}

// After some thought I am unable to establish a relationship between this property holding and
// the property we actually care about, undo/redo working. It seemed intuitive that either this
// property would imply undo/redo works or vice versa. But the closest I have come to
// demonstrating a link requires assumiong that there is only one edit that produces a given rope
// to rope transition, which is clearly false, (sometimes moving the cursor one spec doen the same
// thing as Home/End.) So, at this time it does not seem worth it to try to make this property
// hold. But it feels like it might make sense to do this later, and it also feels like without
// a reminder of this happneing before, it moght happen again so I will leave this commented out.
/*
proptest! {
    #[test]
    fn edits_double_negate_properly(edit in arb_edit()) {
        let initial = edit.clone();

        assert_eq!(!!edit, initial);
    }
}
*/

fn negated_edit_undo_redos_properly(initial_buffer: TextBuffer, edit: Edit) {
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    buffer.apply_edit(edit.clone(), ApplyKind::Record);

    let modified_buffer = deep_clone(&buffer);

    buffer.apply_edit(!(edit.clone()), ApplyKind::Playback);

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

    buffer.apply_edit(edit, ApplyKind::Playback);

    assert_text_buffer_eq_ignoring_history!(buffer, modified_buffer);
}

// I am more confidnent that this weaker theorem follows directly from undo/redo working. It is
// essentially the statement that undo/redo works for a single action.
// However,it is complicated to generate valid edits for this, whereas the method used in
// `undo_redo_works_on_these_edits_and_index` (seemingly?) generates valid edits every time.
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
        old: buffer.cursors.clone(),
        new: Vec1::new(new_cursor.clone())
    }.into();

    buffer.apply_edit(edit.clone(), ApplyKind::Record);

    let modified_buffer = deep_clone(&buffer);

    assert_eq!(modified_buffer.cursors.first(), &new_cursor);

    let undo_edit = !(edit.clone());

    match (&undo_edit.cursors, &edit.cursors) {
        (u, e) => {
            assert_eq!(u.old, e.new);
            assert_eq!(u.new, e.old);
        }
    }

    buffer.apply_edit(undo_edit, ApplyKind::Playback);

    assert_eq!(buffer.cursors.first(), initial_buffer.cursors.first());

    buffer.apply_edit(edit, ApplyKind::Playback);

    assert_eq!(buffer.cursors.first(), modified_buffer.cursors.first());
}


#[derive(Debug, Clone)]
enum TestEdit {
    Insert(char),
    InsertString(String),
    Delete,
    MoveAllCursors(Move),
    ExtendSelectionForAllCursors(Move),
    MoveCursors(usize, Move),
    ExtendSelection(usize, Move),
    SetCursor(Position, ReplaceOrAdd),
    DragCursors(Position),
    SelectCharTypeGrouping(Position, ReplaceOrAdd),
    Cut,
}

fn apply_edit(buffer: &mut TextBuffer, edit: TestEdit) {
    use TestEdit::*;
    match edit {
        Insert(c) => buffer.insert(c),
        InsertString(s) => buffer.insert_string(s),
        Delete => buffer.delete(),
        MoveAllCursors(r#move) => buffer.move_all_cursors(r#move),
        ExtendSelectionForAllCursors(r#move) => {
            buffer.extend_selection_for_all_cursors(r#move)
        },
        MoveCursors(index, r#move) => buffer.move_cursor(index, r#move),
        ExtendSelection(index, r#move) => buffer.extend_selection(index, r#move),
        SetCursor(position, replace_or_add) => buffer.set_cursor(position, replace_or_add),
        DragCursors(position) => buffer.drag_cursors(position),
        SelectCharTypeGrouping(position, replace_or_add) =>
            buffer.select_char_type_grouping(position, replace_or_add),
        Cut => {buffer.cut_selections();},
    }
}

fn arb_replace_or_add() -> impl Strategy<Value = ReplaceOrAdd> {
    prop_oneof![
        Just(ReplaceOrAdd::Replace),
        Just(ReplaceOrAdd::Add),
    ]
}


fn arb_test_edit() -> impl Strategy<Value = TestEdit> {
    use TestEdit::*;
    prop_oneof![
        Just(Delete),
        any::<char>().prop_map(Insert),
        ".*".prop_map(InsertString),
        arb_move().prop_map(MoveAllCursors),
        arb_move().prop_map(ExtendSelectionForAllCursors),
        // Moving a cursor that isn't there should just be a no-op
        (0..MORE_THAN_SOME_AMOUNT, arb_move()).prop_map(|(i, m)| MoveCursors(i, m)),
        (0..MORE_THAN_SOME_AMOUNT, arb_move()).prop_map(|(i, m)| ExtendSelection(i, m)),
        // The user can attempt to move the cursor to invalid positions,
        // and there cursor may get snapped to a valid position producing an actual movement.
        (arb_pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT), arb_replace_or_add()).prop_map(|(p, r)| SetCursor(p, r)),
        arb_pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT).prop_map(DragCursors),
        (arb_pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT), arb_replace_or_add()).prop_map(|(p, r)| SelectCharTypeGrouping(p, r)),
        Just(Cut),
    ]
}

fn arb_test_edit_insert() -> impl Strategy<Value = TestEdit> {
    any::<char>().prop_map(TestEdit::Insert)
}

type Regex = &'static str;

fn arb_test_edit_regex_insert(regex: Regex) -> impl Strategy<Value = TestEdit> {
    regex.prop_map(|s| TestEdit::Insert(s.chars().next().unwrap_or('a')))
}

enum ArbTestEditSpec {
    All,
    Insert,
    RegexInsert(Regex),
}

fn arb_test_edits(max_len: usize, spec: ArbTestEditSpec) -> impl Strategy<Value = Vec<TestEdit>> {
    collection::vec(
        match spec {
            ArbTestEditSpec::All => arb_test_edit().boxed(),
            ArbTestEditSpec::Insert => arb_test_edit_insert().boxed(),
            ArbTestEditSpec::RegexInsert(regex) => arb_test_edit_regex_insert(regex).boxed(),
        },
        0..max_len
    )
}

prop_compose! {
    fn arb_test_edits_and_index(max_len: usize, spec: ArbTestEditSpec)
        (edits in arb_test_edits(max_len, spec))
        (
            i in if edits.len() == 0 { 0..1 } else { 0..edits.len() },
            es in Just(edits)
         ) -> (Vec<TestEdit>, usize) {
        (es, i)
    }
}

// `Rope`s share backing buffers when cloned, so we want to avoid that.
fn deep_clone(buffer: &TextBuffer) -> TextBuffer {
    let s: std::borrow::Cow<str> = (&buffer.rope).into();
    TextBuffer {
        rope: Rope::from_str(&s),
        ..buffer.clone()
    }
}

#[test]
fn undo_undoes() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    buffer.insert('a');
    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn redo_redoes() {
    let mut buffer: TextBuffer = d!();
    buffer.insert('a');

    let final_buffer: TextBuffer = deep_clone(&buffer);

    buffer.undo();

    buffer.redo();

    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);
}

proptest! {
    #[test]
    fn undo_redo_is_a_no_op_if_there_are_no_valid_edits(
        edits in arb_test_edits(SOME_AMOUNT, ArbTestEditSpec::All)
    ) {
        //TODO generate initial buffer? This would simulate a load from a new file.
        let initial_buffer: TextBuffer = d!();
        let mut buffer: TextBuffer = deep_clone(&initial_buffer);

        for edit in edits.iter() {
            apply_edit(&mut buffer, (*edit).clone());

            // The cases where there are valid edits in the history should be covered by tests
            // that call `undo_redo_works_on_these_edits_and_index` so we can just simplify the code
            // here by just letting that case pass.
            if !text_buffer_eq_ignoring_history!(buffer, initial_buffer) {
                return Err(
                    proptest::test_runner::TestCaseError::reject("buffer was changed!")
                );
            }
        }

        // Redo with no redos left should be a no-op
        for _ in 0..3 {
            buffer.redo();
            assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
        }

        // undo with no undos left should be a no-op
        for _ in 0..3 {
            buffer.undo();
            assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
        }
    }
}


fn undo_redo_works_on_these_edits_and_index<TestEdits: Borrow<[TestEdit]>>(edits: TestEdits, index: usize) {
    let edits = edits.borrow();

    //TODO generate initial buffer?
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    let mut expected_buffer_at_index: Option<TextBuffer> = None;

    macro_rules! record_if_index_matches {
        // Things like moving cursors that don't exist are, and are expected to be, no-ops
        // that do not get added to the history. So the `edits` len may be different than the
        //  history len.
        () => (
            if buffer.history.len().checked_sub(1) == Some(index) {
                expected_buffer_at_index = Some(deep_clone(&buffer));
            }
        );
    }

    record_if_index_matches!();

    for edit in edits.iter() {
        apply_edit(&mut buffer, (*edit).clone());

        record_if_index_matches!();
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
            buffer.undo();
        }
    }

    assert_text_buffer_eq_ignoring_history!(buffer, expected_buffer_at_index);

    for _ in 0..len {
        buffer.redo();
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);

    // Redo with no redos left should be a no-op
    for _ in 0..3 {
        dbg!();
        buffer.redo();
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);

    for _ in 0..len {
        dbg!();
        dbg!(&mut buffer).undo();
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

    // undo with no undos left should be a no-op
    for _ in 0..3 {
        dbg!();
        buffer.undo();
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

proptest! {
    #[test]
    fn undo_redo_works((edits, index) in arb_test_edits_and_index(SOME_AMOUNT, ArbTestEditSpec::All)) {
        undo_redo_works_on_these_edits_and_index(edits, index);
    }

    #[test]
    fn undo_redo_works_on_inserts((edits, index) in arb_test_edits_and_index(SOME_AMOUNT, ArbTestEditSpec::Insert)) {
        undo_redo_works_on_these_edits_and_index(edits, index);
    }

    #[test]
    fn undo_redo_works_on_non_control_inserts((edits, index) in arb_test_edits_and_index(SOME_AMOUNT, ArbTestEditSpec::RegexInsert("\\PC"))) {
        undo_redo_works_on_these_edits_and_index(edits, index);
    }

    #[test]
    fn undo_redo_works_on_non_cr_inserts((edits, index) in arb_test_edits_and_index(SOME_AMOUNT, ArbTestEditSpec::RegexInsert("[^\r]"))) {
        undo_redo_works_on_these_edits_and_index(edits, index);
    }
}

#[test]
fn undo_redo_works_on_this_set_of_edits() {
    undo_redo_works_on_these_edits_and_index(
        vec![
            TestEdit::Insert('\u{b}'),
            TestEdit::Insert('a'),
            TestEdit::Insert('\n'),
        ],
        0,
    );
}

#[test]
fn undo_redo_works_in_this_reduced_scenario() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    apply_edit(&mut buffer, TestEdit::Insert('\u{b}'));

    let expected_final_buffer = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Insert('a'));

    let expected_mid_buffer = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Insert('\n'));

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, expected_mid_buffer);

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, expected_final_buffer);
}

#[test]
fn undo_redo_works_on_this_single_movement_edit() {
    undo_redo_works_on_these_edits_and_index(vec![TestEdit::MoveAllCursors(Move::Up)], 0);
}

#[test]
fn undo_redo_works_on_a_wall_hitting_movement_after_an_insert() {
    undo_redo_works_on_these_edits_and_index(
        vec![TestEdit::Insert('a'), TestEdit::MoveAllCursors(Move::Up)],
        0,
    );
}

#[test]
fn undo_redo_works_on_this_manually_invented_case() {
    undo_redo_works_on_these_edits_and_index(
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
fn undo_redo_works_on_this_previously_panicking_case() {
    undo_redo_works_on_these_edits_and_index(
        vec![
            TestEdit::Insert('a'),
            TestEdit::ExtendSelectionForAllCursors(Move::Left),
            TestEdit::Delete,
        ],
        2,
    );
}

#[test]
fn undo_redo_works_on_this_cr_lf_case() { // sigh
    undo_redo_works_on_these_edits_and_index(
        vec![
            TestEdit::Insert('\r'), TestEdit::Insert('\n')
        ],
        0,
    );
}

#[test]
fn undo_redo_works_on_this_line_end_case() {
    undo_redo_works_on_these_edits_and_index(
        vec![TestEdit::Insert('a'), TestEdit::MoveAllCursors(Move::ToLineEnd)],
        0,
    );
}

#[test]
fn undo_redo_works_in_this_familiar_scenario() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    apply_edit(&mut buffer, TestEdit::Insert('1'));

    let buffer_with_1 = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Insert('2'));

    let buffer_with_2 = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Insert('3'));

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_with_2);

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_with_1);

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn undo_redo_works_on_this_simple_insert_delete_case() {
    undo_redo_works_on_these_edits_and_index(
        vec![TestEdit::Insert('a'), TestEdit::Delete],
        0,
    );
}

#[test]
fn undo_redo_works_on_this_reduced_simple_insert_delete_case() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    let inserted_char = 'a';

    apply_edit(&mut buffer, TestEdit::Insert(inserted_char));

    let buffer_with_a = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Delete);

    dbg!();
    let delete_edit = buffer.history.get(buffer.history_index.checked_sub(1).unwrap()).unwrap();

    match &delete_edit.range_edits.first().delete_range {
        Some(r_e) => assert_eq!(r_e.chars, char_to_string(inserted_char)),
        _ => assert!(false),
    }

    dbg!();
    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_with_a);
    dbg!();
    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn undo_redo_works_on_this_move_to_line_start_case() {
    undo_redo_works_on_these_edits_and_index(
        vec![
            TestEdit::Insert('¡'),
            TestEdit::ExtendSelectionForAllCursors(Move::ToLineStart),
            TestEdit::Delete
        ],
        0,
    );
}

#[test]
fn undo_redo_works_on_this_reduced_move_to_line_start_case() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    apply_edit(&mut buffer, TestEdit::Insert('¡'));

    let buffer_after_1 = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::ExtendSelectionForAllCursors(Move::ToLineStart));

    let buffer_after_2 = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Delete);

    assert_eq!(buffer.rope.to_string(),  "");

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_after_2);

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_after_1);

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn undo_redo_works_on_this_case_involving_moving_a_missing_cursor() {
    use TestEdit::*;
    undo_redo_works_on_these_edits_and_index(
        [Insert('0'), MoveCursors(1, Move::Up)], 0
    );
}

#[test]
fn undo_redo_works_on_this_case_involving_two_characters_at_once() {
    undo_redo_works_on_these_edits_and_index(
        [InsertString!("¡A")], 0
    );
}

#[test]
fn undo_redo_works_on_this_reduced_case_involving_two_characters_at_once() {
    //TODO generate initial buffer?
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    apply_edit(&mut buffer, InsertString!("¡A"));

    dbg!(&mut buffer).undo();

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

    // undo with no undos left should be a no-op
    for _ in 0..3 {
        dbg!();
        buffer.undo();
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn undo_redo_works_on_this_case_involving_two_characters_at_once_then_a_newline_then_dragging() {
    undo_redo_works_on_these_edits_and_index(
        [InsertString!("Aa"), InsertString!("\n"), TestEdit::DragCursors(pos!{l 0 o 0})], 0
    );
}

#[test]
fn undo_redo_works_on_this_case_involving_a_select_bewtween_char_type_grouping_and_non_ascii_chars() {
    undo_redo_works_on_these_edits_and_index(
        [
            InsertString!("ࠀ\u{e000}㐀"),
            TestEdit::SelectCharTypeGrouping(pos!{l 0 o 0}, ReplaceOrAdd::Add),
            InsertString!("a¡")
        ],
        0
    );
}

#[test]
fn undo_redo_works_on_this_smaller_case_involving_a_select_bewtween_char_type_grouping_and_non_ascii_chars() {
    undo_redo_works_on_these_edits_and_index(
        [
            InsertString!("¡㐀"),
            TestEdit::SelectCharTypeGrouping(pos!{l 0 o 0}, ReplaceOrAdd::Add),
            InsertString!("a¡")
        ],
        0
    );
}

#[test]
fn undo_redo_works_on_this_reduced_case_involving_a_select_bewtween_char_type_grouping_and_non_ascii_chars() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    apply_edit(&mut buffer, InsertString!("¡㐀"));

    apply_edit(&mut buffer, TestEdit::SelectCharTypeGrouping(pos!{l 0 o 0}, ReplaceOrAdd::Add));

    let expected_buffer_after_second_edit = deep_clone(&buffer);

    apply_edit(&mut buffer, InsertString!("a¡"));

    dbg!(&mut buffer);

    buffer.undo();

    dbg!(&mut buffer);

    assert_text_buffer_eq_ignoring_history!(buffer, expected_buffer_after_second_edit);
}
