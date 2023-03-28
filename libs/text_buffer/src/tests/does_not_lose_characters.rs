use super::{*, dbg};


use arb::{Counts, TestEdit, get_counts};
use proptest::proptest;
#[cfg(feature = "do_proptests")]
use arb::{TestEditSpec, SOME_AMOUNT}; 

use text_buffer_testing::counts_assert;

fn on<TestEdits: Borrow<[TestEdit]>>(
    initial_buffer: TextBuffer,
    edits: TestEdits,
) {
    let mut counts = get_counts(&initial_buffer);
    let mut buffer = deep_clone(&initial_buffer);

    for edit in edits.borrow().iter() {
        TestEdit::apply_with_counts(&mut buffer, &mut counts, edit);
    }

    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}

proptest! {
    #[test]
    fn test_spec_all((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::All)) {
        on(buffer, edits);
    }

    #[test]
    fn inserts((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::Insert)) {
        on(buffer, edits);
    }

    #[test]
    fn non_control_inserts((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::RegexInsert("\\PC"))) {
        on(buffer, edits);
    }

    #[test]
    fn non_cr_inserts((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::RegexInsert("[^\r]"))) {
        on(buffer, edits);
    }

    #[test]
    fn set_cursor_heavy((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::SetCursorHeavy)) {
        on(buffer, edits);
    }

    #[test]
    fn tab_in_out_heavy((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::TabInOutHeavy)) {
        on(buffer, edits);
    }

    #[test]
    fn delete_and_tab_in_out_heavy((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::DeleteAndTabInOutHeavy)) {
        on(buffer, edits);
    }

    #[test]
    fn delete_then_tab_out(buffer in arb::text_buffer_with_valid_cursors(), edits in arb::test_edit_delete_then_tab_out_vec()) {
        on(buffer, edits);
    }
}

#[test]
fn in_this_generated_case() {
    use TestEdit::*;
    on(
        t_b!(""),
        [InsertString("\u{b}".to_string()), DragCursors(pos!{l 0 o 0}), TabIn]
    );
}

#[test]
fn in_this_reduced_generated_case() {
    use TestEdit::*;

    let mut buffer = t_b!("");
    let mut counts = get_counts(&buffer);


    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &InsertString("\n".to_string()));
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &DragCursors(pos!{l 0 o 0}));
    dbg!(&buffer);
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(get_counts(&buffer), &counts);

    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}

#[test]
fn in_this_tab_in_tab_in_tab_out_case() {
    use TestEdit::*;
    on(
        t_b!(""),
        [TabIn, TabIn, TabOut]
    );
}

#[test]
fn in_this_extend_selection_case() {
    use TestEdit::*;
    on(
        t_b!(""),
        [
            InsertString("\u{b}\t".to_owned()),
            ExtendSelectionForAllCursors(Move::ToBufferStart),
            TabIn
        ]
    );
}

#[test]
fn in_this_reduced_extend_selection_case() {
    use TestEdit::*;

    let mut buffer = t_b!("");
    let mut counts = get_counts(&buffer);


    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &InsertString("\u{b}\t".to_owned()));
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &ExtendSelectionForAllCursors(Move::ToBufferStart));
    dbg!(&buffer);
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(get_counts(&buffer), &counts);

    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}

#[test]
fn in_this_further_reduced_extend_selection_case() {
    use TestEdit::*;

    let mut buffer = t_b!("");
    let mut counts = get_counts(&buffer);


    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &InsertString("\u{b}\t".to_owned()));
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &ExtendSelectionForAllCursors(Move::ToBufferStart));
    dbg!(&buffer);
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(get_counts(&buffer), &counts);

    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}

#[test]
fn in_this_delete_lines_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\u{2028}ૠ�🌀");

    buffer.set_cursor(cur!{l 0 o 0 h l 1 o 0}, Replace);
    on(
        buffer,
        [DeleteLines]
    );
}

#[test]
fn in_this_reduced_delete_lines_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\na");

    buffer.set_cursor(cur!{l 0 o 0 h l 1 o 0}, Replace);
    on(
        buffer,
        [DeleteLines]
    );
}

#[test]
fn in_this_single_newline_delete_lines_case() {
    use TestEdit::*;

    on(
        t_b!("\n"),
        [DeleteLines]
    );
}

#[test]
fn in_this_more_complicated_delete_lines_case() {
    use TestEdit::*;

    on(
        t_b!("a"),
        [
            Insert('\n'),
            InsertString("\\\u{b}\u{b}\\".to_string()),
            SetCursor(pos!{l 3 o 2}, ReplaceOrAdd::Add),
            DeleteLines
        ]
    );
}

#[test]
fn in_this_reduced_more_complicated_delete_lines_case() {
    use TestEdit::*;

    let mut buffer = t_b!("\n\\\u{b}\u{b}\\a", vec1![
        cur!{l 3 o 2},
        cur!{l 3 o 1}
    ]);
    let mut counts = get_counts(&buffer);

    dbg!(get_counts(&buffer), &counts);
    dbg!(&buffer);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &DeleteLines);
    dbg!(get_counts(&buffer), &counts);
    dbg!(&buffer);

    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}

#[test]
fn in_this_further_reduced_more_complicated_delete_lines_case() {
    use TestEdit::*;

    let mut buffer = t_b!("\n\\\n\n\\a", vec1![
        cur!{l 3 o 2},
        cur!{l 3 o 1}
    ]);
    let mut counts = get_counts(&buffer);

    dbg!(get_counts(&buffer), &counts);
    dbg!(&buffer);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &DeleteLines);
    dbg!(get_counts(&buffer), &counts);
    dbg!(&buffer);

    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}

#[test]
fn in_minimal_tab_in_case() {
    use TestEdit::*;
    on(
        t_b!(""),
        [TabIn]
    );
}

#[test]
fn in_this_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\u{a0}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);
    on(
        buffer,
        [TabIn]
    );
}

#[test]
fn in_this_reduced_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\u{a0}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);
    let mut counts = get_counts(&buffer);

    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);

    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}


#[test]
fn in_this_delete_then_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    use Move::*;
    let mut buffer = t_b!("\u{a0}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);
    on(
        buffer,
        [
            SetCursor(pos!{l 0 o 0}, Add),
            ExtendSelectionForAllCursors(Up),
            ExtendSelectionForAllCursors(Left),
            Delete,
            SetCursor(pos!{l 0 o 2}, Add),
            ExtendSelectionForAllCursors(Up),
            ExtendSelectionForAllCursors(Left),
            TabIn
        ]
    );
}

#[test]
fn in_this_two_space_then_zero_delete_then_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    use Move::*;
    let buffer = t_b!("  0");

    on(
        buffer,
        [
            SetCursor(pos!{l 0 o 0}, Add),
            ExtendSelectionForAllCursors(Up),
            ExtendSelectionForAllCursors(Left),
            Delete,
            SetCursor(pos!{l 0 o 2}, Add),
            ExtendSelectionForAllCursors(Up),
            ExtendSelectionForAllCursors(Left),
            TabIn
        ]
    );
}

#[test]
fn in_this_reduced_two_space_then_zero_delete_then_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    use Move::*;
    let initial_buffer = t_b!("  0");

    let mut counts = get_counts(&initial_buffer);
    let mut buffer = deep_clone(&initial_buffer);

    let edits = [
            SetCursor(pos!{l 0 o 0}, Add),
            SetCursor(pos!{l 0 o 2}, Add),
            ExtendSelectionForAllCursors(Left),
            TabIn
        ];

    for edit in edits.iter() {
        TestEdit::apply_with_counts(&mut buffer, &mut counts, edit);
    }

    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}

#[test]
fn in_this_duplicate_lines_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    on(
        t_b!(" "),
        [SetCursor(pos!{l 0 o 1}, Add), DuplicateLines]
    );
}

#[test]
fn in_this_duplicate_lines_vertical_tab_case() {
    use TestEdit::*;
    use Move::*;
    on(
        t_b!(""),
        [InsertString("\u{b}\u{b}".to_string()), MoveAllCursors(ToBufferEnd), Delete, DuplicateLines]
    );
}

#[test]
fn in_this_toggle_case_case() {
    use TestEdit::*;
    on(
        t_b!("abc"),
        [ToggleCase]
    );
}

#[test]
fn in_this_toggle_case_tab_out_case() {
    use TestEdit::*;
    let mut buffer = t_b!("abc");
    buffer.select_all();

    on(
        buffer,
        [ToggleCase, TabOut]
    );
}

#[test]
fn in_this_unicode_toggle_case_tab_out_case() {
    use TestEdit::*;
    let mut buffer = t_b!("a\u{119da}");
    buffer.select_all();

    on(
        buffer,
        [ToggleCase, TabOut]
    );
}

#[test]
fn in_this_unicode_toggle_case_case() {
    use TestEdit::*;
    let mut buffer = t_b!("a\u{119da}");
    buffer.select_all();

    {
        // TODO move to separate test, or delete.
        let mut buffer = buffer.clone();
        let mut counts = get_counts(&buffer);
        TestEdit::apply_with_counts(&mut buffer, &mut counts, &ToggleCase);
        std::dbg!(&counts);
        assert_eq!(buffer.borrow_rope(), Rope::from("A\u{119da}"));
    }

    on(
        buffer,
        [ToggleCase]
    );
}

#[test]
fn in_this_two_wide_selection_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("!\u{2000}");

    let mut cursor = cur!{l 0 o 2 h l 0 o 0};
    cursor.sticky_offset = d!();
    buffer.set_cursor(cursor, Replace);

    let mut counts = get_counts(&buffer);

    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(&counts);

    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}

#[test]
fn in_this_two_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("A");

    buffer.set_cursor(cur!{l 0 o 0 h l 0 o 1}, Replace);

    let mut counts = get_counts(&buffer);

    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &SetCursor(pos!{l 0 o 0}, Add));
    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(&counts);
    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}

#[test]
fn in_this_no_insert_auto_indent_case() {
    use TestEdit::*;
    on(
        t_b!("123"),
        [AutoIndentSelection]
    );
}


#[test]
fn in_this_basic_auto_indent_case() {
    use TestEdit::*;
    on(
        t_b!(""),
        [Insert('a'), AutoIndentSelection]
    );
}

#[test]
fn in_this_basic_auto_indent_case_reduction() {
    use TestEdit::*;
    let initial_buffer = t_b!("");
    let mut counts = get_counts(&initial_buffer);
    let mut buffer = deep_clone(&initial_buffer);

    TestEdit::apply_with_counts(&mut buffer, &mut counts, &Insert('a'));
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &AutoIndentSelection);

    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}

#[test]
fn in_this_set_cursor_heavy_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("a ");
    buffer.set_cursor(cur!{l 0 o 0 h l 0 o 1}, Replace);
    on(
        buffer,
        [SetCursor(pos!{l 0 o 2}, Add), Delete]
    );
}

#[test]
fn in_this_tab_out_then_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("!\u{2000}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);

    let mut counts = get_counts(&buffer);

    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabOut);
    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(&counts);
    counts.retain(|_, v| *v != 0);

    counts_assert!(buffer, counts);
}