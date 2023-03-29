use super::{*, assert_eq, dbg};

fn on<TestEdits: Borrow<[TestEdit]>>(
    edits: TestEdits,
    index: usize,
) {
    let edits = edits.borrow();

    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    let len = edits.len();

    for edit in edits.iter() {
        TestEdit::apply(&mut buffer, (*edit).clone());
    }

    for _ in (index..len).rev() {
        buffer.undo(None);
    }

    TestEdit::apply(&mut buffer, TestEdit::Insert('6'));

    let final_buffer = deep_clone(&buffer);

    for _ in index..(len + 2) {
        buffer.redo(None);
        assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);
    }

    if len != 0 {
        for _ in 0..len {
            dbg!();
            dbg!(&mut buffer).undo(None);
        }

        assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
    }
}

proptest! {
    #[test]
    fn does_not_allow_applying_stale_redos((edits, index) in arb::test_edits_and_index(SOME_AMOUNT, TestEditSpec::All)) {
        on(edits, index);
    }
}

#[test]
fn in_this_generated_case() {
    use TestEdit::*;
    let edits = [
        InsertString("+\r\u{3e7ae}/�I\u{b}".to_string()),
        SetCursor(pos!{l 0 o 16}, ReplaceOrAdd::Add),
        StripTrailingWhitespace
    ];
    on(edits, 0);
}

#[test]
fn in_this_reduced_generated_case() {
    use TestEdit::*;
    let edits = [
        InsertString("+\r\u{3e7ae}/�I\u{b}".to_string()),
        SetCursor(pos!{l 0 o 1}, ReplaceOrAdd::Add),
        StripTrailingWhitespace
    ];
    on(edits, 0);
}

#[test]
fn in_this_reduced_generated_case_reduction() {
    use TestEdit::*;
    use ReplaceOrAdd::*;

    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, InsertString("+\r\u{3e7ae}/�I\u{b}".to_string()));
    {
        let mut expected = t_b!("+\r\u{3e7ae}/�I\u{b}");
        TestEdit::apply(&mut expected, SetCursor(pos!{l 2 o 0}, Replace));
        assert_text_buffer_eq_ignoring_history!(buffer, expected);
    }

    TestEdit::apply(&mut buffer, SetCursor(pos!{l 0 o 1}, ReplaceOrAdd::Add));
    TestEdit::apply(&mut buffer, StripTrailingWhitespace);
    {
        let mut expected = t_b!("+\r\u{3e7ae}/�I\u{b}");
        TestEdit::apply(&mut expected, SetCursor(pos!{l 2 o 0}, Replace));
        TestEdit::apply(&mut expected, SetCursor(pos!{l 0 o 1}, ReplaceOrAdd::Add));
        assert_text_buffer_eq_ignoring_history!(buffer, expected);
    }

    buffer.undo(None);
    buffer.undo(None);
    buffer.undo(None);

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn on_this_basic_auto_indent_case() {
    use TestEdit::*;
    let edits = [
        Insert('a'),
        AutoIndentSelection,
    ];
    on(edits, 1);
}