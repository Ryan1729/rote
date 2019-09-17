use super::*;
use crate::move_cursor::last_position;
use crate::tests::{
    arb::{TestEdit, TestEditSpec, *},
    deep_clone, SOME_AMOUNT, *,
};
use editor_types::{vec1, CursorState};

use proptest::prelude::*;
use proptest::{option, prop_compose, proptest};

#[test]
fn line_indicies_touched_by_counts_the_line_ahead_if_the_newline_is_included() {
    let rope = r!("0\n 1\n");

    let range = AbsoluteCharOffsetRange::new(d!(), AbsoluteCharOffset(2));

    let line_indicies = line_indicies_touched_by(&rope, range).unwrap();

    let expected = line_indicies_touched_by(
        &rope,
        AbsoluteCharOffsetRange::new(
            d!(),
            // Everything but the final `'\n'`.
            rope.len_chars() - 1,
        ),
    )
    .unwrap();

    assert_eq!(line_indicies, expected);
}

// Non-breaking space =
const NBSP: char = '\u{A0}';

#[test]
fn get_tab_in_edit_produces_the_expected_edit_from_this_buffer_with_different_leading_whitespace() {
    let text = format!("0\n 1\n  2\n   3\n    4\n\n{0}\n{0}1\n {0}2\n", NBSP);

    let mut buffer = t_b!(text.to_owned());
    buffer.select_all();

    let rope = &buffer.rope;

    let cursors = &buffer.cursors;

    let edit = get_tab_in_edit(rope, cursors);

    let expected = {
        let new_chars =
            "    0\n     1\n      2\n       3\n        4\n    \n     \n     1\n      2\n    "
                .to_owned();

        let new_rope = r!(new_chars);
        let mut cursor = Cursor::new(last_position(&new_rope).unwrap());
        cursor.set_highlight_position(Position {
            offset: CharOffset(TAB_STR_CHAR_COUNT),
            ..d!()
        });

        let insert_range = Some(RangeEdit {
            range: AbsoluteCharOffsetRange::new(
                d!(),
                AbsoluteCharOffset(new_chars.chars().count()),
            ),
            chars: new_chars,
        });

        let delete_range = Some(RangeEdit {
            range: AbsoluteCharOffsetRange::new(d!(), AbsoluteCharOffset(text.chars().count())),
            chars: text,
        });

        Edit {
            range_edits: Vec1::new(RangeEdits {
                insert_range,
                delete_range,
            }),
            cursors: Change {
                new: Cursors::new(Vec1::new(cursor)),
                old: cursors.clone(),
            },
        }
    };

    assert_eq!(edit, expected);
}

#[test]
fn get_tab_in_edit_produces_the_expected_edit_with_multiple_cursors_in_this_buffer_with_different_leading_whitespace(
) {
    let text = format!("0\n 1\n  2\n   3\n    4\n\n{0}\n{0}1\n {0}2\n", NBSP);
    let start_of_empty_line = pos! {l 5 o 0};

    let mut buffer = t_b!(text.to_owned());
    buffer.set_cursor(start_of_empty_line, ReplaceOrAdd::Add);
    const RIGHT_COUNT: usize = 19;
    for _ in 0..RIGHT_COUNT {
        buffer.extend_selection_for_all_cursors(Move::Right);
    }

    //pre-condition
    assert_eq!(
        buffer.cursors,
        Cursors::new(vec1![
            {
                let mut c = Cursor::new_with_highlight(pos! {l 9 o 0}, start_of_empty_line);
                c.state = CursorState::PressedAgainstWall;
                c
            },
            Cursor::new_with_highlight(pos! {l 4 o 5}, pos! {l 0 o 0})
        ])
    );

    let rope = &buffer.rope;

    let cursors = &buffer.cursors;

    let edit = get_tab_in_edit(rope, cursors);

    let expected = {
        // Many things here rely on the example text being ASCII.
        const EXPECTED_TEXT: &'static str =
            "    0\n     1\n      2\n       3\n        4\n    \n     \n     1\n      2\n    ";
        const EXPECTED_CLEAVE_POINT: usize = RIGHT_COUNT + TAB_STR_CHAR_COUNT * 5;

        let new_cursors = {
            let expected_rope = r!(EXPECTED_TEXT.to_owned());
            let mut first_cursor = Cursor::new(pos! {l 0, o TAB_STR_CHAR_COUNT});
            let mut last_cursor = Cursor::new(start_of_empty_line);
            move_cursor::right_n_times(&expected_rope, &mut last_cursor, TAB_STR_CHAR_COUNT);

            for _ in 0..EXPECTED_CLEAVE_POINT - TAB_STR_CHAR_COUNT {
                move_cursor::and_extend_selection(&expected_rope, &mut first_cursor, Move::Right);
                // we expect te last cursor to hit the end.
                move_cursor::and_extend_selection(&expected_rope, &mut last_cursor, Move::Right);
            }

            Cursors::new(dbg!(vec1![last_cursor.clone(), first_cursor.clone()]))
        };
        dbg!(&new_cursors);
        // precondition
        assert_eq!(new_cursors.len(), 2);

        let first_range_edits = {
            let new_chars = (&EXPECTED_TEXT[..EXPECTED_CLEAVE_POINT]).to_owned();

            let insert_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(
                    d!(),
                    AbsoluteCharOffset(EXPECTED_CLEAVE_POINT),
                ),
                chars: new_chars,
            });

            let delete_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(d!(), AbsoluteCharOffset(RIGHT_COUNT)),
                chars: (&text[..RIGHT_COUNT]).to_owned(),
            });
            RangeEdits {
                insert_range,
                delete_range,
            }
        };

        let last_range_edits = {
            const START_OF_SECOND_HIGHLIGHT: usize = RIGHT_COUNT + 1;
            let chars = (&EXPECTED_TEXT[EXPECTED_CLEAVE_POINT + 1..]).to_owned();
            let insert_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(
                    AbsoluteCharOffset(START_OF_SECOND_HIGHLIGHT),
                    AbsoluteCharOffset(START_OF_SECOND_HIGHLIGHT + chars.chars().count()),
                ),
                chars,
            });

            let delete_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(
                    AbsoluteCharOffset(START_OF_SECOND_HIGHLIGHT),
                    AbsoluteCharOffset(text.chars().count()),
                ),
                chars: (&text[START_OF_SECOND_HIGHLIGHT..]).to_owned(),
            });
            RangeEdits {
                insert_range,
                delete_range,
            }
        };

        Edit {
            range_edits: vec1![last_range_edits, first_range_edits],
            cursors: Change {
                new: new_cursors,
                old: cursors.clone(),
            },
        }
    };

    assert_eq!(edit, expected);
}

#[test]
fn get_tab_in_edit_produces_the_expected_change_when_two_cursors_are_on_the_same_line() {
    let text = format!("0\n  2\n");

    let mut buffer = t_b!(text.to_owned());
    buffer.set_cursor(pos! {l 1 o 2}, ReplaceOrAdd::Add);
    const RIGHT_COUNT: usize = 3;
    for _ in 0..RIGHT_COUNT {
        buffer.extend_selection_for_all_cursors(Move::Right);
    }

    let edit = get_tab_in_edit(&buffer.rope, &buffer.cursors);

    dbg!(&edit);

    buffer.apply_edit(edit, ApplyKind::Playback);

    let s: String = buffer.rope.into();
    //2 + (2 * 4) = 10 ___1234567890
    assert_eq!(s, "    0\n          2\n    ");
}

#[test]
fn get_tab_in_edit_produces_the_expected_change_when_three_cursors_are_on_the_same_line() {
    let text = format!("0\n       7\n");

    let mut buffer = t_b!(text.to_owned());
    buffer.set_cursor(pos! {l 1 o 2}, ReplaceOrAdd::Add);
    buffer.set_cursor(pos! {l 1 o 6}, ReplaceOrAdd::Add);
    const RIGHT_COUNT: usize = 3;
    for _ in 0..RIGHT_COUNT {
        buffer.extend_selection_for_all_cursors(Move::Right);
    }

    let edit = get_tab_in_edit(&buffer.rope, &buffer.cursors);

    dbg!(&edit);

    buffer.apply_edit(edit, ApplyKind::Playback);

    let s: String = buffer.rope.into();
    //7 + (3 * 4) = 19 ___1234567890123456789
    assert_eq!(s, "    0\n                   7\n    ");
}

#[test]
fn get_first_non_white_space_offset_in_range_works_on_these_examples() {
    let rope = r!("\n \n 1\n  \n  2\n");

    let mut lines = rope.lines();

    let empty_line = lines.next().unwrap();
    assert_eq!(
        get_first_non_white_space_offset_in_range(
            empty_line,
            CharOffset(0)..=CharOffset(usize::max_value())
        ),
        None
    );

    let one_space_line = lines.next().unwrap();
    assert_eq!(
        get_first_non_white_space_offset_in_range(
            one_space_line,
            CharOffset(0)..=CharOffset(usize::max_value())
        ),
        None
    );

    let one_space_then_non_whitespace_line = lines.next().unwrap();
    assert_eq!(
        get_first_non_white_space_offset_in_range(
            one_space_then_non_whitespace_line,
            CharOffset(0)..CharOffset(1)
        ),
        None
    );
    assert_eq!(
        get_first_non_white_space_offset_in_range(
            one_space_then_non_whitespace_line,
            CharOffset(0)..=CharOffset(usize::max_value())
        ),
        Some(CharOffset(1))
    );

    let two_spaces_line = lines.next().unwrap();
    assert_eq!(
        get_first_non_white_space_offset_in_range(two_spaces_line, CharOffset(0)..CharOffset(1)),
        None
    );
    assert_eq!(
        get_first_non_white_space_offset_in_range(
            two_spaces_line,
            CharOffset(0)..=CharOffset(usize::max_value())
        ),
        None
    );

    let two_spaces_then_non_whitespace_line = lines.next().unwrap();
    assert_eq!(
        get_first_non_white_space_offset_in_range(
            two_spaces_then_non_whitespace_line,
            CharOffset(0)..CharOffset(1)
        ),
        None
    );
    assert_eq!(
        get_first_non_white_space_offset_in_range(
            two_spaces_then_non_whitespace_line,
            CharOffset(0)..CharOffset(2)
        ),
        None
    );
    assert_eq!(
        get_first_non_white_space_offset_in_range(
            two_spaces_then_non_whitespace_line,
            CharOffset(1)..CharOffset(2)
        ),
        None
    );
    assert_eq!(
        get_first_non_white_space_offset_in_range(
            two_spaces_then_non_whitespace_line,
            CharOffset(0)..=CharOffset(usize::max_value())
        ),
        Some(CharOffset(2))
    );
    assert_eq!(
        get_first_non_white_space_offset_in_range(
            two_spaces_then_non_whitespace_line,
            CharOffset(1)..=CharOffset(usize::max_value())
        ),
        Some(CharOffset(2))
    );
}

proptest! {
    #[test]
    fn tab_in_preserves_line_count(
        mut buffer in arb::text_buffer_with_many_cursors(),
    ) {
        let line_count = buffer.rope.len_lines();

        for i in 0..SOME_AMOUNT {
            TestEdit::apply(&mut buffer, TestEdit::TabIn);

            assert_eq!(line_count, buffer.rope.len_lines(), "iteration {}", i);
        }
    }
}

fn tab_out_preserves_line_count_on(mut buffer: TextBuffer) {
    let line_count = buffer.rope.len_lines();

    for i in 0..SOME_AMOUNT {
        TestEdit::apply(&mut buffer, TestEdit::TabOut);

        assert_eq!(line_count, buffer.rope.len_lines(), "iteration {}", i);
    }
}

proptest! {
    #[test]
    fn tab_out_preserves_line_count(
        buffer in arb::text_buffer_with_many_cursors(),
    ) {
        tab_out_preserves_line_count_on(buffer);
    }
}

#[test]
fn tab_out_preserves_line_count_on_this_generated_example() {
    let mut buffer = t_b!("�<AGL");

    buffer.cursors = Cursors::new(vec1![
        Cursor::new_with_highlight(pos! {l 1 o 4}, pos! {l 2 o 0}),
        Cursor::new_with_highlight(pos! {l 1 o 3}, pos! {l 0 o 0})
    ]);

    tab_out_preserves_line_count_on(buffer);
}

proptest! {
    #[test]
    fn tab_in_then_tab_out_is_identity(
        initial_buffer in arb::text_buffer_with_many_cursors(),
    ) {
        let mut buffer = deep_clone(&initial_buffer);

        TestEdit::apply(&mut buffer, TestEdit::TabIn);
        TestEdit::apply(&mut buffer, TestEdit::TabOut);

        assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
    }
}

proptest! {
    #[test]
    fn tab_out_then_tab_in_is_identity(
        initial_buffer in arb::text_buffer_with_many_cursors(),
    ) {
        let mut buffer = deep_clone(&initial_buffer);

        TestEdit::apply(&mut buffer, TestEdit::TabOut);
        TestEdit::apply(&mut buffer, TestEdit::TabIn);

        assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
    }
}

proptest! {
    #[test]
    fn tab_in_preserves_non_white_space(
        mut buffer in arb::text_buffer_with_many_cursors(),
    ) {
        let expected: String = buffer.rope.chars().filter(|c| !c.is_whitespace()).collect();

        for i in 0..SOME_AMOUNT {
            TestEdit::apply(&mut buffer, TestEdit::TabIn);

            let actual: String = buffer.rope.chars().filter(|c| !c.is_whitespace()).collect();

            assert_eq!(actual, expected, "iteration {}", i);
        }
    }
}

fn tab_out_preserves_non_white_space_on(mut buffer: TextBuffer) {
    let expected: String = buffer.rope.chars().filter(|c| !c.is_whitespace()).collect();

    for i in 0..SOME_AMOUNT {
        TestEdit::apply(&mut buffer, TestEdit::TabOut);

        let actual: String = buffer.rope.chars().filter(|c| !c.is_whitespace()).collect();

        assert_eq!(actual, expected, "iteration {}", i);
    }
}

proptest! {
    #[test]
    fn tab_out_preserves_non_white_space(
        buffer in arb::text_buffer_with_many_cursors(),
    ) {
        tab_out_preserves_non_white_space_on(buffer);
    }
}

#[test]
fn tab_out_preserves_non_white_space_on_this_generated_example() {
    let mut buffer = t_b!(" 2b");

    buffer.cursors = Cursors::new(vec1![
        Cursor::new(pos! {l 2 o 0}),
        Cursor::new_with_highlight(pos! {l 1 o 24}, pos! {l 0 o 1})
    ]);
    tab_out_preserves_non_white_space_on(buffer);
}

mod edit_arb;
mod undo_redo;
