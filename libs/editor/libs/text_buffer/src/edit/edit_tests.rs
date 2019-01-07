use super::*;
use crate::move_cursor::last_position;
use crate::tests::{
    arb::{TestEdit, TestEditSpec, *},
    deep_clone, SOME_AMOUNT, *,
};
use editor_types::{cur, vec1, Cursor};
use platform_types::CursorState;

use arb::get_counts;

use pretty_assertions::assert_eq;
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
                new: Cursors::new(&new_rope, Vec1::new(cursor)),
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
        Cursors::new(
            &buffer.rope,
            vec1![
                cur! {
                    l 9 o 0 h start_of_empty_line, ->|(Move::Right)
                },
                cur!(l 4 o 5 h l 0 o 0)
            ]
        )
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
            for _ in 0..TAB_STR_CHAR_COUNT {
                move_cursor::directly(&expected_rope, &mut last_cursor, Move::Right);
            }

            for _ in 0..EXPECTED_CLEAVE_POINT - TAB_STR_CHAR_COUNT {
                move_cursor::and_extend_selection(&expected_rope, &mut first_cursor, Move::Right);
                // we expect the last cursor to hit the end here.
                move_cursor::and_extend_selection(&expected_rope, &mut last_cursor, Move::Right);
            }
            // but we don't actually want to require that
            last_cursor.state = d!();

            Cursors::new(
                &expected_rope,
                vec1![last_cursor.clone(), first_cursor.clone()],
            )
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

fn tab_in_preserves_line_count_on(mut buffer: TextBuffer) {
    let line_count = buffer.rope.len_lines();

    for i in 0..SOME_AMOUNT {
        TestEdit::apply(&mut buffer, TestEdit::TabIn);

        assert_eq!(line_count, buffer.rope.len_lines(), "iteration {}", i);
    }
}

proptest! {
    #[test]
    fn tab_in_preserves_line_count(
        buffer in arb::text_buffer_with_many_cursors(),
    ) {
        tab_in_preserves_line_count_on(buffer);
    }
}

#[test]
fn tab_in_preserves_line_count_on_the_empty_rope() {
    tab_in_preserves_line_count_on(t_b!(""));
}

fn tab_out_preserves_line_count_on(mut buffer: TextBuffer) {
    let line_count = buffer.rope.len_lines();

    for i in 0..SOME_AMOUNT {
        TestEdit::apply(&mut buffer, TestEdit::TabOut);

        assert_eq!(
            line_count,
            buffer.rope.len_lines(),
            "iteration {}, rope: {:?}",
            i,
            buffer.rope
        );
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
fn tab_out_preserves_line_count_on_the_empty_rope() {
    tab_out_preserves_line_count_on(t_b!(""));
}

#[test]
fn tab_out_preserves_line_count_on_this_generated_example() {
    let mut buffer = t_b!("�<AGL");

    buffer.set_cursors_from_vec1(vec1![
        Cursor::new_with_highlight(pos! {l 1 o 4}, pos! {l 2 o 0}),
        Cursor::new_with_highlight(pos! {l 1 o 3}, pos! {l 0 o 0})
    ]);

    tab_out_preserves_line_count_on(buffer);
}

#[test]
fn tab_out_preserves_line_count_on_this_shorter_generated_example() {
    let mut buffer = t_b!("\u{2028}");
    buffer.set_cursors_from_vec1(vec1![Cursor::new_with_highlight(
        pos! {l 0 o 0},
        pos! {l 5 o 10}
    )]);

    tab_out_preserves_line_count_on(buffer);
}

#[test]
fn tab_out_preserves_line_count_on_this_reduced_example() {
    let mut buffer = t_b!("\n");
    buffer.set_cursors_from_vec1(vec1![Cursor::new_with_highlight(
        pos! {l 0 o 0},
        pos! {l 3 o 0}
    )]);

    tab_out_preserves_line_count_on(buffer);
}

fn tab_in_then_tab_out_is_identity_on_regarding_ropes(initial_buffer: TextBuffer) {
    let mut buffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, TestEdit::TabIn);
    dbg!(&buffer);
    TestEdit::apply(&mut buffer, TestEdit::TabOut);

    assert_eq!(buffer.rope, initial_buffer.rope);
}

proptest! {
    #[test]
    fn tab_in_then_tab_out_is_identity_on_all_space_buffers_regarding_ropes(
        initial_buffer in arb::all_space_text_buffer_with_many_cursors(),
    ) {
        tab_in_then_tab_out_is_identity_on_regarding_ropes(initial_buffer);
    }
}

#[test]
fn tab_in_then_tab_out_is_identity_on_a_single_newline_regarding_ropes() {
    tab_in_then_tab_out_is_identity_on_regarding_ropes(t_b!("\n"));
}

#[test]
fn tab_in_then_tab_out_is_identity_on_this_generated_example_regarding_ropes() {
    let mut buffer = t_b!("\n   \n\n");
    buffer.set_cursors_from_vec1(vec1![cur! {l 3 o 0}, cur! {l 2 o 0 h l 0 o 0}]);
    tab_in_then_tab_out_is_identity_on_regarding_ropes(buffer);
}

#[test]
fn tab_in_acts_as_expected_on_this_example_based_on_the_above_generated_test() {
    let mut buffer = t_b!("\n   \n\n");
    buffer.set_cursors_from_vec1(vec1![cur! {l 3 o 0}, cur! {l 2 o 0 h l 0 o 0}]);

    TestEdit::apply(&mut buffer, TestEdit::TabIn);

    assert_eq!(&buffer.rope.to_string(), "    \n       \n    \n    ");
    assert_eq!(
        buffer.cursors,
        Cursors::new(
            &buffer.rope,
            vec1![cur! {l 3 o 4}, cur! {l 2 o 4 h l 0 o 4}]
        )
    );
}

#[test]
fn tab_out_acts_as_expected_on_this_example_based_on_the_above_generated_test() {
    let mut buffer = t_b!("    \n       \n    \n    ");
    // These cursors are close enough, but not exactly what we would expect from the above.
    buffer.set_cursors_from_vec1(vec1![cur! {l 3 o 4}, cur! {l 2 o 4 h l 0 o 4}]);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);

    assert_eq!(&buffer.rope.to_string(), "\n   \n\n");
}

fn get_2_spaces_then_a_single_newline_and_particular_cursors() -> TextBuffer {
    let mut buffer = t_b!("  \n");
    // The reson these cursors are interesting is that they result in an offset pair of
    // `(None, Some(...))` after the first edit. This is not a desirable state, so we may want to
    // just make that state impossible.
    buffer.set_cursors_from_vec1(vec1![
        Cursor::new_with_highlight(pos! {l 6 o 3}, pos! {l 0 o 4}),
        Cursor::new_with_highlight(pos! {l 0 o 3}, pos! {l 0 o 0})
    ]);
    buffer
}

#[test]
fn tab_in_then_tab_out_is_identity_on_2_spaces_then_a_single_newline_and_particular_cursors_regarding_ropes(
) {
    tab_in_then_tab_out_is_identity_on_regarding_ropes(
        get_2_spaces_then_a_single_newline_and_particular_cursors(),
    );
}

#[test]
fn tab_in_is_as_expected_on_2_spaces_then_a_single_newline_and_particular_cursors_regarding_ropes()
{
    let mut buffer = get_2_spaces_then_a_single_newline_and_particular_cursors();

    TestEdit::apply(&mut buffer, TestEdit::TabIn);
    // I'm not positive this is actually what we should expect given the cursors are both starting
    // on the first line
    assert_eq!(buffer.rope, r!("      \n    "));
}

#[test]
fn tab_out_results_in_2_spaces_then_a_single_newline_in_this_case() {
    let mut buffer = get_2_spaces_then_a_single_newline_and_particular_cursors();
    TestEdit::apply(&mut buffer, TestEdit::TabIn);

    dbg!(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);

    assert_eq!(buffer.rope, r!("  \n"));
}

#[test]
fn tab_out_then_tab_in_is_as_expected_on_this_example() {
    //                     three spaces    vvv
    let initial_buffer: TextBuffer = t_b!("   ");
    //                     four spaces      vvvv
    let expected_buffer: TextBuffer = t_b!("    ");
    let mut buffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);
    TestEdit::apply(&mut buffer, TestEdit::TabIn);

    assert_eq!(buffer.rope, expected_buffer.rope);
}

#[test]
fn tab_out_then_tab_in_is_as_expected_on_this_smaller_example() {
    //                     three spaces    vvv
    let initial_buffer: TextBuffer = t_b!("   ");
    let mut buffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);
    let middle_buffer = deep_clone(&buffer);

    assert_text_buffer_eq_ignoring_history!(middle_buffer, t_b!(""));
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
    buffer.set_cursors_from_vec1(vec1![
        Cursor::new(pos! {l 2 o 0}),
        Cursor::new_with_highlight(pos! {l 1 o 24}, pos! {l 0 o 1})
    ]);

    tab_out_preserves_non_white_space_on(buffer);
}

#[test]
fn tab_out_preserves_non_white_space_on_this_reduced_example() {
    let mut buffer = t_b!(" 2b");
    buffer.set_cursors_from_vec1(vec1![Cursor::new_with_highlight(
        pos! {l 1 o 1},
        pos! {l 0 o 1}
    )]);

    tab_out_preserves_non_white_space_on(buffer);
}

#[test]
fn tab_out_preserves_non_white_space_on_this_reduced_in_a_different_way_example() {
    let mut buffer = t_b!(" 2b");
    buffer.set_cursors_from_vec1(vec1![Cursor::new_with_highlight(
        pos! {l 1 o 0},
        pos! {l 0 o 1}
    )]);

    tab_out_preserves_non_white_space_on(buffer);
}

fn get_code_like_example() -> TextBuffer {
    // like this:
    /*
    {
        {
            A
        }
    }
    */
    let mut buffer = t_b!("{\n    {\n        A\n    }\n}\n");
    buffer.set_cursors_from_vec1(vec1![Cursor::new_with_highlight(
        pos! {l 1 o 0},
        pos! {l 3 o 5}
    )]);
    buffer
}

#[test]
fn tab_out_produces_the_expected_string_on_this_code_like_example() {
    let mut buffer = get_code_like_example();

    TestEdit::apply(&mut buffer, TestEdit::TabOut);
    assert_eq!(buffer.rope, r!("{\n{\n    A\n}\n}\n"));
}

#[test]
fn tab_out_places_the_cursors_correctly_on_this_code_like_example() {
    let mut buffer = get_code_like_example();

    TestEdit::apply(&mut buffer, TestEdit::TabOut);
    assert_eq!(buffer.cursors, curs!(buffer.rope, cur!(l 1 o 0 h l 3 o 1)));
}

#[test]
fn tab_in_places_the_cursors_correctly_on_this_edge_case_example() {
    let mut buffer = t_b!("{\n    A\n}\n");
    buffer.set_cursors_from_vec1(vec1![cur!(l 1 o 3 h l 2 o 1 ),]);

    TestEdit::apply(&mut buffer, TestEdit::TabIn);
    assert_eq!(buffer.cursors, curs!(buffer.rope, cur!(l 1 o 7 h l 2 o 5 )));
}

// several of the next tests were based on a generated test about the effect of n inserts and then n deletes
#[test]
fn get_insert_edit_produces_the_expected_edit_on_this_cr_lf_edit_example() {
    let mut buffer = t_b!("\rA");
    buffer.set_cursors_from_vec1(vec1![Cursor::new(pos! {l 1 o 0})]);
    let rope = &buffer.rope;

    let cursors = &buffer.cursors;

    // Act
    let edit = get_insert_edit(rope, cursors, |_| "\n".to_owned());

    // Assert
    let expected = {
        let new_cursors = {
            let expected_rope = r!("\r\nA".to_owned());

            Cursors::new(&expected_rope, vec1![cur! {l 1 o 0}])
        };

        let range_edits = {
            let insert_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(AbsoluteCharOffset(1), AbsoluteCharOffset(2)),
                chars: "\n".to_owned(),
            });

            let delete_range = None;
            RangeEdits {
                insert_range,
                delete_range,
            }
        };

        Edit {
            range_edits: vec1![range_edits],
            cursors: Change {
                new: new_cursors,
                old: cursors.clone(),
            },
        }
    };

    assert_eq!(edit, expected);
}

// this was based on a generated test about the effect of n inserts and then n deletes
#[test]
fn get_delete_edit_produces_the_expected_edit_on_this_cr_lf_edit_example() {
    // Arrange
    let mut buffer = t_b!("A");

    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('\r'));
    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('\n'));

    let rope = &buffer.rope;

    let cursors = &buffer.cursors;

    // Act
    let edit = get_delete_edit(rope, cursors);

    // Assert
    let expected = {
        let new_cursors = {
            let expected_rope = r!("\rA".to_owned());

            Cursors::new(&expected_rope, vec1![cur! {l 1 o 0}])
        };

        let range_edits = {
            let insert_range = None;

            let delete_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(AbsoluteCharOffset(1), AbsoluteCharOffset(2)),
                chars: "\n".to_owned(),
            });
            RangeEdits {
                insert_range,
                delete_range,
            }
        };

        Edit {
            range_edits: vec1![range_edits],
            cursors: Change {
                new: new_cursors,
                old: cursors.clone(),
            },
        }
    };

    assert_eq!(edit, expected);
}

#[test]
fn get_insert_edit_produces_the_expected_edit_on_this_multi_byte_char_example() {
    let mut buffer = t_b!("Aa 0");
    buffer.set_cursors_from_vec1(vec1![cur! {l 0 o 2}, cur! {l 0 o 1}]);

    let rope = &buffer.rope;

    let cursors = &buffer.cursors;

    // Act
    let edit = get_insert_edit(rope, cursors, |_| "¡".to_owned());

    // Assert
    let expected = {
        let new_cursors = {
            //                     "A¡|a¡| 0"
            let expected_rope = r!("A¡a¡ 0".to_owned());

            Cursors::new(
                &expected_rope,
                vec1![Cursor::new(pos! {l 0 o 4}), Cursor::new(pos! {l 0 o 2})],
            )
        };

        let range_edits = {
            vec1![
                RangeEdits {
                    insert_range: Some(RangeEdit {
                        range: AbsoluteCharOffsetRange::new(
                            AbsoluteCharOffset(2),
                            AbsoluteCharOffset(3)
                        ),
                        chars: "¡".to_owned(),
                    }),
                    delete_range: None,
                },
                RangeEdits {
                    insert_range: Some(RangeEdit {
                        range: AbsoluteCharOffsetRange::new(
                            AbsoluteCharOffset(1),
                            AbsoluteCharOffset(2)
                        ),
                        chars: "¡".to_owned(),
                    }),
                    delete_range: None,
                }
            ]
        };

        Edit {
            range_edits,
            cursors: Change {
                new: new_cursors,
                old: cursors.clone(),
            },
        }
    };

    assert_eq!(edit, expected);
}

#[test]
fn get_insert_edit_produces_the_expected_edit_on_this_multi_cursor_cr_lf_example() {
    let mut buffer = t_b!("1\r2\r3\r4");
    buffer.set_cursors_from_vec1(vec1![cur! {l 3 o 0}, cur! {l 2 o 0}, cur! {l 1 o 0}]);

    let rope = &buffer.rope;

    let cursors = &buffer.cursors;

    // Act
    let edit = get_insert_edit(rope, cursors, |_| "\n".to_owned());

    // Assert
    let expected = {
        let new_cursors = {
            let expected_rope = r!("1\r\n2\r\n3\r\n4".to_owned());

            Cursors::new(
                &expected_rope,
                vec1![cur! {l 3 o 0}, cur! {l 2 o 0}, cur! {l 1 o 0}],
            )
        };

        let range_edits = {
            vec1![
                RangeEdits {
                    insert_range: Some(RangeEdit {
                        range: AbsoluteCharOffsetRange::new(
                            AbsoluteCharOffset(6),
                            AbsoluteCharOffset(7)
                        ),
                        chars: "\n".to_owned(),
                    }),
                    delete_range: None,
                },
                RangeEdits {
                    insert_range: Some(RangeEdit {
                        range: AbsoluteCharOffsetRange::new(
                            AbsoluteCharOffset(4),
                            AbsoluteCharOffset(5)
                        ),
                        chars: "\n".to_owned(),
                    }),
                    delete_range: None,
                },
                RangeEdits {
                    insert_range: Some(RangeEdit {
                        range: AbsoluteCharOffsetRange::new(
                            AbsoluteCharOffset(2),
                            AbsoluteCharOffset(3)
                        ),
                        chars: "\n".to_owned(),
                    }),
                    delete_range: None,
                }
            ]
        };

        Edit {
            range_edits,
            cursors: Change {
                new: new_cursors,
                old: cursors.clone(),
            },
        }
    };

    assert_eq!(edit, expected);
}

fn does_not_lose_characters_on<TestEdits: Borrow<[TestEdit]>>(
    initial_buffer: TextBuffer,
    edits: TestEdits,
) {
    let mut counts = get_counts(&initial_buffer);
    let mut buffer = deep_clone(&initial_buffer);

    for edit in edits.borrow().iter() {
        TestEdit::apply_with_counts(&mut buffer, &mut counts, edit);
    }

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

proptest! {
    #[test]
    fn does_not_lose_characters((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::All)) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_inserts((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::Insert)) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_non_control_inserts((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::RegexInsert("\\PC"))) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_non_cr_inserts((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::RegexInsert("[^\r]"))) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_set_cursor_heavy((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::SetCursorHeavy)) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_tab_in_out_heavy((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::TabInOutHeavy)) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_delete_and_tab_in_out_heavy((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::DeleteAndTabInOutHeavy)) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_delete_then_tab_out(buffer in arb::text_buffer_with_valid_cursors(), edits in arb::test_edit_delete_then_tab_out_vec()) {
        does_not_lose_characters_on(buffer, edits);
    }
}

#[test]
fn does_not_lose_characters_in_this_generated_case() {
    use TestEdit::*;
    does_not_lose_characters_on(
        t_b!(""),
        [InsertString("\u{b}".to_string()), DragCursors(pos!{l 0 o 0}), TabIn]
    );
}

#[test]
fn does_not_lose_characters_in_this_reduced_generated_case() {
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

    assert_eq!(get_counts(&buffer), counts);
}

#[test]
fn does_not_lose_characters_in_minimal_tab_in_case() {
    use TestEdit::*;
    does_not_lose_characters_on(
        t_b!(""),
        [TabIn]
    );
}

#[test]
fn does_not_lose_characters_in_this_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\u{a0}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);
    does_not_lose_characters_on(
        buffer,
        [TabIn]
    );
}

#[test]
fn does_not_lose_characters_in_this_reduced_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\u{a0}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);
    let mut counts = get_counts(&buffer);

    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}


#[test]
fn does_not_lose_characters_in_this_delete_then_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    use Move::*;
    let mut buffer = t_b!("\u{a0}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);
    does_not_lose_characters_on(
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
fn does_not_lose_characters_in_this_set_cursor_heavy_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("a ");
    buffer.set_cursor(cur!{l 0 o 0 h l 0 o 1}, Replace);
    does_not_lose_characters_on(
        buffer,
        [SetCursor(pos!{l 0 o 2}, Add), Delete]
    );
}

#[test]
fn does_not_lose_characters_in_this_tab_out_then_in_case() {
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

    assert_eq!(get_counts(&buffer), counts);
}

mod edit_arb;
mod undo_redo;
