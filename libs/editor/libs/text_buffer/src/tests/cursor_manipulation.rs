use super::*;

use pretty_assertions::assert_eq;

#[test]
fn insert_with_matching_cursor_and_highlight_sets_highlight_to_none() {
    let mut buffer: TextBuffer = d!();

    {
        let mut c = buffer
            .cursors
            .get_cloned_cursors()
            .into_vec()
            .pop()
            .unwrap();
        c.set_highlight_position(c.get_position());
        assert_eq!(c.get_highlight_position(), None);
        buffer.cursors = Cursors::new(Vec1::new(c));
    }

    buffer.insert('1');

    {
        let c = buffer.cursors.first();
        assert_eq!(c.get_highlight_position(), None);
    }

    buffer.insert('2');

    {
        let c = buffer.cursors.first();
        assert_eq!(c.get_highlight_position(), None);
    }
}

#[test]
fn insertion_with_forward_selection_deletes_selected_text() {
    // Arrange
    let mut buffer: TextBuffer = d!();
    buffer.insert('1');
    buffer.insert('2');
    buffer.insert('5'); // We will attempt to fix this as part of the test
    buffer.insert('4');

    // TODO move these sanity checks into a separate test?
    cursor_assert! {
        buffer,
        p: pos! {l 0 o 4},
        h: None
    }

    buffer.move_cursor(0, Move::Left);
    buffer.move_cursor(0, Move::Left);

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 2},
        h: None
    }

    buffer.extend_selection(0, Move::Right);

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 3},
        h: Some(pos! {l 0 o 2})
    }

    // Act
    buffer.insert('3');

    // Assert
    let s: String = buffer.rope.into();
    assert_eq!(s, "1234");

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 3},
        h: None
    }
}

#[test]
fn insert_string_places_cursor_at_the_end() {
    // Arrange
    let mut buffer = t_b!("125");

    buffer.move_cursor(0, Move::Right);
    buffer.move_cursor(0, Move::Right);

    // Act
    buffer.insert_string("34".into());

    // Assert
    let s: String = buffer.rope.into();
    assert_eq!(s, "12345");

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 4},
        h: None
    }
}

#[test]
fn newline_places_cursor_in_correct_spot() {
    let mut buffer = t_b!("123");

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 0},
    }

    for _ in 0..5 {
        buffer.move_cursor(0, Move::Right);
    }

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 3},
    }

    buffer.insert('\n');

    cursor_assert! {
        buffer,
        p: pos! {l 1 o 0},
    }
}

#[test]
fn right_does_not_go_to_the_next_line_if_there_is_not_one() {
    let mut buffer = t_b!("123\n567");

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 0},
    }

    buffer.move_cursor(0, Move::Down);

    cursor_assert! {
        buffer,
        p: pos! {l 1 o 0},
    }

    buffer.move_cursor(0, Move::Right);
    buffer.move_cursor(0, Move::Right);
    buffer.move_cursor(0, Move::Right);

    cursor_assert! {
        buffer,
        p: pos! {l 1 o 3},
    }

    buffer.move_cursor(0, Move::Right);

    cursor_assert! {
        buffer,
        p: pos! {l 1 o 3},
    }
}

#[test]
fn in_cursor_bounds_does_not_allow_going_past_a_line_feed() {
    let rope = r!("123\n567");

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 5}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 4}), false);

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 3}), true);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 1 o 0}), true);
}

#[test]
fn in_cursor_bounds_does_not_allow_going_past_a_carriage_return_line_feed() {
    let rope = r!("123\r\n678");

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 5}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 4}), false);

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 3}), true);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 1 o 0}), true);
}

#[test]
fn in_cursor_bounds_works_on_line_feed() {
    let rope = r!("\n");

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 0}), true);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 1}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 2}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 1 o 0}), true);
}

#[test]
fn in_cursor_bounds_works_on_carriage_return_line_feed() {
    let rope = r!("\r\n");

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 0}), true);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 1}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 2}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 1 o 0}), true);
}

#[test]
fn in_cursor_bounds_does_not_allow_going_to_a_non_existant_line() {
    let rope = r!("123");

    assert_eq!(in_cursor_bounds(&rope, pos! {l 1 o 0}), false);
}

fn moving_across_lines(mut buffer: TextBuffer) {
    cursor_assert! {
        buffer,
        p: pos! {l 0 o 0},
    }

    for _ in 0..3 {
        buffer.move_cursor(0, Move::Right);
    }

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 3},
    }

    buffer.move_cursor(0, Move::Right);

    cursor_assert! {
        buffer,
        p: pos! {l 1 o 0},
    }

    buffer.move_cursor(0, Move::Left);

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 3},
    }
}

#[test]
fn moving_across_line_feeds_works() {
    moving_across_lines(t_b!("123\n567"));
}

#[test]
fn moving_across_carriage_return_line_feeds_works() {
    moving_across_lines(t_b!("123\r\n567"));
}

macro_rules! all_cursor_movements {
    ($line_separator: literal) => {
        let mut buffer: TextBuffer = t_b!(concat!("123", $line_separator, "567"));
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::Right);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::Down);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::Up);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::Left);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::ToLineEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 3},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::ToLineStart);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::ToBufferEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::ToBufferStart);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            s: CursorState::None
        }
    };
}

#[test]
fn all_cursor_movements_across_line_feeds_works() {
    all_cursor_movements!("\n");
}

#[test]
fn all_cursor_movements_across_carriage_return_line_feeds_works() {
    all_cursor_movements!("\r\n");
}

macro_rules! multiline_selection {
    ($line_separator: literal) => {
        let mut buffer: TextBuffer = t_b!(concat!("123", $line_separator, "567"));
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None
        }

        buffer.extend_selection(0, Move::ToBufferEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: pos! {l 0 o 0}
        }

        buffer.move_cursor(0, Move::ToBufferEnd);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        buffer.extend_selection(0, Move::ToBufferStart);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: pos! {l 1 o 3}
        }

        buffer.move_cursor(0, Move::ToLineEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 3},
            h: None
        }

        buffer.extend_selection(0, Move::Right);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: pos! {l 0 o 3}
        }

        buffer.extend_selection(0, Move::ToLineEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: pos! {l 0 o 3}
        }

        buffer.extend_selection(0, Move::ToLineStart);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: pos! {l 0 o 3}
        }

        buffer.move_cursor(0, Move::Down);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        buffer.extend_selection(0, Move::Left);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 3},
            h: pos! {l 1 o 0},
        }

        buffer.extend_selection(0, Move::ToLineStart);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: pos! {l 1 o 0}
        }

        buffer.extend_selection(0, Move::ToLineEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 3},
            h: pos! {l 1 o 0}
        }
    };
}

#[test]
fn multiline_selection_across_line_feeds_works() {
    multiline_selection!("\n");
}

#[test]
fn multiline_selection_across_carriage_return_line_feeds_works() {
    multiline_selection!("\r\n");
}

macro_rules! moving_by_words {
    ($line_separator: literal) => {
        use Move::*;
        let mut buffer: TextBuffer = t_b!(concat!(
            // non word before and after
            "(123)",
            $line_separator,
            // has non word before
            "\"456",
            $line_separator,
            // has non word after
            "789\"",
            $line_separator,
            // multiple words and tyes of non word charactera
            "{[(012), (345)]}",
            $line_separator,
        ));
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 5},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 6},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 9},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 10},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 13},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 16},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 4 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 4 o 0},
            h: None,
            s: CursorState::PressedAgainstWall
        }

        // Now back the other way

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 13},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 10},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 9},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 6},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        dbg!(1);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: CursorState::PressedAgainstWall
        }

        let mut alt_buffer: TextBuffer = t_b!(concat!(
            // starts with line
            $line_separator,
            //does not end with new line
            "123",
        ));

        alt_buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, ToNextLikelyEditLocation);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: CursorState::PressedAgainstWall
        }

        // Back to the beginning again

        alt_buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, ToPreviousLikelyEditLocation);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: CursorState::PressedAgainstWall
        }
    };
}

#[test]
fn moving_by_words_across_line_feeds_works() {
    moving_by_words!("\n");
}

#[test]
fn moving_by_words_across_carriage_return_line_feeds_works() {
    moving_by_words!("\r\n");
}

macro_rules! selecting_likely_edit_locations_works_on_a_single_character {
    ($single_char_str: expr) => {
        let mut buffer = t_b!($single_char_str);

        buffer.select_char_type_grouping(pos! {}, ReplaceOrAdd::Replace);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            h: pos! {l 0 o 0},
            s: d!()
        }

        buffer.select_char_type_grouping(pos! {l 0 o 1}, ReplaceOrAdd::Replace);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            h: pos! {l 0 o 0},
            s: d!()
        }
    };
}

#[test]
fn selecting_likely_edit_locations_works_on_a_word_char() {
    selecting_likely_edit_locations_works_on_a_single_character!("a");
}

#[test]
fn selecting_likely_edit_locations_works_on_a_whitespace_char() {
    selecting_likely_edit_locations_works_on_a_single_character!(" ");
}

#[test]
fn selecting_likely_edit_locations_works_on_a_punctuation_char() {
    selecting_likely_edit_locations_works_on_a_single_character!(".");
}

#[test]
fn selecting_likely_edit_locations_works_on_wrapped_with_braces_example() {
    let mut buffer = t_b!("{a}");

    let len = "{a}".len();
    let mut ranges = Vec::with_capacity(len);

    for offset in (0..len).map(CharOffset) {
        let p = Position { offset, ..d!() };

        buffer.select_char_type_grouping(p, ReplaceOrAdd::Replace);

        let c = buffer.cursors.first();

        ranges.push((c.get_highlight_position(), c.get_position()))
    }

    assert!(ranges.contains(&(Some(pos! {l 0 o 0}), pos! {l 0 o 1})));

    assert!(ranges.contains(&(Some(pos! {l 0 o 1}), pos! {l 0 o 2})));

    assert!(ranges.contains(&(Some(pos! {l 0 o 2}), pos! {l 0 o 3})));
}

macro_rules! check_select_bounds {
    ($buffer: expr, ($left_edge: expr, $right_edge: expr)) => {
        for offset in ($left_edge.offset.0..$right_edge.offset.0).map(CharOffset) {
            let p = Position { offset, ..d!() };

            $buffer.select_char_type_grouping(p, ReplaceOrAdd::Replace);

            cursor_assert! {
                $buffer,
                p: $right_edge,
                h: $left_edge,
                s: d!()
            }
        }
    };
}

#[test]
fn selecting_likely_edit_locations_works_on_this_snake_case_example() {
    let mut buffer = t_b!("{snake_case_example}");

    let inner_len = "snake_case_example".len();
    let inner_left_edge = pos! {l 0 o 1};
    let inner_right_edge = Position {
        offset: CharOffset(1 + inner_len),
        ..d!()
    };

    check_select_bounds! {
        buffer,
        (pos!{}, inner_left_edge)
    }

    check_select_bounds! {
        buffer,
        (inner_left_edge, inner_right_edge)
    }

    buffer.select_char_type_grouping(inner_right_edge, ReplaceOrAdd::Replace);

    let last = Position {
        offset: inner_right_edge.offset + 1,
        ..d!()
    };
    cursor_assert! {
        buffer,
        p: last,
        h: inner_right_edge,
        s: d!()
    }

    buffer.select_char_type_grouping(last, ReplaceOrAdd::Replace);

    cursor_assert! {
        buffer,
        p: last,
        h: inner_right_edge,
        s: d!()
    }
}

#[test]
fn selecting_likely_edit_locations_works_on_this_subtraction_example() {
    let mut buffer = t_b!("(this-that)");

    let this_left_edge = pos! {l 0 o 1};
    let this_right_edge = pos! {l 0 o 5};
    let minus_left_edge = this_right_edge;
    let minus_right_edge = pos! {l 0 o 6};
    let that_left_edge = minus_right_edge;
    let that_right_edge = pos! {l 0 o 10};

    check_select_bounds! {
        buffer,
        (pos!{}, this_left_edge)
    }

    check_select_bounds! {
        buffer,
        (this_left_edge, this_right_edge)
    }

    check_select_bounds! {
        buffer,
        (minus_left_edge, minus_right_edge)
    }

    check_select_bounds! {
        buffer,
        (that_left_edge, that_right_edge)
    }

    buffer.select_char_type_grouping(that_right_edge, ReplaceOrAdd::Replace);

    let last = Position {
        offset: that_right_edge.offset + 1,
        ..d!()
    };
    cursor_assert! {
        buffer,
        p: last,
        h: that_right_edge,
        s: d!()
    }

    buffer.select_char_type_grouping(last, ReplaceOrAdd::Replace);

    cursor_assert! {
        buffer,
        p: last,
        h: that_right_edge,
        s: d!()
    }
}

macro_rules! four_spaces {
    () => {
        "    "
    };
}

#[test]
fn selecting_likely_edit_locations_works_on_this_camel_case_example() {
    let mut buffer = t_b!(concat!("\tcamelCase", four_spaces!()));

    let camel_case_left_edge = pos! {l 0 o 1};
    let camel_case_right_edge = pos! {l 0 o 10};
    let four_spaces_left_edge = camel_case_right_edge;
    let four_spaces_right_edge = pos! {l 0 o 14};

    check_select_bounds! {
        buffer,
        (pos!{}, camel_case_left_edge)
    }

    check_select_bounds! {
        buffer,
        (camel_case_left_edge, camel_case_right_edge)
    }

    check_select_bounds! {
        buffer,
        (four_spaces_left_edge, four_spaces_right_edge)
    }
}

#[test]
fn get_previous_likely_edit_location_finds_the_first_location_in_the_file_before_a_single_char() {
    assert_eq!(
        get_previous_selection_point(&r!("a"), pos! {l 0 o 1}),
        Some(pos! {})
    );
}

#[test]
fn get_next_selection_point_finds_the_end_of_the_word_before_whitespace() {
    assert_eq!(
        get_next_selection_point(&r!(concat!("ab", four_spaces!())), pos! {l 0 o 1}),
        Some(pos! {l 0 o 2})
    );
}

#[test]
fn get_next_selection_point_finds_the_end_of_the_word_before_punctuation() {
    assert_eq!(
        get_next_selection_point(&r!("ab..."), pos! {l 0 o 1}),
        Some(pos! {l 0 o 2})
    );
}

#[test]
fn get_next_selection_point_finds_the_end_of_the_punctuation_before_a_word() {
    assert_eq!(
        get_next_selection_point(&r!("...ab"), pos! {l 0 o 1}),
        Some(pos! {l 0 o 3})
    );
}

#[test]
fn get_next_selection_point_finds_the_end_of_the_punctuation_before_whitespace() {
    assert_eq!(
        get_next_selection_point(&r!(concat!("...", four_spaces!())), pos! {l 0 o 1}),
        Some(pos! {l 0 o 3})
    );
}

#[test]
fn get_next_selection_point_finds_the_end_of_the_whitespace_before_a_word() {
    assert_eq!(
        get_next_selection_point(&r!(concat!(four_spaces!(), "ab")), pos! {l 0 o 1}),
        Some(pos! {l 0 o 4})
    );
}

#[test]
fn get_next_selection_point_finds_the_end_of_the_whitespace_before_punctuation() {
    assert_eq!(
        get_next_selection_point(&r!(concat!(four_spaces!(), "...")), pos! {l 0 o 1}),
        Some(pos! {l 0 o 4})
    );
}

macro_rules! moving_onto_shorter_lines {
    ($line_separator: literal) => {
        use Move::*;
        let mut buffer: TextBuffer = t_b!(concat!($line_separator, "123", $line_separator,));

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Down);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }
        dbg!(1);

        buffer.move_cursor(0, Right);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }
        dbg!(2);

        buffer.move_cursor(0, Up);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }
        dbg!(3);

        buffer.move_cursor(0, Down);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToLineEnd);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Up);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Down);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToLineStart);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Right);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Down);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Up);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToLineEnd);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }
        dbg!(4);

        buffer.move_cursor(0, Down);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 0},
            h: None,
            s: d!()
        }
        dbg!(5);

        // longer than one line

        let mut buffer: TextBuffer = t_b!(concat!(
            "1",
            $line_separator,
            "234",
            $line_separator,
            "5",
            $line_separator
        ));

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Down);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Right);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Up);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Down);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToLineEnd);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Up);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Down);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToLineStart);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Right);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Down);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Up);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToLineEnd);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Down);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 1},
            h: None,
            s: d!()
        }
    };
}

#[test]
fn moving_onto_shorter_lines_across_line_feeds_works() {
    moving_onto_shorter_lines!("\n");
}

#[test]
fn moving_onto_shorter_lines_across_carriage_return_line_feeds_works() {
    moving_onto_shorter_lines!("\r\n");
}

proptest! {
    #[test]
    fn right_n_times_is_the_same_as_move_right_n_times(
        (rope, pos, AbsoluteCharOffset(n)) in arb_rope_and_pos_and_offset(),
    ) {
        let mut c1 = Cursor::new(pos);
        let mut c2 = Cursor::new(pos);

        move_cursor::right_n_times(&rope, &mut c1, n);

        for _ in 0..n {
            move_cursor::directly(&rope, &mut c2, Move::Right);
        }

        assert_eq!(c1, c2);
    }
}
