use super::*;

#[test]
fn insert_with_matching_cursor_and_highlight_sets_highlight_to_none() {
    let mut buffer: TextBuffer = d!();

    {
        let c = &mut buffer.cursors[0];
        c.set_highlight_position(c.get_position());
        assert_eq!(c.get_highlight_position(), None);
    }

    buffer.insert('1');

    {
        let c = &buffer.cursors[0];
        assert_eq!(c.get_highlight_position(), None);
    }

    buffer.insert('2');

    {
        let c = &buffer.cursors[0];
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
            "(123)", $line_separator,
            // has non word before
            "\"456", $line_separator,
            // has non word after
            "789\"", $line_separator,
            // multiple words and tyes of non word charactera
            "{[(012), (345)]}", $line_separator,
        ));
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }
        dbg!();
        buffer.move_cursor(0, ToNextWordBoundary);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 5},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 6},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 8},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 10},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 13},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 16},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 4 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 4 o 0},
            h: None,
            s: CursorState::PressedAgainstWall
        }

        // Now back the other way

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 13},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 10},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 9},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 6},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, ToPreviousWordBoundary);
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

        alt_buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, ToNextWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: CursorState::PressedAgainstWall
        }

        // Back to the beginning again

        alt_buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, ToPreviousWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, ToPreviousWordBoundary);
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
