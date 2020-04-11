use super::*;

#[test]
fn final_non_newline_offset_for_line_works_on_a_string_with_no_newline() {
    let rope = Rope::from("1234");

    assert_eq!(
        final_non_newline_offset_for_line(&rope, LineIndex(0)),
        CharOffset(4).into()
    );
}

#[test]
fn final_non_newline_offset_for_line_works_on_a_string_with_a_line_feed() {
    let rope = Rope::from("1234\n5678");

    assert_eq!(
        final_non_newline_offset_for_line(&rope, LineIndex(0)),
        CharOffset(4).into()
    );
}

#[test]
fn final_non_newline_offset_for_line_works_on_a_string_with_a_carriage_return_line_feed() {
    let rope = Rope::from("1234\r\n5678");

    assert_eq!(
        final_non_newline_offset_for_line(&rope, LineIndex(0)),
        CharOffset(4).into()
    );
}

#[test]
fn final_non_newline_offset_for_line_works_if_asked_about_a_non_existant_line() {
    let rope = Rope::from("1234\r\n5678");

    assert_eq!(final_non_newline_offset_for_line(&rope, LineIndex(2)), None);
}

#[test]
fn final_non_newline_offset_for_rope_line_works_when_iterating_over_lines() {
    // Non-breaking space =
    const NBSP: char = '\u{A0}';
    let text = format!("0\n 1\n  2\n   3\n    4\n\n{0}\n{0}1\n {0}2\n", NBSP);
    let rope = Rope::from(text);

    let max_index = rope.len_lines().0 - 1;
    let mut line_ends = Vec::with_capacity(max_index);

    for index in (0..max_index).map(LineIndex) {
        let line = rope.line(index).unwrap();
        dbg!(line);
        line_ends.push(final_non_newline_offset_for_rope_line(line));
    }

    let expected: Vec<_> = [1, 2, 3, 4, 5, 0, 1, 2, 3]
        .iter()
        .map(|&n| CharOffset(n))
        .collect();

    assert_eq!(line_ends, expected);
}

#[test]
fn final_non_newline_offset_for_rope_line_works_on_these_examples() {
    let rope = Rope::from("\n \n 1\n  \n  2\n");

    let max_index = rope.len_lines().0 - 1;
    let mut line_ends = Vec::with_capacity(max_index);

    for index in (0..max_index).map(LineIndex) {
        let line = rope.line(index).unwrap();
        dbg!(line);
        line_ends.push(final_non_newline_offset_for_rope_line(line));
    }

    let expected: Vec<_> = [0, 1, 2, 2, 3]
        .iter()
        .map(|&n| CharOffset(n))
        .collect();

    assert_eq!(line_ends, expected);
}
