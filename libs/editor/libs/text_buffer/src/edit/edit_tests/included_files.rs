#![deny(dead_code, unused_variables)]

use super::*;

use pretty_assertions::assert_eq;

#[test]
fn does_not_lose_a_line_in_this_manually_found_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!(include_str!("./test-text-0.txt"));

    buffer.set_cursor(cur!{l 21 o 0 h l 38 o 2}, Replace);

    TestEdit::apply(&mut buffer, Delete);
    TestEdit::apply(&mut buffer, Delete);
    TestEdit::apply(&mut buffer, Delete);
    TestEdit::apply(&mut buffer, Delete);
    TestEdit::apply(&mut buffer, Delete);

    let expected_line_suffix = "assert_eq!(u.old, e.new);";

    //precondition given we assert it again below
    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix), "\n*****\n\nPrecondition failure!\n\n*****\n");
    }

    let line_count = buffer.rope.len_lines();

    buffer.set_cursor(cur!{l 92 o 0 h l 93 o 37}, Replace);

    TestEdit::apply(&mut buffer, TabOut);

    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix));
    }
    assert_eq!(line_count, buffer.rope.len_lines());
}

#[test]
fn does_not_lose_a_line_in_this_reduced_manually_found_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!(include_str!("./test-text-0-reduced-0.txt"));

    buffer.set_cursor(cur!{l 21 o 0 h l 38 o 2}, Replace);

    TestEdit::apply(&mut buffer, Delete);
    TestEdit::apply(&mut buffer, Delete);
    TestEdit::apply(&mut buffer, Delete);
    TestEdit::apply(&mut buffer, Delete);
    TestEdit::apply(&mut buffer, Delete);

    let expected_line_suffix = "assert_eq!(u.old, e.new);";

    //precondition given we assert it again below
    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix), "\n*****\n\nPrecondition failure!\n\n*****\n");
    }

    let line_count = buffer.rope.len_lines();

    buffer.set_cursor(cur!{l 92 o 0 h l 93 o 37}, Replace);

    TestEdit::apply(&mut buffer, TabOut);

    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix));
    }
    assert_eq!(line_count, buffer.rope.len_lines());
}

#[test]
fn does_not_lose_a_line_in_this_further_reduced_manually_found_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!(include_str!("./test-text-0-reduced-1.txt"));

    buffer.set_cursor(cur!{l 12 o 0 h l 13 o 0}, Replace);

    dbg!(&buffer.rope);

    TestEdit::apply(&mut buffer, Delete);

    println!();
    println!();
    println!();
    dbg!(&buffer.rope);

    let expected_line_suffix = "precious";

    //precondition given we assert it again below
    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix), "\n*****\n\nPrecondition failure!\n\n*****\n");
    }

    let line_count = buffer.rope.len_lines();

    buffer.set_cursor(cur!{l 13 o 0 h l 14 o 13}, Replace);

    TestEdit::apply(&mut buffer, TabOut);

    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix));
    }
    assert_eq!(line_count, buffer.rope.len_lines());
}

#[test]
fn does_not_lose_a_line_in_this_codewise_reduced_manually_found_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut delete_buffer = t_b!(include_str!("./test-text-0-reduced-2.txt"));

    delete_buffer.set_cursor(cur!{l 12 o 0 h l 13 o 0}, Replace);

    dbg!(&delete_buffer);

    TestEdit::apply(&mut delete_buffer, Delete);

    println!();
    println!();
    println!();
    let mut buffer: TextBuffer = d!();
    buffer.rope = delete_buffer.rope;
    dbg!(&buffer);

    let expected_line_suffix = "precious";

    //precondition given we assert it again below
    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix), "\n*****\n\nPrecondition failure!\n\n*****\n");
    }

    let line_count = buffer.rope.len_lines();

    buffer.set_cursor(cur!{l 13 o 0 h l 14 o 13}, Replace);

    let cloned_rope = buffer.rope.clone();

    let range_edits = vec1![{
            {
                let selected_min = AbsoluteCharOffset(975);
                let selected_max = AbsoluteCharOffset(989);
                let selected_range = AbsoluteCharOffsetRange::new(selected_min, selected_max);

                let mut chars = String::new();

                let delete_count = CharOffset(TAB_STR_CHAR_COUNT);

                {
                    let index = LineIndex(13);
                    let line = cloned_rope.line(index).unwrap();

                    let (relative_line_end, slice_end) = {
                        let first_char_of_line: AbsoluteCharOffset = cloned_rope.line_to_char(index).unwrap();
                        let end_of_selection_on_line: CharOffset = selected_max - first_char_of_line;
                        dbg!(end_of_selection_on_line, end_of_selection_on_line)
                    };

                    let first_non_white_space_offset: Option<CharOffset> =
                        get_first_non_white_space_offset_in_range(line, d!()..=relative_line_end);

                    dbg!(&first_non_white_space_offset, relative_line_end);

                    dbg!(delete_count, slice_end);
                    line.slice(delete_count..(slice_end - 1)).and_then(|l| dbg!(l.as_str()));
                    chars.push_str(line.slice(delete_count..slice_end).and_then(|l| dbg!(l.as_str())).unwrap());
                    // what should happen
                    //chars.push_str("\r\n");
                }

                {
                    let line = cloned_rope.line(LineIndex(14)).unwrap();

                    let (relative_line_end, slice_end) = {
                        (final_non_newline_offset_for_rope_line(line), line.len_chars())
                    };

                    let first_non_white_space_offset: Option<CharOffset> =
                        get_first_non_white_space_offset_in_range(line, d!()..=relative_line_end);

                    dbg!(&first_non_white_space_offset, relative_line_end);

                    dbg!(delete_count, slice_end);
                    line.slice(delete_count..(slice_end - 1)).and_then(|l| dbg!(l.as_str()));
                    chars.push_str(line.slice(delete_count..slice_end).and_then(|l| dbg!(l.as_str())).unwrap());
                    // what should happen
                    //chars.push_str("precious");
                }

                dbg!(
                    RangeEdits {
                        insert_range: Some(RangeEdit { chars, range: AbsoluteCharOffsetRange::new_usize(selected_min.0, selected_max.0 - TAB_STR_CHAR_COUNT) }),
                        delete_range: Some(RangeEdit {    chars: "\r\n    precious".to_owned(),    range: selected_range,}),
                    }
                )
            }
    }];

    let edit = Edit {
        range_edits,
        cursors: Change {
            new: buffer.cursors.clone(),
            old: buffer.cursors.clone(),
        },
    };


    dbg!(&edit);
    buffer.apply_edit(edit, ApplyKind::Playback);

    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix));
    }
    assert_eq!(line_count, buffer.rope.len_lines());
}

#[test]
fn the_rope_acts_as_expected_in_this_does_not_lose_a_line_derived_case() {
    use ReplaceOrAdd::*;
    let mut delete_buffer = t_b!(include_str!("./test-text-0-reduced-3.txt"));

    delete_buffer.set_cursor(cur!{l 12 o 0 h l 13 o 0}, Replace);

    dbg!(&delete_buffer);

    let edit = Edit {
    range_edits: 
        vec1![
            RangeEdits {
                insert_range: None,
                delete_range: Some(
                    RangeEdit {
                        chars: "0\n".to_owned(),
                        range: AbsoluteCharOffsetRange::new_usize(971, 973),
                    },
                ),
            },
        ],
    cursors: Change {
        old: delete_buffer.cursors.clone(),
        new: delete_buffer.cursors.clone(),
    },
};

    delete_buffer.apply_edit(edit, ApplyKind::Playback);

    let rope = delete_buffer.rope;

    let line = rope.line(LineIndex(14)).unwrap();

    let after_spaces = CharOffset(TAB_STR_CHAR_COUNT);
    let slice_end = dbg!(line.len_chars());

    dbg!();
    assert_eq!(
        dbg!(line.slice(after_spaces..slice_end - 1)).and_then(|l| dbg!(l.as_str())).unwrap(),
        "preciou"
    );
    dbg!();
    
    assert_eq!(
        line.slice(after_spaces..slice_end).and_then(|l| dbg!(l.as_str())).unwrap(),
        "precious"
    );
}