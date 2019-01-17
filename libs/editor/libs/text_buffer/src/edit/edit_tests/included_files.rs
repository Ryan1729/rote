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

    let expected_line_suffix: &'static str = "precious";
    let expected_pre_chunk: &'static str = "\r\n    precious";
    let expected_post_chunk: &'static str = "\r\nprecious";

    //precondition given we assert it again below
    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix), "\n*****\n\nPrecondition failure!\n\n*****\n");
        assert!(s.contains(expected_pre_chunk));
    }

    let line_count = buffer.rope.len_lines();

    buffer.set_cursor(cur!{l 13 o 0 h l 14 o 13}, Replace);

    TestEdit::apply(&mut buffer, TabOut);

    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix));
        assert!(s.contains(expected_post_chunk));
    }
    assert_eq!(line_count, buffer.rope.len_lines());
}

#[test]
fn does_not_lose_a_line_in_this_codewise_unreduced_manually_found_case() {
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
    let expected_pre_chunk: &'static str = "\r\n    precious";
    let expected_post_chunk: &'static str = "\r\nprecious";

    //precondition given we assert it again below
    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix), "\n*****\n\nPrecondition failure!\n\n*****\n");
        assert!(s.contains(expected_pre_chunk));
    }

    let line_count = buffer.rope.len_lines();

    buffer.set_cursor(cur!{l 13 o 0 h l 14 o 13}, Replace);

    TestEdit::apply(&mut buffer, TabOut);

    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix));
        assert!(s.contains(expected_post_chunk));
    }
    assert_eq!(line_count, buffer.rope.len_lines());
}

#[test]
fn the_rope_acts_as_expected_in_this_does_not_lose_a_line_derived_case() {
    let mut rope = r!(include_str!("./test-text-0-reduced-3.txt"));

    rope.remove(AbsoluteCharOffsetRange::new_usize(971, 973).range());

    let line = rope.line(LineIndex(14)).unwrap();

    let after_spaces = CharOffset(TAB_STR_CHAR_COUNT);
    let slice_end = dbg!(line.len_chars());

    dbg!();
    assert_eq!(
        dbg!(line.slice(after_spaces..slice_end - 1)).unwrap(),
        "precious"
    );
    dbg!();
    
    assert_eq!(
        line.slice(after_spaces..slice_end).unwrap(),
        "precious\n"
    );
}