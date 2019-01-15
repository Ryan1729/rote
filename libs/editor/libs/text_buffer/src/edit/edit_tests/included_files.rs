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

    buffer.set_cursor(cur!{l 86 o 0 h l 104 o 2}, Replace);

    dbg!(&buffer.rope);

    TestEdit::apply(&mut buffer, Delete);

    println!();
    println!();
    println!();
    dbg!(&buffer.rope);

    let expected_line_suffix = "assert_eq!(u.old, e.new);";

    //precondition given we assert it again below
    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix), "\n*****\n\nPrecondition failure!\n\n*****\n");
    }

    let line_count = buffer.rope.len_lines();

    buffer.set_cursor(cur!{l 87 o 0 h l 88 o 37}, Replace);

    TestEdit::apply(&mut buffer, TabOut);

    {
        let s: String = buffer.rope.clone().into();
        assert!(s.contains(expected_line_suffix));
    }
    assert_eq!(line_count, buffer.rope.len_lines());
}