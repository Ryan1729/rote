use super::*;

use pretty_assertions::assert_eq;

macro_rules! buffer_str_assert_eq {
    ($buffer: expr, $s: expr) => {
        let string: String = $buffer.rope.clone().into();
        assert_eq!(&string, $s);
    };
}

fn with_multiple_non_highlight_cursors_works_on(
    mut buffer: TextBuffer,
    inserts: Vec<arb::TestEdit>,
) {
    let len = inserts.len();

    let mut expecteds = Vec::with_capacity(len);

    for insert in inserts {
        expecteds.push(buffer.clone());
        arb::TestEdit::apply(&mut buffer, insert);
    }

    for i in (0..len).rev() {
        arb::TestEdit::apply(&mut buffer, arb::TestEdit::Delete);
        assert_text_buffer_eq_ignoring_history!(&buffer, &expecteds[i]);
    }
}

proptest! {
    #[test]
    fn with_multiple_non_highlight_cursors_works(
        buffer in arb::text_buffer_with_many_non_highlight_cursors(),
        inserts in arb::test_edits(SOME_AMOUNT, arb::TestEditSpec::Insert)
    ) {
        with_multiple_non_highlight_cursors_works_on(buffer, inserts);
    }
}

#[test]
fn with_multiple_non_highlight_cursors_works_on_this_trivial_generated_example(
) {
    with_multiple_non_highlight_cursors_works_on(
        t_b!(""),
        vec![TestEdit::Insert('0')],
    );
}

#[test]
fn with_multiple_non_highlight_cursors_works_on_this_smaller_generated_example(
) {
    with_multiple_non_highlight_cursors_works_on(
        t_b!("0"),
        vec![TestEdit::Insert('\n')],
    );
}

#[test]
fn with_multiple_non_highlight_cursors_works_on_this_smaller_splayed_example(
) {
    let mut buffer = t_b!("0");

    let expected = buffer.clone();
    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('\n'));

    arb::TestEdit::apply(&mut buffer, arb::TestEdit::Delete);
    assert_text_buffer_eq_ignoring_history!(&buffer, &expected);
}

#[test]
fn with_multiple_non_highlight_cursors_works_on_this_generated_example() {
    with_multiple_non_highlight_cursors_works_on(
        t_b!("0"),
        vec![TestEdit::Insert('A'), TestEdit::Insert('\n')],
    );
}

#[test]
fn with_multiple_non_highlight_cursors_works_on_this_cr_lf_example() {
    with_multiple_non_highlight_cursors_works_on(
        t_b!("A"),
        vec![TestEdit::Insert('\r'), TestEdit::Insert('\n')],
    );
}

#[test]
fn with_multiple_non_highlight_cursors_works_on_this_splayed_cr_lf_example()
{
    let mut buffer = t_b!("A");

    let mut expecteds = Vec::with_capacity(2);

    expecteds.push(buffer.clone());
    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('\r'));
    expecteds.push(buffer.clone());
    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('\n'));

    arb::TestEdit::apply(&mut buffer, arb::TestEdit::Delete);
    assert_text_buffer_eq_ignoring_history!(&buffer, &expecteds[1]);
    arb::TestEdit::apply(&mut buffer, arb::TestEdit::Delete);
    assert_text_buffer_eq_ignoring_history!(&buffer, &expecteds[0]);
}

#[test]
fn with_multiple_non_highlight_cursors_works_on_this_splayed_example() {
    let mut buffer = t_b!("0");

    let mut expecteds = Vec::with_capacity(2);

    expecteds.push(buffer.clone());
    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('A'));
    expecteds.push(buffer.clone());
    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('\n'));

    arb::TestEdit::apply(&mut buffer, arb::TestEdit::Delete);
    assert_text_buffer_eq_ignoring_history!(&buffer, &expecteds[1]);
    arb::TestEdit::apply(&mut buffer, arb::TestEdit::Delete);
    assert_text_buffer_eq_ignoring_history!(&buffer, &expecteds[0]);
}

/* this one takes a long time and has not failed recently
proptest! {
    #[test]
    fn with_all_but_end_cursors_works(
        buffer in arb::text_buffer_with_all_but_end_cursors(),
        inserts in arb::test_edits(SOME_AMOUNT, arb::TestEditSpec::Insert)
    ) {
        with_multiple_non_highlight_cursors_works_on(buffer, inserts);
    }
}
*/

#[test]
fn with_all_but_end_cursors_works_on_this_generated_example() {
    let mut buffer = t_b!("Aa 0");
    buffer.set_cursors_from_vec1(vec1![cur! {l 0 o 2}, cur! {l 0 o 1}]);

    let expected = buffer.clone();
    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('¡'));

    dbg!(&buffer);
    arb::TestEdit::apply(&mut buffer, arb::TestEdit::Delete);
    assert_text_buffer_eq_ignoring_history!(&buffer, &expected);
}

#[test]
fn with_all_but_end_cursors_works_on_this_larger_generated_example() {
    let mut buffer = t_b!("\u{dcf}aAAୟ");
    buffer.set_cursors_from_vec1(vec1![cur! {l 0 o 3}, cur! {l 0 o 2}, cur! {l 0 o 1}]);
    with_multiple_non_highlight_cursors_works_on(
        buffer,
        vec![TestEdit::Insert('\r'), TestEdit::Insert('\n')],
    );
}

#[test]
fn with_all_but_end_cursors_works_on_this_reduced_larger_generated_example()
{
    let mut buffer = t_b!("1234"); // was "\u{dcf}aAAୟ"
    buffer.set_cursors_from_vec1(vec1![cur! {l 0 o 3}, cur! {l 0 o 2}, cur! {l 0 o 1}]);

    let mut expecteds = Vec::with_capacity(2);

    expecteds.push(buffer.clone());
    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('\r'));
    dbg!(&buffer);
    expecteds.push(buffer.clone());
    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('\n'));

    arb::TestEdit::apply(&mut buffer, arb::TestEdit::Delete);
    assert_text_buffer_eq_ignoring_history!(&buffer, &expecteds[1]);
    arb::TestEdit::apply(&mut buffer, arb::TestEdit::Delete);
    assert_text_buffer_eq_ignoring_history!(&buffer, &expecteds[0]);
}

#[test]
fn with_multiple_non_highlight_cursors_works_on_this_motivating_example() {
    use arb::TestEdit::*;
    // | represents a cursor
    //                              "a|b|c|d|e|f|g"
    let mut buffer: TextBuffer = t_b!("abcdefg");

    buffer.set_cursor(pos! {l 0 o 1}, ReplaceOrAdd::Replace);
    buffer.set_cursor(pos! {l 0 o 2}, ReplaceOrAdd::Add);
    buffer.set_cursor(pos! {l 0 o 3}, ReplaceOrAdd::Add);
    buffer.set_cursor(pos! {l 0 o 4}, ReplaceOrAdd::Add);
    buffer.set_cursor(pos! {l 0 o 5}, ReplaceOrAdd::Add);
    buffer.set_cursor(pos! {l 0 o 6}, ReplaceOrAdd::Add);

    buffer_str_assert_eq!(buffer, "abcdefg");

    arb::TestEdit::apply(&mut buffer, Insert('1'));
    //                        "a1|b1|c1|d1|e1|f1|g"
    buffer_str_assert_eq!(buffer, "a1b1c1d1e1f1g");

    arb::TestEdit::apply(&mut buffer, Insert('2'));
    //                        "a12|b12|c12|d12|e12|f12|g"
    buffer_str_assert_eq!(buffer, "a12b12c12d12e12f12g");

    arb::TestEdit::apply(&mut buffer, Delete);
    //                        "a1|b1|c1|d1|e1|f1|g" again
    buffer_str_assert_eq!(buffer, "a1b1c1d1e1f1g");

    arb::TestEdit::apply(&mut buffer, Delete);
    //                              "a|b|c|d|e|f|g" again
    buffer_str_assert_eq!(buffer, "abcdefg");
}

