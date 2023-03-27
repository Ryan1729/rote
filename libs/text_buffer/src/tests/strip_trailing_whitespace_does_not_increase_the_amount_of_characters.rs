use super::{*};
use arb::{TestEdit, get_normalized_newline_counts};
use proptest::proptest;
use pretty_assertions::assert_eq;

fn on(mut buffer: TextBuffer) {
    let initial_string = String::from(&buffer);
    let initial_counts = get_normalized_newline_counts(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::StripTrailingWhitespace);

    let final_counts = get_normalized_newline_counts(&buffer);

    for c in final_counts.keys() {
        assert!(
            initial_counts.get(c).is_some(),
            r#"
            {c:?} was unexpectedly added by stripping trailng whitespace
            initial:
            {initial_string:?}
            final:
            {:?}
            "#,
            String::from(&buffer)
        );
    }

    assert!(
        initial_counts.keys().count() >= final_counts.keys().count()
    );

    for c in initial_counts.keys() {
        let initial_count = initial_counts.get(c).unwrap();
        if let Some(final_count) = final_counts.get(c) {
            if *c == '\n' {
                assert!(
                    initial_count >= final_count,
                    r#"final count for linebreak characters was {final_count},
                    which is more than the initial count of {initial_count}
                    initial:
                    {initial_string:?}
                    final:
                    {:?}
                    "#,
                    String::from(&buffer)
                );
            } else {
                assert!(
                    initial_count >= final_count,
                    r#"final count for {c:?} was {final_count},
                    which is more than the initial count of {initial_count}
                    initial:
                    {initial_string:?}
                    final:
                    {:?}
                    "#,
                    String::from(&buffer)
                );
            }
        }
    }
}

proptest! {
    #[test]
    fn strip_trailing_whitespace_does_not_increase_the_amount_of_characters(
        buffer in arb::text_buffer_with_many_cursors(),
    ) {
        on(buffer);
    }
}

#[test]
fn on_this_simple_generated_example() {
    let mut buffer = t_b!("\u{2029}");
    buffer.set_cursor(cur!{l 1 o 0 h l 0 o 0}, ReplaceOrAdd::Replace);
    on(buffer);
}

#[test]
fn on_this_complex_generated_example() {
    let mut buffer = t_b!("+\r\u{3e7ae}/�I\u{b}");
    buffer.set_cursor(cur!{l 0 o 1}, ReplaceOrAdd::Add);
    on(buffer);
}