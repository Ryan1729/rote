use super::*;

use platform_types::BorrowRope;

#[test]
fn asking_to_parse_the_empty_string_twice_does_not_panic() {
    let rust_lang = unsafe { tree_sitter_rust() };

    let mut parser = Parser::new();

    parser.set_language(rust_lang).unwrap();

    let to_parse = "";

    let mut tree = None;

    tree = parser.parse(to_parse, tree.as_ref());

    let final_tree = parser.parse(to_parse, tree.as_ref());

    assert!(final_tree.is_some());
}

#[test]
fn parsers_rust_to_c_abort_does_not_happen() {
    u!{BufferName, ParserKind, Style}
    let buffer_name = Path("fakefile.rs".into());

    let mut parsers = Parsers::default();

    let rope = Rope::from(r#"int main() {
    return strlen('d'); "";
}"#);
    let cursors = d!();

    let mut parser_kind = Rust(Extra);

    parsers.get_spans(
        (&rope).into(),
        &buffer_name,
        parser_kind
    );

    // Here we simulate the user switching languages
    parser_kind = C(Extra);

    let mut rope = edit::CursoredRope::new(
        rope,
        cursors
    );

    let insert_edit = edit::get_insert_edit(&rope, |_| "\n".to_owned());

    parsers.acknowledge_edit(
        &buffer_name,
        parser_kind,
        &insert_edit,
        rope.borrow_rope()
    );

    rope.apply(&insert_edit);

    // As of this writing, the abort happens after this.
    // uncomment this for a demonstation:
    //assert!(false, "pre parsers.get_spans");

    parsers.get_spans(
        rope.borrow_rope().into(),
        &buffer_name,
        parser_kind
    );

    // if we didn't panic/abort yet, the test passed.
}

#[test]
fn this_acknowedge_edit_does_not_cause_get_spans_to_panic_or_abort() {
    u!{BufferName, ParserKind, Style}
    use cursors::{Cursors, curs};
    use editor_types::{Cursor, cur, pos, *};
    use vec1::{vec1};

    let mut parsers = Parsers::default();

    let parser_kind = Rust(Extra);

    let buffer_name = Path("fakefile.rs".into());

    let s = r#"fn main() {
    println!("hi");
}"#;
    let rope = Rope::from(s);

    let cursors = curs!{rope, cur!{l 2 o 1}};

    let mut rope = edit::CursoredRope::new(
        rope,
        cursors
    );

    // pre-condition check
    parsers.get_spans(
        rope.borrow_rope().into(),
        &buffer_name,
        parser_kind
    );

    let mut expected_last_span_index = Some(s.len() - 1);

    for _ in 0..s.len() {
        let edit = edit::get_delete_edit(&rope);

        parsers.acknowledge_edit(
            &buffer_name,
            parser_kind,
            &edit,
            rope.borrow_rope()
        );
    
        rope.apply(&edit);
    
        let spans = parsers.get_spans(
            rope.borrow_rope().into(),
            &buffer_name,
            parser_kind
        );std::dbg!(&spans);
        
        assert_eq!(
            expected_last_span_index,
            spans.into_iter().last().map(|s| s.one_past_end.0)
        );
        expected_last_span_index = expected_last_span_index
            .and_then(|x| x.checked_sub(1));
        if expected_last_span_index == Some(0) {
            expected_last_span_index = None;
        }
    }

    // if we didn't panic/abort yet, the test passed.
}