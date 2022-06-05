use super::*;

// This test demonstrates what I believe is a bug in tree-sitter. We have an 
// attempted workaround, in `parsers.rs` but if this test starts passing in the 
// future, we should be able to stop ignoring, and/or delete this test.
#[ignore]
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

    let insert_edit = edit::get_insert_edit((&rope, &cursors), |_| "\n".to_owned());

    parsers.acknowledge_edit(
        &buffer_name,
        parser_kind,
        &insert_edit,
        &rope
    );

    let mut rope = edit::CursoredRope::new(
        rope,
        cursors
    );
    edit::apply(&mut rope, &insert_edit);

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