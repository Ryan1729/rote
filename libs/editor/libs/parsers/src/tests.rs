use super::*;

// This tests demonstrates what I believe is a bug in tree-sitter. We have an 
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