use super::*;
use proptest::proptest;

const SOME_AMOUNT: usize = 16;

mod arb {
    use proptest::{collection, sample, prelude::{Strategy, any}};
    pub use pub_arb_rust_code::rust_code;

    // TODO parse the /tree-sitter-rust/src/node-types.json file
    // so we get them all.
    const RUST_NODE_TYPES: [&'static str; 11] = [
        "source_file",
        "function_item",
        "identifier",
        "parameters",
        "block",
        "string_literal",
        "let_declaration",
        "match_pattern",
        "line_comment",
        "block_comment",
        "primitive_type",
    ];

    pub fn rust_node_type() -> impl Strategy<Value = String> {
        any::<sample::Selector>().prop_map(|s| (*s.select(&RUST_NODE_TYPES)).to_owned())
    }

    pub fn query_source(max_size: usize) -> impl Strategy<Value = String> {
        collection::vec(rust_node_type(), 0..max_size).prop_map(move |v| {
            let mut src = String::with_capacity(max_size * 16);

            let mut index = 0;
            for s in v {
                src.push_str(&format!("({}) @cap{}", s, index));
                src.push_str("\n");
                index += 1;
            }

            src
        })
    }
}

const COMMENT: SpanKind = SpanKind::COMMENT;
const PLAIN: SpanKind = SpanKind::PLAIN;
const STRING: SpanKind = SpanKind::STRING;

macro_rules! get_rust_parser {
    () => {{
        let mut rust = Parser::new();
        let rust_lang = unsafe { tree_sitter_rust() };
        rust.set_language(rust_lang).unwrap();

        rust
    }}
}

macro_rules! get_rust_tree {
    ($code: expr) => {{
        let mut rust = get_rust_parser!();

        rust.parse($code, None)
    }}
}

macro_rules! spans_assert {
    ($spans: expr) => {
        spans_assert!($spans, "");
    };
    ($spans: expr, $suffix: expr) => {
        spans_assert!($spans, $suffix, 0);
    };
    ($spans: expr, $suffix: expr, skip kind) => {
        spans_assert!($spans, $suffix, 1);
    };
    // internal
    ($spans: expr, $suffix: expr, $mode: expr) => {
        let spans = $spans;
        let mode = $mode;

        let mut previous_kind = None;
        let mut previous_end_byte_index = 0;
        for (i, s) in spans.clone().into_iter().enumerate() {
            assert_ne!(
                s.end_byte_index,
                0,
                "the span at index {}, {:?} has an end_byte_index of 0. This indicates a useless 0 length span, and so it should be removed.",
                i,
                s
            );

            match mode {
                1 => {}
                _ => {
                    if let Some(prev) = previous_kind {
                        assert_ne!(
                            s.kind,
                            prev,
                            "at index {} in spans was {:?} which has the same kind as the previous span: {:?}", 
                            i,
                            s,
                            spans,
                        );
                    }
                    previous_kind = Some(s.kind);

                    assert_ne!(
                        s.end_byte_index,
                        previous_end_byte_index,
                        "at index {} in spans was {:?} which has the same end_byte_index as the previous span: {:?}", 
                        i,
                        s,
                        spans,
                    );
                }
            }
            
            assert!(
                previous_end_byte_index <= s.end_byte_index,
                "{} > {} {}\n\n{:?}",
                previous_end_byte_index,
                s.end_byte_index,
                $suffix,
                spans
            );
            previous_end_byte_index = s.end_byte_index;
        }
    }
}

fn get_rust_parser_and_query(query_source: &str) -> (Parser, Query) {
    let mut rust = Parser::new();
    let rust_lang = unsafe { tree_sitter_rust() };
    rust.set_language(rust_lang).unwrap();

    let query = Query::new(
        rust_lang,
        query_source
    ).unwrap();

    return (rust, query);
}

fn arbitary_span_kind_from_match(Match {    
    pattern_index,
    capture: _
} : Match) -> SpanKind {
    sk!(pattern_index.to_le_bytes()[0])
}

fn query_spans_for_produces_valid_rust_spans_on(
    code: &str,
    query_source: &str,
) {
    let (mut rust, query) = get_rust_parser_and_query(query_source);

    let tree = rust.parse(&code, None);
    
    let spans = query_spans_for(tree.as_ref(), &query, code, arbitary_span_kind_from_match);

    spans_assert!(spans);
}

proptest!{
    #[test]
    #[ignore] // takes a long time
    fn query_spans_for_produces_valid_rust_spans(
        code in arb::rust_code(SOME_AMOUNT),
        query_source in arb::query_source(SOME_AMOUNT)
    ) {
        query_spans_for_produces_valid_rust_spans_on(
            &code,
            &query_source
        )
    }
}

#[test]
fn query_spans_for_produces_valid_rust_spans_on_this_predicate_case() {
    query_spans_for_produces_valid_rust_spans_on(
        PREDICATE_EXAMPLE,
        "(source_file) @cap0\n"
    )
}

#[test]
fn query_spans_for_produces_valid_rust_spans_on_this_predicate_case_with_an_odd_query() {
    query_spans_for_produces_valid_rust_spans_on(
        PREDICATE_EXAMPLE,
        "(identifier) @cap0
        (identifier) @cap1"
    )
}

#[test]
fn query_spans_for_produces_valid_rust_spans_on_this_generated_predicate_case_with_an_odd_query() {
    query_spans_for_produces_valid_rust_spans_on(
        "fn r#f() -> bool{\nfalse\n}\n\n",
        "(identifier) @cap0
        (identifier) @cap1"
    )
}

#[test]
fn query_spans_for_produces_the_right_result_on_this_multiple_match_case() {
    const OUTER: SpanKind = sk!(1);
    const INNER: SpanKind = sk!(2);
    // As of this writing this only works if OTHER is the default.
    const OTHER: SpanKind = SpanKind::PLAIN; 
    fn span_kind_from_match_example(Match {    
        pattern_index,
        ..
    } : Match) -> SpanKind {    
        match pattern_index {
            0 => OUTER,
            1 => INNER,
            _ => OTHER,
        }
    }

    let foo = "fn f(s: i32) {}";
    let query_source = "
        (parameters) @p
        (primitive_type) @t
    ";

    let (mut rust, query) = get_rust_parser_and_query(query_source);

    let tree = rust.parse(foo, None);
    
    let spans = query_spans_for(tree.as_ref(), &query, foo, span_kind_from_match_example);

    assert_eq!(
        spans,
        vec![
            SpanView { kind: OTHER, end_byte_index: 4 },
            SpanView { kind: OUTER, end_byte_index: 8 },
            SpanView { kind: INNER, end_byte_index: 11 },
            SpanView { kind: OUTER, end_byte_index: 12 },
            SpanView { kind: OTHER, end_byte_index: foo.len()},
        ]
    )
}

#[test]
fn query_spans_for_produces_the_right_result_on_this_basic_case() {
    fn span_kind_from_match_example(Match {    
        pattern_index,
        ..
    } : Match) -> SpanKind {    
        match pattern_index {
            0 | 1 => COMMENT,
            2 => STRING,
            _ => PLAIN,
        }
    }

    let foo = 
"fn main() {
let hi = \"hi\";
// TODO
}";

    let query_source = "
        (line_comment) @c1
        (block_comment) @c2
        (string_literal) @s
    ";

    let (mut rust, query) = get_rust_parser_and_query(query_source);

    let tree = rust.parse(foo, None);
    
    let spans = query_spans_for(tree.as_ref(), &query, foo, span_kind_from_match_example);

    assert_eq!(
        spans,
        vec![
            SpanView { kind: PLAIN, end_byte_index: 21 },
            SpanView { kind: STRING, end_byte_index: 21 + 4 },
            SpanView { kind: PLAIN, end_byte_index: 27 },
            SpanView { kind: COMMENT, end_byte_index: 34 },
            SpanView { kind: PLAIN, end_byte_index: foo.len()},
        ]
    )
}

#[test]
fn query_spans_for_produces_the_right_result_on_this_less_basic_case() {
    fn span_kind_from_match_example(Match {    
        pattern_index,
        ..
    } : Match) -> SpanKind {    
        match pattern_index {
            0 | 1 => COMMENT,
            2 => STRING,
            _ => PLAIN,
        }
    }

    let foo = 
"fn main() {
let hi = \"hi\";
// TODO
let yo = \"yo\";
}";

    let query_source = "
        (line_comment) @c1
        (block_comment) @c2
        (string_literal) @s
    ";

    let (mut rust, query) = get_rust_parser_and_query(query_source);

    let tree = rust.parse(foo, None);
    
    let spans = query_spans_for(tree.as_ref(), &query, foo, span_kind_from_match_example);

    assert_eq!(
        spans,
        vec![
            SpanView { kind: PLAIN, end_byte_index: 21 },
            SpanView { kind: STRING, end_byte_index: 21 + 4 },
            SpanView { kind: PLAIN, end_byte_index: 27 },
            SpanView { kind: COMMENT, end_byte_index: 34 },
            SpanView { kind: PLAIN, end_byte_index: 44 },
            SpanView { kind: STRING, end_byte_index: 44 + 4 },
            SpanView { kind: PLAIN, end_byte_index: foo.len()},
        ]
    )
}

fn arbitary_span_kind_from_node(node: Node) -> SpanKindSpec {
    SpanKindSpec::Kind(sk!(node.kind_id().to_le_bytes()[0]))
}

fn totally_classified_spans_for_produces_valid_rust_spans_on(code: &str) {
    let tree = get_rust_tree!(code);

    let spans = totally_classified_spans_for(tree.as_ref(), code, arbitary_span_kind_from_node);

    spans_assert!(spans);
}

proptest!{
    #[test]
    #[ignore] // takes a long time
    fn totally_classified_spans_for_produces_valid_rust_spans(
        code in arb::rust_code(SOME_AMOUNT),
    ) {
        totally_classified_spans_for_produces_valid_rust_spans_on(
            &code,
        )
    }
}

#[test]
fn totally_classified_spans_for_produces_valid_rust_spans_on_unit() {
    totally_classified_spans_for_produces_valid_rust_spans_on(
        "()",
    );
}

#[allow(dead_code)]
fn preceding_backslash_count_is_even(s: &str, mut byte_index: usize) -> bool {
    let mut count = 0;

    if byte_index > 0 {
        byte_index -= 1;
        while byte_index > 0 && s.get(byte_index..=byte_index) == Some("\\") {
            count += 1;
            byte_index -= 1;
        }
    }

    count & 1 == 0
}

fn remove_pair_characters_from_embedded_strings_and_chars(code: &str) -> String {
    let mut output = String::with_capacity(code.len());
    
    let mut eating_until: Option<char> = None;

    for (_, c) in code.char_indices() {
        match eating_until {
            None => {
                if "\"'".find(c).is_some() {
                    eating_until = Some(c);
                }
            }
            Some(terminator) => {
                if c == terminator {
                    eating_until = None;
                } else if "\"'".find(c).is_some() {
                    continue;
                }
            }
        }
        output.push(c);
    }

    output
}

proptest!{
    #[test]
    fn remove_pair_characters_from_embedded_strings_and_chars_is_idempotent(
        s in ".*"
    ) {
        let applied_once = remove_pair_characters_from_embedded_strings_and_chars(&s);
        assert_eq!(
            remove_pair_characters_from_embedded_strings_and_chars(&applied_once),
            applied_once
        );
    }
}

#[test]
fn remove_pair_characters_from_embedded_strings_and_chars_works_on_a_backslash_char_literal() {
    assert_eq!(remove_pair_characters_from_embedded_strings_and_chars("'\\'"), "'\\'");
}

fn get_rust_token_pairs(code: &str) -> Vec<(usize, usize)> {
    const PAIRED_TOKENS_IN_N_BYTES_ESTIMATE: usize = 16;
    let cap = code.len() / PAIRED_TOKENS_IN_N_BYTES_ESTIMATE;

    let mut pairs: Vec<(usize, usize)> = Vec::with_capacity(cap / 2);
    let mut stack = Vec::with_capacity(cap);

    // The different character pairs
    for (i, c) in code.char_indices() {
        if "({".find(c).is_some() {
            stack.push((i, c));
        } else if ")}".find(c).is_some() {
            let (prev_i, prev_c) = stack.pop().expect("not enough paired tokens!");

            match (prev_c, c) {
                ('(', ')')|('{', '}') => {
                     pairs.push((prev_i, i));
                }
                _ => {
                    panic!("{:?} was incorrectly looped around {:?}", prev_c, c);
                }
            }
        }
    }

    assert!(stack.len() == 0, "leftover token(s): {:?}", stack);

    // The identical character pairs
    for (i, c) in code.char_indices() {
        if "\"'".find(c).is_some() {
            if let Some(&(prev_i, prev_c)) = stack.last() {
                match (prev_c, c) {
                    ('\'', '\'')|('"', '"') => {
                        stack.pop();
                        pairs.push((prev_i, i));
                    },
                    _ => {
                        stack.push((i, c));
                    }
                }
            } else {
                stack.push((i, c));
            }
        }
    }

    assert!(stack.len() == 0, "leftover token(s): {:?}", stack);
    
    pairs
}

#[test]
fn get_rust_token_pairs_should_work_on_this_identical_wrapped_in_marked_example() {
    let output = get_rust_token_pairs("{'a'}");
    assert_eq!(output.len(), 2);

    assert!(output.contains(&(0, 4)));
    assert!(output.contains(&(1, 3)));
}

const RETURN_BACKSLASH_EXAMPLE: &'static str = "fn bs() -> char{'\\\\'}";

#[test]
fn get_rust_token_pairs_should_work_on_the_return_backslash_example() {
    let output = get_rust_token_pairs(RETURN_BACKSLASH_EXAMPLE);
    assert_eq!(output.len(), 3, "{:?} was expected to have 3 elements", output);

    assert!(output.contains(&(5, 6)));
    assert!(output.contains(&(15, 20)));
    assert!(output.contains(&(16, 19)));
}

fn rust_extra_spans_should_not_give_paired_tokens_different_kinds_on(
    unfiltered_code: &str,
) {
    let code = remove_pair_characters_from_embedded_strings_and_chars(unfiltered_code);

    let pairs = get_rust_token_pairs(&code);
    
    if pairs.len() == 0 {
        return
    }

    let spans = Parsers::default().get_spans_with_previous(code.into(), ParserKind::Rust(Style::Extra), None);
    
    spans_assert!(&spans);

    for (start, end) in pairs.iter() {
        let start_kind = span_for_byte_index(&spans, *start).expect("start was not in spans!").kind;
        let end_kind = span_for_byte_index(&spans, *end).expect("end was not in spans!").kind;
        assert_eq!(
            start_kind,
            end_kind,
            "{:?} != {:?} for byte {} and byte {} in {:?}",
            start_kind,
            end_kind,
            start,
            end, 
            spans,
        );
    }
}

fn span_for_byte_index(spans: &Spans, byte_index: usize) -> Option<SpanView> {
    for span in spans.iter() {
        if span.end_byte_index > byte_index {
            return Some(*span);
        }
    }

    None
}

proptest!{
    #[test]
    #[ignore] // takes a long time
    fn rust_extra_spans_should_not_give_paired_tokens_different_kinds(
        code in arb::rust_code(SOME_AMOUNT),
    ) {
        rust_extra_spans_should_not_give_paired_tokens_different_kinds_on(
            &code,
        );
    }
}

#[test]
fn rust_extra_spans_should_not_give_paired_tokens_different_kinds_on_the_predicate_example() {
    rust_extra_spans_should_not_give_paired_tokens_different_kinds_on(
        PREDICATE_EXAMPLE,
    )
}

#[test]
fn rust_extra_spans_should_not_give_paired_tokens_different_kinds_on_unit() {
    rust_extra_spans_should_not_give_paired_tokens_different_kinds_on(
        "()",
    )
}

// This example came from the proptest after we temporairily added a termination check.
#[test]
fn rust_extra_spans_should_not_give_paired_tokens_different_kinds_on_empty_string() {
    rust_extra_spans_should_not_give_paired_tokens_different_kinds_on(
        "",
    )
}

#[test]
fn rust_extra_spans_should_not_give_paired_tokens_different_kinds_on_the_return_backslash_example() {
    rust_extra_spans_should_not_give_paired_tokens_different_kinds_on(
        RETURN_BACKSLASH_EXAMPLE,
    )
}

#[test]
fn rust_extra_spans_should_not_give_paired_tokens_different_kinds_on_the_return_backslash_example_with_embedded_pairs_removed() {
    rust_extra_spans_should_not_give_paired_tokens_different_kinds_on(
        &dbg!(remove_pair_characters_from_embedded_strings_and_chars(RETURN_BACKSLASH_EXAMPLE)),
    )
}


#[test]
fn rust_extra_spans_should_not_give_paired_tokens_different_kinds_on_this_reduced_char_example() {
    rust_extra_spans_should_not_give_paired_tokens_different_kinds_on(
        // this will become the string with a "'", two backslashes and another "'".
        "'\\\\'",
    )
}

fn rust_extra_spans_produces_valid_spans_on(code: &str) {
    let spans = Parsers::default().get_spans_with_previous(code.into(), ParserKind::Rust(Style::Extra), None);

    spans_assert!(spans);
}

proptest!{
    #[test]
    #[ignore] // Takes a long time
    fn rust_extra_spans_produces_valid_spans(
        code in arb::rust_code(SOME_AMOUNT),
    ) {
        rust_extra_spans_produces_valid_spans_on(
            &code,
        )
    }
}

#[test]
fn rust_extra_spans_produces_valid_spans_on_unit() {
    rust_extra_spans_produces_valid_spans_on(
        "()",
    )
}

// We had a bug that turned out not to be this part.
#[test]
fn parser_kind_iteration_does_not_produce_the_same_thing_twice() {
    let mut seen = Vec::with_capacity(16);
    let kind = ParserKind::default();
    
    seen.push(kind);
    for k in kind {
        dbg!(&k, seen.contains(&k));
        assert!(
            !seen.contains(&k),
            "{:?} already in {:?}",
            k,
            seen
        );
        seen.push(k);
    }
}

const NESTED_COMMENT_EXAMPLE: &'static str = 
"fn main() {
let hi = \"hi there\";
// TODO
}
/*
fn uncommentable_function() {
let _ = \"foo/*.rs\"; //  */\r
}
*/";

fn tree_depth_spans_for_terminates_on(code: &'static str) {
    let tree = get_rust_tree!(code);

    let (send, recv) = std::sync::mpsc::channel();

    std::thread::spawn(move || {
        tree_depth_spans_for(tree.as_ref(), code);
        send.send(()).unwrap();
    });
    
    assert_eq!(
        recv.recv_timeout(std::time::Duration::from_millis(1000)),
        Ok(())
    );
}

// This test failed at least once.
#[test]
fn tree_depth_spans_for_terminates_on_the_nested_comment_tree() {
    tree_depth_spans_for_terminates_on(NESTED_COMMENT_EXAMPLE);
}

const SMALLER_NESTED_COMMENT_EXAMPLE: &'static str = 
"/*
fn uncommentable_function() {
let _ = \"foo/*.rs\"; //  */\r
}
*/";

#[test]
fn tree_depth_spans_for_terminates_on_the_smaller_nested_comment_tree() {
    tree_depth_spans_for_terminates_on(SMALLER_NESTED_COMMENT_EXAMPLE);
}

const HI_THERE_EXAMPLE: &'static str = 
"fn main() {
let hi = \"hi there\";
// TODO
}";

#[test]
fn tree_depth_spans_for_terminates_on_the_hi_there_example_tree() {
    tree_depth_spans_for_terminates_on(HI_THERE_EXAMPLE);
}

#[test]
fn tree_depth_spans_for_terminates_on_fn_main() {
    tree_depth_spans_for_terminates_on("fn main() {}");
}

#[test]
fn tree_depth_spans_for_terminates_on_an_empty_tuple_struct() {
    tree_depth_spans_for_terminates_on("struct A ();");
}

fn tree_depth_spans_for_gets_the_right_answer_for(code: &str, expected_spans: Vec<SpanView>) {
    spans_assert!(&expected_spans, "expected_spans is not valid!");

    let tree = get_rust_tree!(code);

    let spans = tree_depth_spans_for(tree.as_ref(), code);

    spans_assert!(&spans, "tree_depth_spans_for produced invalid spans!");

    assert_eq!(spans, expected_spans);
}

#[test]
#[ignore] // tree_depth_spans correctness was deemed not an immediate priority
fn tree_depth_spans_for_gets_the_right_answer_for_the_nested_comment_tree() {
    recursive_dbg_code!(rust NESTED_COMMENT_EXAMPLE);
    /*
    This is a formatted vesion of what we get from `recursive_dbg_code!`

    (depth: 0, {Node source_file (0, 0) - (8, 2)})
    i = 0
    (depth: 1, {Node function_item (0, 0) - (3, 1)})
    i = 0
    (depth: 2, {Node fn (0, 0) - (0, 2)})
    i = 1
    (depth: 2, {Node identifier (0, 3) - (0, 7)})
    i = 2
    (depth: 2, {Node parameters (0, 7) - (0, 9)})
    i = 0
    (depth: 3, {Node ( (0, 7) - (0, 8)})
    i = 1
    (depth: 3, {Node ) (0, 8) - (0, 9)})
    i = 3
    (depth: 2, {Node block (0, 10) - (3, 1)})
    i = 0
    (depth: 3, {Node { (0, 10) - (0, 11)})
    i = 1
    (depth: 3, {Node let_declaration (1, 4) - (1, 24)})
    i = 0
    (depth: 4, {Node let (1, 4) - (1, 7)})
    i = 1
    (depth: 4, {Node identifier (1, 8) - (1, 10)})
    i = 2
    (depth: 4, {Node = (1, 11) - (1, 12)})
    i = 3
    (depth: 4, {Node string_literal (1, 13) - (1, 23)})
    i = 0
    (depth: 5, {Node " (1, 13) - (1, 14)})
    i = 1
    (depth: 5, {Node " (1, 22) - (1, 23)})
    i = 4
    (depth: 4, {Node ; (1, 23) - (1, 24)})
    i = 2
    (depth: 3, {Node line_comment (2, 4) - (2, 11)})
    i = 3
    (depth: 3, {Node } (3, 0) - (3, 1)})
    i = 1
    (depth: 1, {Node block_comment (4, 0) - (8, 2)})    */

    tree_depth_spans_for_gets_the_right_answer_for(
        NESTED_COMMENT_EXAMPLE,
        vec![
            SpanView { kind: sk!(2), end_byte_index: 7 },
            SpanView { kind: sk!(3), end_byte_index: 9 },
            SpanView { kind: sk!(2), end_byte_index: 10 },
            SpanView { kind: sk!(3), end_byte_index: 11 + 4 },
            SpanView { kind: sk!(4), end_byte_index: 11 + 12 },
            SpanView { kind: sk!(5), end_byte_index: 11 + 23 },
            SpanView { kind: sk!(4), end_byte_index: 11 + 24 },
            SpanView { kind: sk!(2), end_byte_index: 36 + 4 },
            SpanView { kind: sk!(3), end_byte_index: 36 + 11 },
            SpanView { kind: sk!(2), end_byte_index: 47 },
            SpanView { kind: sk!(3), end_byte_index: 47 + 1 },
            SpanView { kind: sk!(1), end_byte_index: NESTED_COMMENT_EXAMPLE.len() },
        ]
    );
}

fn tree_depth_spans_for_produces_valid_rust_spans_on(code: &str) {
    let tree = get_rust_tree!(code);

    let spans = tree_depth_spans_for(tree.as_ref(), &code);

    spans_assert!(spans);
}

proptest!{
    #[test]
    #[ignore] // takes a long time
    fn tree_depth_spans_for_produces_valid_rust_spans(
        code in arb::rust_code(SOME_AMOUNT)
    ) {
        tree_depth_spans_for_produces_valid_rust_spans_on(&code);
    }
}

#[test]
fn tree_depth_spans_for_produces_valid_rust_spans_in_this_generated_case() {
    tree_depth_spans_for_produces_valid_rust_spans_on(
        "fn r#A() -> bool{false}"
    );
}

#[test]
fn tree_depth_spans_for_produces_valid_rust_spans_in_this_predicate_case() {
    tree_depth_spans_for_produces_valid_rust_spans_on(
        PREDICATE_EXAMPLE
    );
}

#[test]
fn tree_depth_spans_for_produces_valid_rust_spans_in_this_incomplete_case() {
    tree_depth_spans_for_produces_valid_rust_spans_on(
        "fn"
    );
}

#[test]
fn tree_depth_spans_for_produces_valid_rust_spans_in_this_larger_incomplete_case() {
    tree_depth_spans_for_produces_valid_rust_spans_on(
        "fn a"
    );
}

const MINIMAL_ITEM_CASE: &'static str = "struct A;";

#[test]
fn tree_depth_spans_for_produces_valid_rust_spans_in_this_minimal_case() {
    tree_depth_spans_for_produces_valid_rust_spans_on(
        MINIMAL_ITEM_CASE
    );
}

#[test]
fn tree_depth_spans_for_produces_valid_rust_spans_on_empty_string() {
    tree_depth_spans_for_produces_valid_rust_spans_on(
        ""
    );
}

#[test]
fn tree_depth_spans_for_gets_the_right_answer_for_this_minimal_case() {
    tree_depth_spans_for_gets_the_right_answer_for(
        MINIMAL_ITEM_CASE,
        vec![
            SpanView { kind: sk!(2), end_byte_index: 9 },
        ]
    );
}

const PREDICATE_EXAMPLE: &'static str = "fn no() -> bool{false}";

#[test]
fn tree_depth_spans_for_gets_the_right_answer_for_this_predicate_case() {
    tree_depth_spans_for_gets_the_right_answer_for(
        PREDICATE_EXAMPLE,
        vec![
            SpanView { kind: sk!(2), end_byte_index: 5 },
            SpanView { kind: sk!(3), end_byte_index: 7 },
            SpanView { kind: sk!(2), end_byte_index: 15 },
            SpanView { kind: sk!(3), end_byte_index: 16 },
            SpanView { kind: sk!(4), end_byte_index: 21 },
            SpanView { kind: sk!(3), end_byte_index: 22 },
        ]
    );
}

type Collectings = (Depth, usize, &'static str);

fn collecting_depth_first_produces_the_expected_answer_on(
    code: &str,
    expected_spans: Vec<Collectings>
) {
    let tree = get_rust_tree!(code);

    let collected: Vec<_> = DepthFirst::new(tree.as_ref())
        .map(|(depth, node)| (depth, node.end_byte(), node.kind()))
        .collect();

    assert_eq!(
        collected,
        expected_spans
    );
}

#[test]
fn collecting_depth_first_produces_the_expected_answer_on_this_minimal_case() {
    collecting_depth_first_produces_the_expected_answer_on(
        MINIMAL_ITEM_CASE,
        vec![
            (0, 9, "source_file"),
            (1, 9, "struct_item"),
            (2, 6, "struct"),
            (2, 8, "type_identifier"),
            (2, 9, ";"),
        ]
    );
}

#[test]
fn collecting_depth_first_produces_the_expected_answer_on_this_predicate_case() {
    collecting_depth_first_produces_the_expected_answer_on(
        PREDICATE_EXAMPLE,
        vec![
            (0, 22, "source_file"),
            (1, 22, "function_item"),
            (2,  2, "fn"),
            (2,  5, "identifier"),
            (2,  7, "parameters"),
            (3,  6, "("),
            (3,  7, ")"),
            (2, 10, "->"),
            (2, 15, "primitive_type"),
            (2, 22, "block"),
            (3, 16, "{"),
            (3, 21, "boolean_literal"),
            (4, 21, "false"),
            (3, 22, "}"),
        ]
    );
}

// This test failed at least once.
#[test]
fn depth_first_terminates_on_the_nested_comment_tree() {
    let tree = get_rust_tree!(NESTED_COMMENT_EXAMPLE);

    let (send, recv) = std::sync::mpsc::channel();

    std::thread::spawn(move || {
        // Do something so we are sure this isn't optimized out.
        let mut dummy_count = 0;
        for e in DepthFirst::new(tree.as_ref()) {
            dummy_count += e.0;
        }
        dbg!(dummy_count);
        send.send(()).unwrap();
    });
    
    assert_eq!(
        recv.recv_timeout(std::time::Duration::from_millis(16000)),
        Ok(())
    );
}

fn tree_depth_extract_sorted_produces_sorted_rust_spans_on(code: &str) {
    let tree = get_rust_tree!(code);

    let mut spans = Vec::with_capacity(1024);

    tree_depth_extract_sorted(tree.as_ref(), &mut spans);

    spans_assert!(
        spans,
        "tree_depth_extract_sorted did not produce sorted spans!",
        skip kind
    );
}

proptest!{
    #[test]
    #[ignore] //takes a long time, probably due to our slow code
    fn tree_depth_extract_sorted_produces_sorted_rust_spans(
        code in arb::rust_code(SOME_AMOUNT)
    ) {
        tree_depth_extract_sorted_produces_sorted_rust_spans_on(&code);
    }
}

#[test]
fn tree_depth_extract_sorted_produces_sorted_rust_spans_on_the_nested_comment_example() {
    tree_depth_extract_sorted_produces_sorted_rust_spans_on(NESTED_COMMENT_EXAMPLE);
}

#[test]
fn dedup_by_kind_keeping_last_in_this_case_with_three_in_a_row() {
    let mut spans = vec![
        SpanView {
            end_byte_index: 5,
            kind: d!()
        },
        SpanView {
            end_byte_index: 6,
            kind: d!()
        },
        SpanView {
            end_byte_index: 7,
            kind: d!()
        },
    ];

    dedup_by_kind_keeping_last(&mut spans);

    assert_eq!(
        spans,
        vec![
            SpanView {
                end_byte_index: 7,
                kind: d!()
            }
        ]
    );
}

#[test]
fn filter_spans_produces_valid_spans_in_this_three_in_a_row_case() {
    let mut spans = vec![
        SpanView {
            end_byte_index: 5,
            kind: sk!(5)
        },
        SpanView {
            end_byte_index: 5,
            kind: sk!(6)
        },
        SpanView {
            end_byte_index: 5,
            kind: sk!(7)
        },
    ];

    filter_spans(&mut spans);

     spans_assert!(spans);
}