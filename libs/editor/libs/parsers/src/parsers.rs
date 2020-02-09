#![deny(unused_variables)]
use macros::{d, fmt_debug};
use platform_types::{SpanView, SpanKind, sk};

use tree_sitter::{Parser, Language, LanguageError, Node, Query, QueryCapture, QueryCursor, QueryError, Tree, TreeCursor};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Style {
    Basic,
    TreeDepth
}
d!(for Style: Style::Basic);

impl Iterator for Style {
    type Item = Style;

    fn next(&mut self) -> Option<Self::Item> {
        use Style::*;
        match self {
            Basic => {
                Some(TreeDepth)
            },
            TreeDepth => {
                None
            }
        }.map(|s| {
            *self = s;
            s
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ParserKind {
    Plaintext,
    Rust(Style)
}
d!(for ParserKind: ParserKind::Plaintext);

impl Iterator for ParserKind {
    type Item = ParserKind;

    fn next(&mut self) -> Option<Self::Item> {
        use ParserKind::*;
        match self {
            Plaintext => {
                Some(Rust(d!()))
            },
            Rust(style) => {
                style.next()
                    .map(Rust)
            }
        }.map(|p| {
            *self = p;
            p
        })
    }
}

#[derive(Debug)]
pub enum Parsers {
    NotInitializedYet,
    Initialized(InitializedParsers),
    FailedToInitialize(InitializationError)
}
d!(for Parsers: Parsers::NotInitializedYet);

pub struct InitializationError(String);
fmt_debug!(for InitializationError: InitializationError(e) in "{:?}", e);

impl From<LanguageError> for InitializationError {
    fn from(error: LanguageError) -> Self {
        dbg!(&error);
        InitializationError(format!("{:?}", error))
    }
}

impl From<QueryError> for InitializationError {
    fn from(error: QueryError) -> Self {
        dbg!(&error);
        InitializationError(format!("{:?}", error))
    }
}

pub struct InitializedParsers {
    rust: Parser,
    rust_tree: Option<Tree>,
    rust_query: Query,
}
impl std::fmt::Debug for InitializedParsers {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("InitializedParsers")
           .field("rust", &("TODO rust: Parser Debug".to_string()))
           .field("rust_tree", &self.rust_tree)
           .field("rust_query", &self.rust_query)
           .finish()
    }
}

type Spans = Vec<SpanView>;

impl Parsers {
    pub fn get_spans(&mut self, to_parse: &str, kind: ParserKind) -> Spans {
        use Parsers::*;
        self.attempt_init();

        match self {
            Initialized(p) => p.get_spans(to_parse, kind),
            NotInitializedYet | FailedToInitialize(_) => {
                plaintext_spans_for(to_parse)
            }
        }
    }
}
impl InitializedParsers {
    fn get_spans(&mut self, to_parse: &str, kind: ParserKind) -> Spans {
        use ParserKind::*;
        use Style::*;
        match kind {
            Plaintext => {
                plaintext_spans_for(to_parse)
            }
            Rust(style) => {
                // TODO edit the tree properly so and pass it down so we get faster parses
                self.rust_tree = self.rust.parse(to_parse, None);

                match style {
                    Basic => {
                        query_spans_for(
                            self.rust_tree.as_ref(),
                            &self.rust_query,
                            to_parse,
                            rust_span_kind_from_match
                        )
                    },
                    TreeDepth => {
                        tree_depth_spans_for(
                            self.rust_tree.as_ref(),
                            to_parse
                        )
                    }
                }
                
            }
        }
    }
}

struct Match<'capture> {
    pattern_index: usize,
    capture: QueryCapture<'capture>
}

fn query_spans_for<'to_parse>(
    tree: Option<&Tree>,
    query: &Query,
    to_parse: &'to_parse str,
    span_kind_from_match: fn(Match) -> SpanKind,
) -> Spans {
    let mut spans = Vec::with_capacity(1024);

    let mut query_cursor = QueryCursor::new();

    if let Some(r_t) = tree {
        let text_callback = |n: Node| {
            let r = n.range();
            &to_parse[r.start_byte..r.end_byte]
        };

        let mut span_stack = Vec::with_capacity(16);

        for q_match in query_cursor.matches(
            query,
            r_t.root_node(),
            text_callback,
        ) {
            let pattern_index: usize = q_match.pattern_index;

            for q_capture in q_match.captures.iter() {
                let node = q_capture.node;
                let kind = span_kind_from_match(Match {
                    pattern_index,
                    capture: *q_capture
                });

                macro_rules! get_prev {
                    () => {
                        span_stack.last().cloned().unwrap_or_else(|| SpanView {
                            kind: SpanKind::PLAIN,
                            end_byte_index: 0xFFFF_FFFF_FFFF_FFFF
                        })
                    }
                }

                if get_prev!().end_byte_index <= node.start_byte() {
                    if let Some(s) = span_stack.pop() {
                        spans.push(s);
                    }
                }

                spans.push(SpanView {
                    end_byte_index: node.start_byte(),
                    kind: get_prev!().kind,
                });

                span_stack.push(SpanView {
                    end_byte_index: node.end_byte(),
                    kind,
                });
            }
        }

        while let Some(s) = span_stack.pop() {
            spans.push(s);
        }

        let len = to_parse.len();
        if spans.last().map(|s| s.end_byte_index < len).unwrap_or(true) {
            spans.push(SpanView {
                end_byte_index: len,
                kind: SpanKind::PLAIN,
            });
        }
    }

    spans
}

type Depth = u8;

fn tree_depth_spans_for<'to_parse>(
    tree: Option<&Tree>,
    _to_parse: &'to_parse str,
) -> Spans {
    let mut spans: Spans = Vec::with_capacity(1024);

    for (depth, node) in DepthFirst::new(tree) {
        let new_end_byte_index = node.end_byte();
        let new = SpanView {
            kind: sk!(depth),
            end_byte_index: new_end_byte_index
        };

        if let Some(prev) = spans.pop() {
            dbg!(&prev, &new);
            if prev.end_byte_index >= new_end_byte_index {
                dbg!("prev.end_byte_index >= new_end_byte_index");
                spans.push(new);
                if prev.end_byte_index != new_end_byte_index {
                    spans.push(prev);
                }
            } else {
                spans.push(prev);
                spans.push(new);
            }
        } else {
            spans.push(new);
        }
    }

    spans
}

struct DepthFirst<'tree> {
    depth: Depth,
    cursor: Option<TreeCursor<'tree>>,
}

impl <'tree> DepthFirst<'tree> {
    fn new(tree: Option<&'tree Tree>) -> Self {
        DepthFirst {
            depth: 0,
            cursor: tree.map(|t| t.walk()),
        }
    }
}

impl <'tree> Iterator for DepthFirst<'tree> {
    type Item = (Depth, Node<'tree>);

    fn next(&mut self) -> Option<Self::Item> {
        let mut done = false;
        let depth = &mut self.depth;
        let output = self.cursor.as_mut().and_then(|cursor| {
            let output = Some((*depth, cursor.node()));
            dbg!(&output);
            
            'outer: loop {
                if cursor.goto_first_child() {
                    *depth = depth.wrapping_add(1);
                    break output;
                } else {                    while !cursor.goto_next_sibling() {
                        if cursor.goto_parent() {
                            *depth = depth.wrapping_sub(1);
                            continue;
                        } else {
                            done = true;
                            break 'outer output;
                        }
                    }
                }
            }
        });

        if done {
            self.cursor = None;
        }

        output
    }
}

fn plaintext_spans_for(s: &str) -> Spans {
    vec![plaintext_end_span_for(s)]
}

fn plaintext_end_span_for(s: &str) -> SpanView {
    SpanView { kind: SpanKind::PLAIN, end_byte_index: s.len()}
}

extern "C" { fn tree_sitter_rust() -> Language; }

impl Parsers {
    fn attempt_init(&mut self) {
        use Parsers::*;
        match self {
            NotInitializedYet => {
                match InitializedParsers::new() {
                    Ok(p) => { *self = Initialized(p); }
                    Err(e) => { *self = FailedToInitialize(e); }
                }
            },
            Initialized(_) | FailedToInitialize(_) => {}
        }
    }
}

impl InitializedParsers {
    fn new() -> Result<Self, InitializationError> {
        let mut rust = Parser::new();
        let rust_lang = unsafe { tree_sitter_rust() };
        rust.set_language(
            rust_lang
        )?;

        let rust_tree: Option<Tree> = None;

        let rust_query = Query::new(
            rust_lang,
            RUST_QUERY_SOURCE
        )?;

        Ok(InitializedParsers {
            rust,
            rust_tree,
            rust_query,
        })
    }
}

const RUST_QUERY_SOURCE: &'static str = "
(line_comment) @c1
(block_comment) @c2
(string_literal) @s
";

fn rust_span_kind_from_match(Match {    
    pattern_index,
    capture: _
} : Match) -> SpanKind {
    match pattern_index {
        0 | 1 => SpanKind::COMMENT,
        2 => SpanKind::STRING,
        _ => SpanKind::PLAIN,
    }
    
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::proptest;

    const SOME_AMOUNT: usize = 16;

    mod arb {
        use proptest::{collection, sample, prelude::{Strategy, any}};
        pub use pub_arb::rust_code;

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

    fn get_rust_parser() -> Parser {
        let mut rust = Parser::new();
        let rust_lang = unsafe { tree_sitter_rust() };
        rust.set_language(rust_lang).unwrap();

        rust
    }

    macro_rules! spans_assert {
        ($spans: expr) => {
            spans_assert!($spans, "");
        };
        ($spans: expr, $suffix: expr) => {
            let spans = $spans;

            let mut previous_kind = None;
            let mut previous_end_byte_index = 0;
            for (i, s) in spans.clone().into_iter().enumerate() {
                if let Some(prev) = previous_kind {
                    assert_ne!(
                        s.kind,
                        prev,
                        "at index {} in spans was {:?} which as the same as the previous span: {:?}", 
                        i,
                        s,
                        spans,
                    );
                }
                previous_kind = Some(s.kind);

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
                SpanView { kind: PLAIN, end_byte_index: 25 },
                SpanView { kind: STRING, end_byte_index: 29 },
                SpanView { kind: PLAIN, end_byte_index: 35 },
                SpanView { kind: COMMENT, end_byte_index: 42 },
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
                SpanView { kind: PLAIN, end_byte_index: 25 },
                SpanView { kind: STRING, end_byte_index: 29 },
                SpanView { kind: PLAIN, end_byte_index: 35 },
                SpanView { kind: COMMENT, end_byte_index: 42 },
                SpanView { kind: PLAIN, end_byte_index: 56 },
                SpanView { kind: STRING, end_byte_index: 60 },
                SpanView { kind: PLAIN, end_byte_index: foo.len()},
            ]
        )
    }

    // We had a bug that turned out not to be this part.
    #[test]
    fn parser_kind_iteration_does_not_produce_the_same_thing_twice() {
        let mut seen = Vec::with_capacity(16);
        let mut kind = ParserKind::default();
        
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

    // This test failed at least once.
    #[test]
    fn tree_depth_spans_for_terminates_on_the_nested_comment_tree() {
        let mut rust = get_rust_parser();

        let tree = rust.parse(NESTED_COMMENT_EXAMPLE, None);

        let (send, recv) = std::sync::mpsc::channel();

        std::thread::spawn(move || {
            tree_depth_spans_for(tree.as_ref(), NESTED_COMMENT_EXAMPLE);
            send.send(()).unwrap();
        });
        
        assert_eq!(
            recv.recv_timeout(std::time::Duration::from_millis(16)),
            Ok(())
        );
    }

    fn get_rust_tree(code: &str) -> Option<Tree> {
        let mut rust = get_rust_parser();

        rust.parse(code, None)
    }

    fn tree_depth_spans_for_gets_the_right_answer_for(code: &str, expected_spans: Vec<SpanView>) {
        spans_assert!(&expected_spans, "expected_spans is not valid!");

        let tree = get_rust_tree(code);

        let spans = tree_depth_spans_for(tree.as_ref(), code);

        spans_assert!(&spans, "tree_depth_spans_for produced invalid spans!");

        assert_eq!(spans, expected_spans);
    }

    #[test]
    fn tree_depth_spans_for_gets_the_right_answer_for_the_nested_comment_tree() {
        /*
        This is what we get from the tree-sitter playground 
        (https://tree-sitter.github.io/tree-sitter/playground)
        for the NESTED_COMMENT_EXAMPLE.

        source_file [0, 0] - [9, 0])
          function_item [0, 0] - [3, 1])
            name: identifier [0, 3] - [0, 7])
            parameters: parameters [0, 7] - [0, 9])
            body: block [0, 10] - [3, 1])
              let_declaration [1, 4] - [1, 24])
                pattern: identifier [1, 8] - [1, 10])
                value: string_literal [1, 13] - [1, 23])
              line_comment [2, 4] - [2, 11])
          block_comment [4, 0] - [8, 2])
        */

        tree_depth_spans_for_gets_the_right_answer_for(
            NESTED_COMMENT_EXAMPLE,
            vec![
                SpanView { kind: sk!(1), end_byte_index: 3 },
                SpanView { kind: sk!(2), end_byte_index: 10 },
                SpanView { kind: sk!(3), end_byte_index: 11 + 8 },
                SpanView { kind: sk!(4), end_byte_index: 11 + 10 },
                SpanView { kind: sk!(3), end_byte_index: 11 + 13 },
                SpanView { kind: sk!(4), end_byte_index: 11 + 23 },
                SpanView { kind: sk!(3), end_byte_index: 11 + 25 },
                SpanView { kind: sk!(2), end_byte_index: 36 + 4 },
                SpanView { kind: sk!(3), end_byte_index: 36 + 11 },
                SpanView { kind: sk!(0), end_byte_index: 47 },
                SpanView { kind: sk!(1), end_byte_index: NESTED_COMMENT_EXAMPLE.len() },
            ]
        );
    }

    fn tree_depth_spans_for_produces_valid_rust_spans_on(code: &str) {
        let tree = get_rust_tree(code);
    
        let spans = tree_depth_spans_for(tree.as_ref(), &code);

        spans_assert!(spans);
    }

    proptest!{
        #[test]
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
    fn tree_depth_spans_for_gets_the_right_answer_for_this_minimal_case() {
        tree_depth_spans_for_gets_the_right_answer_for(
            MINIMAL_ITEM_CASE,
            vec![
                SpanView { kind: sk!(2), end_byte_index: 6 },
                SpanView { kind: sk!(1), end_byte_index: 9 },
            ]
        );
    }

    #[test]
    fn collecting_depth_first_produces_the_expected_answer_on_this_minimal_case() {
        let tree = get_rust_tree(MINIMAL_ITEM_CASE);

        let collected: Vec<_> = DepthFirst::new(tree.as_ref())
            .map(|(depth, node)| (depth, node.end_byte(), node.kind()))
            .collect();

        assert_eq!(
            collected,
            vec![
                (0, 9, "source_file"),
                (1, 9, "struct_item"),
                (2, 6, "struct"),
            ]
        );
    }
}

