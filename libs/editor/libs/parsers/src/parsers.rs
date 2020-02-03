#![deny(unused_variables)]
use macros::{d, fmt_debug};
use platform_types::{SpanView, SpanKind};

use tree_sitter::{Parser, Language, LanguageError, Node, Query, QueryCapture, QueryCursor, QueryError, Tree};

#[derive(Clone, Copy, Debug)]
pub enum ParserKind {
    Plaintext,
    Rust
}
d!(for ParserKind: ParserKind::Plaintext);

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
        match kind {
            Plaintext => {
                plaintext_spans_for(to_parse)
            }
            Rust => {
                // TODO edit the tree properly so and pass it down so we get faster parses
                self.rust_tree = self.rust.parse(to_parse, None);

                spans_for(
                    self.rust_tree.as_ref(),
                    &self.rust_query,
                    to_parse,
                    rust_span_kind_from_match
                )
            }
        }
    }
}

struct Match<'capture> {
    pattern_index: usize,
    capture: QueryCapture<'capture>
}

fn spans_for<'to_parse>(
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
                            kind: SpanKind::Plain,
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
                kind: SpanKind::Plain,
            });
        }
    }

    spans
}

fn plaintext_spans_for(s: &str) -> Spans {
    vec![plaintext_end_span_for(s)]
}

fn plaintext_end_span_for(s: &str) -> SpanView {
    SpanView { kind: SpanKind::Plain, end_byte_index: s.len()}
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
    use SpanKind::*;
    match pattern_index {
        0 | 1 => Comment,
        2 => String,
        _ => Plain,
    }
    
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn spans_for_produces_the_right_result_on_this_multiple_match_case() {
        use SpanKind::*;
        const OUTER: SpanKind = Comment;
        const INNER: SpanKind = String;
        const OTHER: SpanKind = Plain;
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
        
        let spans = spans_for(tree.as_ref(), &query, foo, span_kind_from_match_example);

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
    fn spans_for_produces_the_right_result_on_this_basic_case() {
        use SpanKind::*;
        fn span_kind_from_match_example(Match {    
            pattern_index,
            ..
        } : Match) -> SpanKind {    
            match pattern_index {
                0 | 1 => Comment,
                2 => String,
                _ => Plain,
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
        
        let spans = spans_for(tree.as_ref(), &query, foo, span_kind_from_match_example);

        assert_eq!(
            spans,
            vec![
                SpanView { kind: Plain, end_byte_index: 25 },
                SpanView { kind: String, end_byte_index: 29 },
                SpanView { kind: Plain, end_byte_index: 35 },
                SpanView { kind: Comment, end_byte_index: 42 },
                SpanView { kind: Plain, end_byte_index: foo.len()},
            ]
        )
    }

    #[test]
    fn spans_for_produces_the_right_result_on_this_less_basic_case() {
        use SpanKind::*;
        fn span_kind_from_match_example(Match {    
            pattern_index,
            ..
        } : Match) -> SpanKind {    
            match pattern_index {
                0 | 1 => Comment,
                2 => String,
                _ => Plain,
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
        
        let spans = spans_for(tree.as_ref(), &query, foo, span_kind_from_match_example);

        assert_eq!(
            spans,
            vec![
                SpanView { kind: Plain, end_byte_index: 25 },
                SpanView { kind: String, end_byte_index: 29 },
                SpanView { kind: Plain, end_byte_index: 35 },
                SpanView { kind: Comment, end_byte_index: 42 },
                SpanView { kind: Plain, end_byte_index: 56 },
                SpanView { kind: String, end_byte_index: 60 },
                SpanView { kind: Plain, end_byte_index: foo.len()},
            ]
        )
    }
}

