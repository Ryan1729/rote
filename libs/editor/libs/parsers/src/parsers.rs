#![deny(unused_variables)]
use macros::{d, fmt_debug};
use platform_types::{SpanView, SpanKind};

use tree_sitter::{Parser, Language, LanguageError, Node, Query, QueryCursor, QueryError, Tree};

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
    rust_comment_query: Query,
    rust_string_query: Query,
}
impl std::fmt::Debug for InitializedParsers {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("InitializedParsers")
           .field("rust", &("TODO rust: Parser Debug".to_string()))
           .field("rust_tree", &self.rust_tree)
           .field("rust_comment_query", &self.rust_comment_query)
           .field("rust_string_query", &self.rust_string_query)
           .finish()
    }
}

type Spans = Vec<SpanView>;

/// How many times more common plain nodes are than the other typs of nodes.
const PLAIN_NODES_RATIO: usize = 4;

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
        match dbg!(kind) {
            Plaintext => {
                plaintext_spans_for(to_parse)
            }
            Rust => {
                dbg!("pre rust_spans_for");
                self.rust_spans_for(to_parse)
            }
        }
    }
}

#[derive(Debug)]
struct SpanNodes<'tree> {
    comment_nodes: Vec<&'tree Node<'tree>>,
    string_nodes: Vec<&'tree Node<'tree>>,
}

impl InitializedParsers {
    fn rust_spans_for<'to_parse>(&'to_parse mut self, to_parse: &'to_parse str) -> Spans {
        dbg!("in rust_spans_for");
        self.rust_tree = dbg!(self.rust.parse(to_parse, self.rust_tree.as_ref()));

        let mut comment_nodes = Vec::new();

        let mut string_nodes = Vec::new();

        let mut query_cursor = QueryCursor::new();

        if let Some(r_t) = &self.rust_tree {
            let text_callback = move |n: Node| {
                let r = n.range();
                &to_parse[r.start_byte..r.end_byte]
            };

            let (_, _): (Vec<_>, Vec<_>) = dbg!(
                query_cursor.matches(
                    &self.rust_comment_query,
                    r_t.root_node(),
                    text_callback,
                ).map(|m: tree_sitter::QueryMatch| m.pattern_index).collect(),
                query_cursor.matches(
                    &self.rust_string_query,
                    r_t.root_node(),
                    text_callback,
                ).map(|m: tree_sitter::QueryMatch| m.pattern_index).collect(),
            );

            string_nodes = query_cursor.matches(
                &self.rust_string_query,
                r_t.root_node(),
                text_callback,
            ).flat_map(|q_match| {
                dbg!(q_match.pattern_index);
                q_match.captures.iter().map(|q_capture| &q_capture.node)
            }).collect();

            comment_nodes = query_cursor.matches(
                &self.rust_comment_query,
                r_t.root_node(),
                text_callback,
            ).flat_map(|q_match| 
                q_match.captures.iter().map(|q_capture| &q_capture.node)
            ).collect();
        }

        dbg!(get_spans_from_nodes(
            to_parse,
            SpanNodes {
                comment_nodes,
                string_nodes,
            }
        ))
    }
}

fn plaintext_spans_for(s: &str) -> Spans {
    vec![plaintext_end_span_for(s)]
}

fn plaintext_end_span_for(s: &str) -> SpanView {
    SpanView { kind: SpanKind::Plain, end_byte_index: s.len()}
}

fn get_spans_from_nodes<'tree>(
    to_parse: &'tree str,
    SpanNodes {
        comment_nodes,
        string_nodes,
    }: SpanNodes<'tree>
) -> Spans {
    use SpanKind::*;

    let mut comment_nodes = comment_nodes.iter();
    let mut string_nodes = string_nodes.iter();

    let mut spans = Vec::with_capacity(
        (comment_nodes.len() + string_nodes.len())
        * (PLAIN_NODES_RATIO + 1)
    );

    let mut comment = comment_nodes.next();
    let mut string = string_nodes.next();
    let mut previous_end_byte_index = 0;
    loop {
        if let Some(s) = string.as_ref() {
            dbg!("before s.kind_id()");
            s.kind_id();
            dbg!("after s.kind_id()");
            println!("{{Node {} {} - {}}}", s.kind(), s.kind(), s.kind());
            dbg!(s);
        }
        
        //dbg!(&string, &comment);
        let (node, kind) = {
            macro_rules! handle_previous_chunk {
                ($node: ident) => {
                    let start = $node.start_byte();
                    let extra = start.saturating_sub(previous_end_byte_index);
                    if extra > 0 {
                        previous_end_byte_index = start;
                        spans.push(SpanView {
                            kind: SpanKind::Plain,
                            end_byte_index: previous_end_byte_index,
                        });
                        
                    }   
                }
            }
            match (comment, string) {
                (Some(c), Some(s)) => {
                    // TODO reformulate to make adding in the minimum of n node types easier.
                    if c.start_byte() < s.start_byte() {
                        handle_previous_chunk!(c);

                        comment = comment_nodes.next();

                        (c, Comment)
                    } else {
                        handle_previous_chunk!(s);
                        
                        string = string_nodes.next();
                        
                        (s, String)
                    }
                }
                (Some(c), None) => {
                    comment = comment_nodes.next();

                    (c, Comment)
                }
                (None, Some(s)) => {
                    string = string_nodes.next();

                    (s, String)
                }
                (None, None) => break,
            }
        };

        dbg!(node.end_byte(), kind);
        // this is briefly the current index
        previous_end_byte_index = node.end_byte();
        spans.push(SpanView {
            kind,
            end_byte_index: previous_end_byte_index,
        });
    }

    if previous_end_byte_index < to_parse.len() {
        spans.push(plaintext_end_span_for(to_parse));
    }

    spans
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

        let rust_comment_query = Query::new(
            rust_lang,
            "(line_comment) @c1
            (block_comment) @c2"
        )?;

        let rust_string_query = Query::new(
            rust_lang,
            "(string_literal) @s"
        )?;

        Ok(InitializedParsers {
            rust,
            rust_tree,
            rust_comment_query,
            rust_string_query,
        })
    }
}

