#![deny(unused_variables)]
use macros::{d, fmt_debug};
use platform_types::{SpanView, SpanKind, sk};

use tree_sitter::{Parser, Language, LanguageError, Node, Query, QueryCapture, QueryCursor, QueryError, Tree, TreeCursor};

use std::borrow::Cow;

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum Style {
    Extra,
    Basic,
    TreeDepth
}
d!(for Style: Style::Extra);

impl Iterator for Style {
    type Item = Style;

    fn next(&mut self) -> Option<Self::Item> {
        use Style::*;
        match self {
            Extra => {
                Some(Basic)
            },
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

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
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
    rust_basic_query: Query,
}
impl std::fmt::Debug for InitializedParsers {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("InitializedParsers")
           .field("rust", &("TODO rust: Parser Debug".to_string()))
           .field("rust_tree", &self.rust_tree)
           .field("rust_basic_query", &self.rust_basic_query)
           .finish()
    }
}

type Spans = Vec<SpanView>;

type ToParse<'a> = Cow<'a, str>;

impl Parsers {
    pub fn get_spans(&mut self, to_parse: ToParse<'_>, kind: ParserKind) -> Spans {
        // TODO edit the tree properly as we go, and pass it down so we get faster parses
        self.get_spans_with_previous(to_parse, kind, None)
    }

    pub fn get_spans_with_previous(&mut self, to_parse: ToParse<'_>, kind: ParserKind, previous: Option<&Tree>) -> Spans {
        use Parsers::*;
        self.attempt_init();

        match self {
            Initialized(p) => {
                p.get_spans_with_previous(to_parse, kind, previous)
            },
            NotInitializedYet | FailedToInitialize(_) => {
                plaintext_spans_for(to_parse)
            }
        }
    }
}
impl InitializedParsers {
    fn get_spans_with_previous(
        &mut self,
        to_parse: ToParse<'_>,
        kind: ParserKind,
        previous: Option<&Tree>
    ) -> Spans {
        use ParserKind::*;
        use Style::*;
        match kind {
            Plaintext => {
                plaintext_spans_for(to_parse)
            }
            Rust(style) => {
                self.rust_tree = self.rust.parse(to_parse.as_ref(), previous);

                match style {
                    Basic => {
                        query_spans_for(
                            self.rust_tree.as_ref(),
                            &self.rust_basic_query,
                            &to_parse,
                            rust_basic_span_kind_from_match
                        )
                    },
                    Extra => {
                        totally_classified_spans_for(
                            self.rust_tree.as_ref(),
                            &to_parse,
                            rust_extra_span_kind_from_node
                        )
                    },
                    TreeDepth => {
                        tree_depth_spans_for(
                            self.rust_tree.as_ref(),
                            &to_parse
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

fn get_spans_capacity(to_parse: &str) -> usize {
    const AVERAGE_BYTES_PER_TOKEN: usize = 4;
    return to_parse.len() / AVERAGE_BYTES_PER_TOKEN;
}

fn query_spans_for<'to_parse>(
    tree: Option<&Tree>,
    query: &Query,
    to_parse: &'to_parse str,
    span_kind_from_match: fn(Match) -> SpanKind,
) -> Spans {
    let mut spans = Vec::with_capacity(get_spans_capacity(
        to_parse
    ));

    let mut span_stack = Vec::with_capacity(16);

    let receive_match = |m: Match| {
        let node = m.capture.node;
        let kind = span_kind_from_match(m);
    
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
    };

    query_spans_for_inner(
        tree,
        query,
        to_parse,
        receive_match
    );

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

    // TODO there is probably a clever way to change the above code so this line,
    // and the associted O(n log n) running time, is unnecessary.
    spans.sort_by(|s1, s2|{
        s1.end_byte_index.cmp(&s2.end_byte_index)
    });

    filter_spans(&mut spans);

    spans
}

fn query_spans_for_inner<'to_parse, F: FnMut(Match)>(
    tree: Option<&Tree>,
    query: &Query,
    to_parse: &'to_parse str,
    mut receive_match: F
) {
    let mut query_cursor = QueryCursor::new();

    if let Some(r_t) = tree {
        let text_callback = |n: Node| {
            let r = n.range();
            &to_parse[r.start_byte..r.end_byte]
        };

        for q_match in query_cursor.matches(
            query,
            r_t.root_node(),
            text_callback,
        ) {
            let pattern_index: usize = q_match.pattern_index;

            for q_capture in q_match.captures.iter() {
                receive_match(Match {
                    pattern_index,
                    capture: *q_capture
                });
            }
        }
    }
}

fn totally_classified_spans_for<'to_parse>(
    tree: Option<&Tree>,
    to_parse: &'to_parse str,
    span_kind_from_node: fn(Node) -> SpanKindSpec,
) -> Spans {
    let mut spans = Vec::with_capacity(get_spans_capacity(
        to_parse
    ));

    let mut drop_until_end_byte = None;

    for (_, node) in DepthFirst::new(tree) {
        use SpanKindSpec::*;

        let end_byte_index = node.end_byte();

        if let Some(end_byte) = drop_until_end_byte {
            if end_byte_index > end_byte {
                drop_until_end_byte = None;
            } else {
                continue;
            }
        }

        match span_kind_from_node(node) {
            DropNode => {}
            Kind(kind) => {
                spans.push(SpanView {
                    end_byte_index,
                    kind,
                });
            }
            KindAndDropBelow(kind) => {
                drop_until_end_byte = Some(end_byte_index);

                spans.push(SpanView {
                    end_byte_index,
                    kind,
                });
            }
        }
    }

    // TODO maybe there's a way to do this without an O(n log n) sort?
    spans.sort_by(|s1, s2|{
        s1.end_byte_index.cmp(&s2.end_byte_index)
    });

    filter_spans(&mut spans);

    spans
}

type Depth = u8;

fn tree_depth_spans_for<'to_parse>(
    tree: Option<&Tree>,
    to_parse: &'to_parse str,
) -> Spans {
    let mut spans = Vec::with_capacity(get_spans_capacity(
        to_parse
    ));

    tree_depth_extract_sorted(tree, &mut spans);

    let len = spans.len();
    if len <= 1 {
        filter_spans(&mut spans);
        return spans;
    }

    // scan backwards getting rid of the ones that are overlapped.
    let mut prev_max = 0;

    for i in (0..(spans.len() - 1)).rev() {
        if spans[i].end_byte_index > prev_max {
            prev_max = spans[i].end_byte_index;
        }
        if spans[i + 1].kind != spans[i].kind {
            if spans[i + 1].end_byte_index <= spans[i].end_byte_index
            {
                if spans[i].end_byte_index >= prev_max {
                    spans.remove(i);
                }
            }

            prev_max = spans[i].end_byte_index;
        }
    }
    
    filter_spans(&mut spans);
    
    spans
}

fn tree_depth_extract_sorted(
    tree: Option<&Tree>,
    spans: &mut Spans,
) {
    for (depth, node) in DepthFirst::new(tree) {
        let new_end_byte_index = node.end_byte();
        let new = SpanView {
            kind: sk!(depth),
            end_byte_index: new_end_byte_index
        };

        if let Some(previous) = spans.pop() {
            if previous.end_byte_index >= new_end_byte_index {
                spans.push(new);
                if previous.end_byte_index != new_end_byte_index {
                    spans.push(previous);
                }
            } else {
                spans.push(previous);
                spans.push(new);
            }
        } else {
            spans.push(new);
        }
    }

    // TODO there is probably a clever way to change the above loop so this line,
    // and the associted O(n log n) running time, is unnecessary.
    spans.sort_by(|s1, s2|{
        s1.end_byte_index.cmp(&s2.end_byte_index)
    });
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
            if cursor.goto_first_child() {
                *depth = depth.wrapping_add(1);
            } else {                while !cursor.goto_next_sibling() {
                    if cursor.goto_parent() {
                        *depth = depth.wrapping_sub(1);
                    } else {
                        done = true;
                        break;
                    }
                }
            }
            output
        });

        if done {
            self.cursor = None;
        }

        output
    }
}

fn filter_spans(spans: &mut Spans) {
    dedup_by_end_byte_keeping_last(spans);
    dedup_by_kind_keeping_last(spans);
    
    for i in (0..spans.len()).rev() {
        if spans[i].end_byte_index == 0 {
            spans.remove(i);
        }
    }
}

fn dedup_by_kind_keeping_last(spans: &mut Spans) {
    let mut write = 0;
    for i in 0..spans.len() {
        let prev_kind = spans[write].kind;
        if prev_kind != spans[i].kind {
            write += 1;
        }
        spans[write] = spans[i];
    }

    spans.truncate(write + 1);
}

fn dedup_by_end_byte_keeping_last(spans: &mut Spans) {
    let mut write = 0;
    for i in 0..spans.len() {
        let prev_end_byte_index = spans[write].end_byte_index;
        if prev_end_byte_index != spans[i].end_byte_index {
            write += 1;
        }
        spans[write] = spans[i];
    }

    spans.truncate(write + 1);
}

fn plaintext_spans_for(s: ToParse<'_>) -> Spans {
    vec![plaintext_end_span_for(s)]
}

fn plaintext_end_span_for(s: ToParse<'_>) -> SpanView {
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

        let rust_basic_query = Query::new(
            rust_lang,
            RUST_BASIC_QUERY_SOURCE
        )?;

        Ok(InitializedParsers {
            rust,
            rust_tree,
            rust_basic_query,
        })
    }
}

const RUST_BASIC_QUERY_SOURCE: &'static str = "
(line_comment) @comment
(block_comment) @comment
(string_literal) @string
(char_literal) @string
";

fn rust_basic_span_kind_from_match(Match {    
    pattern_index,
    capture: _
} : Match) -> SpanKind {
    match pattern_index {
        0 | 1 => SpanKind::COMMENT,
        2 => SpanKind::STRING,
        _ => SpanKind::PLAIN,
    }
}

enum SpanKindSpec {
    DropNode,
    KindAndDropBelow(SpanKind),
    Kind(SpanKind)
}

fn rust_extra_span_kind_from_node(node: Node) -> SpanKindSpec {
    use SpanKindSpec::*;
    match node.kind() {
        "\"" | "'" | "escape_sequence" => {DropNode},
        "token_tree" => {
            KindAndDropBelow(sk!(SpanKind::FIRST_UNASSIGNED_RAW + 1))
        },
        s if s.ends_with("comment") => Kind(SpanKind::COMMENT),
        s if s.starts_with("string") | s.starts_with("char") => {
            Kind(SpanKind::STRING)
        },
        _ => {
            if node.is_named() {
                Kind(sk!(SpanKind::FIRST_UNASSIGNED_RAW))
            } else {
                Kind(SpanKind::PLAIN)
            }
        },
    }
}

#[allow(dead_code)]
fn recursive_dbg(node: Option<Node>) {
    recursive_dbg_helper(node, 0)
}

fn recursive_dbg_helper(node: Option<Node>, mut depth: Depth) {
    if let Some(n) = node {
        dbg!((depth, n));

        depth += 1;
        for i in 0..n.child_count() {
            dbg!(i);
            recursive_dbg_helper(n.child(i), depth);
        }
    }
}

#[allow(unused_macros)]
macro_rules! recursive_dbg_code {
    (rust $code: expr) => {
        let tree = get_rust_tree!($code);
        if let Some(t) = tree {
            recursive_dbg(Some(t.root_node()));
        } else {
            dbg!(tree);
        }
    }
}

#[cfg(test)]
mod tests;

