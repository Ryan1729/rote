#![deny(unused)]
use macros::{d, fmt_debug, fmt_display, u};
use platform_types::{BufferName};
use edit::{Edit};

use tree_sitter::{
    Parser,
    Language,
    LanguageError,
    Query,
    QueryError,
    Tree,
};

use std::{
    borrow::Cow,
    collections::HashMap,
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Style {
    Extra,
    Basic,
    TreeDepth
}
d!(for Style: Style::Extra);

fmt_display!(
    for Style: match s {
        Extra => "extra",
        Basic => "basic",
        TreeDepth => "tree-depth" 
    }
);

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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ParserKind {
    Plaintext,
    Rust(Style)
}
d!(for ParserKind: ParserKind::Plaintext);

fmt_display!(
    for ParserKind: match p {
        Plaintext => "txt".to_string(),
        Rust(s) => format!("rs({})", s),
    }
);

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

fmt_display!(
    for Parsers: match p {
        NotInitializedYet => "---".to_string(),
        Initialized(i_p) => {
            format!("{:?}", i_p)
        },
        FailedToInitialize(InitializationError(e)) => e.to_owned(),
    }
);

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

struct BufferState {
    parser: Parser,
    tree: Option<Tree>,
}

d!{for BufferState: BufferState{
    parser: Parser::new(),
    tree: None,
}}

type ParserMap = HashMap<BufferName, BufferState>;

pub struct InitializedParsers {
    parser_map: ParserMap,
    rust_basic_query: Query,
    rust_lang: Language,
}

impl std::fmt::Debug for InitializedParsers {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("InitializedParsers")
           .field("parser_map", &("TODO parser_map: Debug".to_string()))
           .field("rust_basic_query", &self.rust_basic_query)
           .field("rust_lang", &self.rust_lang)
           .finish()
    }
}

type ToParse<'a> = Cow<'a, str>;

#[derive(Debug)]
pub enum SpanError {
    NotInitializedYet,
    FailedToInitialize,
    ParseReturnedNone(BufferName),
}

fmt_display!{
    for SpanError: match e {
        NotInitializedYet => "No spans because parsers were not initialized yet.".to_string(),
        FailedToInitialize => "No spans because parsers failed to initialize!".to_string(),
        ParseReturnedNone(buffer_id) => {
            format!("No spans because parse of {:?} returned `None`!", buffer_id)
        }
    }
}

impl std::error::Error for SpanError {}

type SpansResult<'to_parse> = Result<Spans, (ToParse<'to_parse>, SpanError)>;

impl Parsers {
    pub fn get_spans(
        &mut self,
        to_parse: ToParse<'_>,
        buffer_name: BufferName,
        kind: ParserKind
    ) -> Spans {
        match self.get_spans_result(
            to_parse,
            buffer_name,
            kind,
        ) {
            Ok(spans) => spans,
            Err((to_parse, err)) => {
                // TODO: Propagate error to the editor view so it can be 
                // displayed to the user.
                eprintln!("{}", err);
                query::plaintext_spans_for(to_parse)
            }
        }
    }

    pub fn get_spans_result<'to_parse>(
        &mut self,
        to_parse: ToParse<'to_parse>,
        buffer_name: BufferName,
        kind: ParserKind
    ) -> SpansResult<'to_parse> {
        u!{Parsers}
        self.attempt_init();

        match self {
            Initialized(p) => {
                p.get_spans(to_parse, buffer_name, kind)
            },
            NotInitializedYet => Err((
                to_parse,
                SpanError::NotInitializedYet
            )),
            FailedToInitialize(_) => Err((
                to_parse,
                SpanError::FailedToInitialize
            )),
        }
    }

    /// This method should be called whenever the ToParse associated with a buffer
    /// is changed, so that we can update the stored parse tree. Updating the parse
    /// tree like this enables significant optimizations.
    pub fn acknowledge_edit(&mut self, _buffer_name: &BufferName, _edit: &Edit) {
        use Parsers::*;

        match self {
            Initialized(_p) => {
                todo!() 
            },
            NotInitializedYet | FailedToInitialize(_) => {
                debug_assert!(false, "acknowledge_edit called on uninitalized Parsers");
            }
        }
    }

    /// This method should be called when a buffer is closed, so we don't waste memory
    /// on storing the parse state for it.
    pub fn remove_buffer_state(&mut self, buffer_name: &BufferName) {
        use Parsers::*;

        match self {
            Initialized(p) => {
                p.parser_map.remove(buffer_name);
            },
            NotInitializedYet | FailedToInitialize(_) => {}
        }
    }
}

fn get_or_init_buffer_state(
    parser_map: &mut ParserMap,
    buffer_name: BufferName,
    kind: ParserKind,
    // TODO pass a reference to all the langs, if/when we support more than one.
    rust_lang: Language, 
) -> &mut BufferState {
    parser_map
        .entry(buffer_name)
        .or_insert_with(|| {
            u!{ParserKind}
            let mut s: BufferState = d!();
            // We can assume that `set_language` will return `Ok` because we should
            // have already tried it once in `InitializedParsers::new`
            match kind {
                Rust(_) => {
                    let _res = s.parser.set_language(rust_lang);
                    debug_assert!(
                        _res.is_ok(),
                        "Failed to set language for {}",
                        kind
                    );
                },
                Plaintext => {},
            }
            s
        })
}

impl InitializedParsers {
    fn get_spans<'to_parse>(
        &mut self,
        to_parse: ToParse<'to_parse>,
        buffer_name: BufferName,
        kind: ParserKind,
    ) -> SpansResult<'to_parse> {
        use ParserKind::*;
        use Style::*;
        match kind {
            Plaintext => {
                Ok(query::plaintext_spans_for(to_parse))
            }
            Rust(style) => {
                let state = get_or_init_buffer_state(
                    &mut self.parser_map,
                    buffer_name.clone(),
                    kind,
                    self.rust_lang,
                );
                state.tree = state.parser.parse(to_parse.as_ref(), state.tree.as_ref());

                if let Some(tree) = state.tree.as_ref() {
                    // TODO stop requiring these query functions to handle None.
                    let tree = Some(tree);
                    Ok(match style {
                        Basic => {
                            query::spans_for(
                                tree,
                                &self.rust_basic_query,
                                &to_parse
                            )
                        },
                        Extra => {
                            query::totally_classified_spans_for(
                                tree,
                                &to_parse,
                            )
                        },
                        TreeDepth => {
                            query::tree_depth_spans_for(
                                tree,
                                &to_parse
                            )
                        }
                    })
                } else {
                    Err((
                        to_parse,
                        SpanError::ParseReturnedNone(buffer_name)
                    ))
                }
            }
        }
    }
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
        let rust_lang = unsafe { tree_sitter_rust() };

        let mut first: BufferState = d!();
        // We make sure that each supported language works at the start
        // so we can assume `set_language` always returns `Ok` afterwards.
        first.parser.set_language(
            rust_lang
        )?;

        let mut parser_map = HashMap::new();
        parser_map.insert(d!(), first);

        let rust_basic_query = Query::new(
            rust_lang,
            RUST_BASIC_QUERY_SOURCE
        )?;

        Ok(InitializedParsers {
            parser_map,
            rust_lang,
            rust_basic_query,
        })
    }
}

const RUST_BASIC_QUERY_SOURCE: &str = "
(line_comment) @comment
(block_comment) @comment
(string_literal) @string
(char_literal) @string
";

mod query {
    use crate::{
        ToParse
    };
    use tree_sitter::{
        Node,
        Query,
        QueryCapture,
        QueryCursor,
        Tree,
        TreeCursor
    };
    pub use platform_types::{SpanView, SpanKind, sk};
    pub type Spans = Vec<SpanView>;

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

    struct Match<'capture> {
        pattern_index: usize,
        capture: QueryCapture<'capture>
    }

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
    
    fn get_spans_capacity(to_parse: &str) -> usize {
        const AVERAGE_BYTES_PER_TOKEN: usize = 4;
        to_parse.len() / AVERAGE_BYTES_PER_TOKEN
    }

    pub fn plaintext_spans_for(s: ToParse<'_>) -> Spans {
        vec![plaintext_end_span_for(s)]
    }
    
    fn plaintext_end_span_for(s: ToParse<'_>) -> SpanView {
        SpanView { kind: SpanKind::PLAIN, end_byte_index: s.len()}
    }

    pub fn spans_for<'to_parse>(
        tree: Option<&Tree>,
        query: &Query,
        to_parse: &'to_parse str,
    ) -> Spans {
        spans_for_inner(
            tree,
            query,
            to_parse,
            // TODO take a parameter that determines what to pass here
            rust_basic_span_kind_from_match,
        )
    }
    
    fn spans_for_inner<'to_parse>(
        tree: Option<&Tree>,
        query: &Query,
        to_parse: &'to_parse str,
        span_kind_from_match: fn(Match) -> SpanKind,
    ) -> Spans {
        let mut spans = Vec::with_capacity(get_spans_capacity(
            to_parse
        ));
    
        let mut span_stack = Vec::with_capacity(16);
    
        let mut receive_match = |m: Match| {
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
    
    pub fn totally_classified_spans_for<'to_parse>(
        tree: Option<&Tree>,
        to_parse: &'to_parse str,
    ) -> Spans {
        totally_classified_spans_for_inner(
            tree,
            to_parse,
            rust_extra_span_kind_from_node,
        )
    }

    fn totally_classified_spans_for_inner<'to_parse>(
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
    
    pub fn tree_depth_spans_for<'to_parse>(
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
                && spans[i].end_byte_index >= prev_max {
                    spans.remove(i);
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
                } else {                    while !cursor.goto_next_sibling() {
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
}
use query::{Spans};

#[cfg(test)]
mod tests;

