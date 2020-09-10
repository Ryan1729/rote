#![deny(unused)]
use macros::{d, dbg, fmt_debug, fmt_display, some_or, u};
use platform_types::{BufferName, Rope, Spans};
use edit::{Change, Edit, RangeEdits};

use tree_sitter::{
    InputEdit,
    Parser,
    Point,
    Language,
    LanguageError,
    Query,
    QueryError,
    Tree,
};

use std::{
    borrow::Cow,
    cmp::{max, min}
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

impl ParserKind {
    pub fn default_from_name(name: &BufferName) -> Self {
        u!{ParserKind}
        match name.get_extension_or_empty() {
            "rs" => Rust(d!()),
            _ => Plaintext,
        }
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
    spans: Option<CachedSpans>,
}

d!{for BufferState: BufferState{
    parser: Parser::new(),
    tree: None,
    spans: None,
}}

struct CachedSpans {
    spans: Spans,
    hash: u64,
}

#[cfg(not(feature = "fast_hash"))]
use std::collections::HashMap;

#[cfg(feature = "fast_hash")]
use fast_hash::Map as HashMap;

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
    #[perf_viz::record]
    pub fn get_spans(
        &mut self,
        to_parse: ToParse<'_>,
        buffer_name: &BufferName,
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
        buffer_name: &BufferName,
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
    #[perf_viz::record]
    pub fn acknowledge_edit(
        &mut self,
        buffer_name: &BufferName,
        kind: ParserKind,
        edit: &Edit,
        rope: &Rope
    ) {
        dbg!("acknowledge_edit");
        use Parsers::*;
        self.attempt_init();

        match self {
            Initialized(initialized) => {
                let buffer_state = get_or_init_buffer_state(
                    &mut initialized.parser_map,
                    buffer_name,
                    kind,
                    initialized.rust_lang,
                );
                dbg!(buffer_state.tree.is_some());
                macro_rules! cont {
                    () => {{
                        dbg!();
                        debug_assert!(false, "bailed via cont");
                        continue
                    }}
                }
                if let Some(tree) = buffer_state.tree.as_mut() {
                    for i in 0..edit.range_edits().len() {
                        let input_edit = {
                            perf_viz::record_guard!("convert edit");
                            let (
                                Change{ old, new },
                                RangeEdits{ delete_range, insert_range }
                            ) = some_or!(edit.read_at(i), cont!());
                        
                            let (start_byte, old_end_byte, new_end_byte) = 
                            match dbg!(delete_range, insert_range) {
                                (Some(del_range), Some(ins_range)) => {
                                    let del_start_byte = rope.char_to_byte(
                                        del_range.range.min()
                                    );
                                    let del_end_byte = rope.char_to_byte(
                                        del_range.range.max()
                                    );
                                    let ins_start_byte = rope.char_to_byte(
                                        ins_range.range.min()
                                    );
                                    let ins_end_byte = ins_start_byte
                                        .map(|b| b + ins_range.chars.len());
    
                                    debug_assert_eq!(
                                        del_start_byte,
                                        ins_start_byte
                                    );
                                    (del_start_byte, del_end_byte, ins_end_byte)
                                }
                                (Some(del_range), None) => {
                                    let start_byte = rope.char_to_byte(
                                        del_range.range.min()
                                    );
                                    let end_byte = rope.char_to_byte(
                                        del_range.range.max()
                                    );
                                    (start_byte, end_byte, start_byte)
                                },
                                (None, Some(ins_range)) => {
                                    let start_byte = rope.char_to_byte(
                                        ins_range.range.min()
                                    );
    
                                    let end_byte = start_byte
                                        .map(|b| b + ins_range.chars.len());
    
                                    dbg!(rope, ins_range.range.max(), end_byte);
                                    (start_byte, start_byte, end_byte)
                                }
                                (None, None) => {
                                    // The edit apparently changed no characters,
                                    // so it seems there is nothing to tell the parser
                                    // about.
                                    continue
                                },
                            };
    
                            let start_byte = some_or!(start_byte, cont!()).0;
                            let old_end_byte = some_or!(old_end_byte, cont!()).0;
                            let new_end_byte = some_or!(new_end_byte, cont!()).0;
    
                            let (start_pos, old_end_pos, new_end_pos) = match (old, new) {
                                (Some(old), Some(new)) => {
                                    let old_pos = old.get_position();
                                    let old_h_pos = old.get_highlight_position_or_position();
                                    let old_min = min(old_pos, old_h_pos);
                                    let old_end_pos = max(old_pos, old_h_pos);
            
                                    let new_pos = new.get_position();
                                    let new_h_pos = new.get_highlight_position_or_position();
                                    let new_min = min(new_pos, new_h_pos);
                                    let new_end_pos = max(new_pos, new_h_pos);
            
                                    let start_pos = min(old_min, new_min);
    
                                    (start_pos, old_end_pos, new_end_pos)
                                },
                                (Some(old), None) => {
                                    let old_pos = old.get_position();
                                    let old_h_pos = old.get_highlight_position_or_position();
                                    let old_min = min(old_pos, old_h_pos);
                                    let old_end_pos = max(old_pos, old_h_pos);
    
                                    let start_pos = old_min;
    
                                    (start_pos, old_end_pos, start_pos)
                                },
                                (None, Some(new)) => {
                                    let new_pos = new.get_position();
                                    let new_h_pos = new.get_highlight_position_or_position();
                                    let new_min = min(new_pos, new_h_pos);
                                    let new_end_pos = max(new_pos, new_h_pos);
            
                                    let start_pos = new_min;
    
                                    (start_pos, start_pos, new_end_pos)
                                },
                                (None, None) => cont!(),
                            };

                            dbg!(InputEdit{
                                start_byte,
                                old_end_byte,
                                new_end_byte,
                                start_position: Point{ 
                                    row: start_pos.line,
                                    column: start_pos.offset.0
                                },
                                old_end_position: Point{ 
                                    row: old_end_pos.line,
                                    column: old_end_pos.offset.0
                                },
                                new_end_position: Point{ 
                                    row: new_end_pos.line,
                                    column: new_end_pos.offset.0
                                },
                            })
                        };

                        perf_viz::start_record!("tree.edit");
                        tree.edit(&input_edit);
                        perf_viz::end_record!("tree.edit");
                    }
                }
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

fn get_or_init_buffer_state<'map>(
    parser_map: &'map mut ParserMap,
    buffer_name: &BufferName,
    kind: ParserKind,
    // TODO pass a reference to all the langs, if/when we support more than one.
    rust_lang: Language, 
) -> &'map mut BufferState {
    u!{ParserKind}
    let buffer_state = parser_map
        .entry(buffer_name.clone())
        .or_insert_with(|| BufferState::default());

    // We can assume that `set_language` will return `Ok` because we should
    // have already tried it once in `InitializedParsers::new`
    match kind {
        Rust(_) => {
            let _res = buffer_state.parser.set_language(rust_lang);
            debug_assert!(
                _res.is_ok(),
                "Failed to set language for {}",
                kind
            );
        },
        Plaintext => {},
    }

    buffer_state
}

impl InitializedParsers {
    #[perf_viz::record]
    fn get_spans<'to_parse>(
        &mut self,
        to_parse: ToParse<'to_parse>,
        buffer_name: &BufferName,
        kind: ParserKind,
    ) -> SpansResult<'to_parse> {
        dbg!("get_spans");
        use ParserKind::*;
        use Style::*;
        match kind {
            Plaintext => {
                Ok(query::plaintext_spans_for(to_parse))
            }
            Rust(style) => {
                let state = get_or_init_buffer_state(
                    &mut self.parser_map,
                    buffer_name,
                    kind,
                    self.rust_lang,
                );
                
                perf_viz::start_record!("hash for caching");
                let fresh_hash = hash_to_parse(&to_parse);
                perf_viz::end_record!("hash for caching");

                // This was wriiten right after this caching was introduced.
                //
                // If this still seems too slow, one thing we could try is iterating
                // through with the tree cursor in whatever way is fastest, and 
                // sorting at the end, since we're still sorting as it is. This 
                // assumes that de don't actually need depth first traversal, which
                // is somewhat uncertain.
                //
                // Alternately, we could collect spans for around N ms and return
                // the partially complete spans. This prevents the thread stalling
                // for too long, and should just appear as a few frames where the
                // portions without known spans are just coloured plainly. However,
                // we'd also have to figure out how to signal to the client that
                // `update_and_render` should be called again.
                //
                if let Some(CachedSpans{ spans, hash }) = state.spans.as_ref() {
                    if *hash == fresh_hash {
                        return Ok(spans.clone());
                    }
                }

                perf_viz::start_record!("state.parser.parse");

                // This edit call that should do nothing, is here as a workaround
                // for the `asking_to_parse_the_empty_string_twice_does_not_panic`
                // test failing.
                if let Some(t) = state.tree.as_mut() {
                    t.edit(&InputEdit{
                        start_byte: d!(),
                        old_end_byte: d!(),
                        new_end_byte: d!(),
                        start_position: d!(),
                        old_end_position: d!(),
                        new_end_position: d!(),
                    });
                }

                state.tree = state.parser.parse(
                    to_parse.as_ref(),
                    state.tree.as_ref()
                );

                // Quoting the `parse` method docs:
                // Returns a Tree if parsing succeeded, or None if:
                //
                // * The parser has not yet had a language assigned with Parser::set_language
                // * The timeout set with Parser::set_timeout_micros expired
                // * The cancellation flag set with Parser::set_cancellation_flag was flipped

                // Given that if we got here the language should be set, we don't 
                // currently set a timeout, and, we don't currenlty cancel parses,
                // this assert should not ever fail.
                debug_assert!(state.tree.is_some(), "parse failed");

                perf_viz::end_record!("state.parser.parse");

                if let Some(tree) = state.tree.as_ref() {
                    let spans = match style {
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
                    };

                    state.spans = Some(CachedSpans{
                        spans: spans.clone(),
                        hash: fresh_hash,
                    });

                    Ok(spans)
                } else {
                    state.spans = None;
                    Err((
                        to_parse,
                        SpanError::ParseReturnedNone(buffer_name.clone())
                    ))
                }
            }
        }
    }
}

fn hash_to_parse<'to_parse>(to_parse: &ToParse<'to_parse>) -> u64 {
    use core::hash::{Hash, Hasher};
    
    let mut hasher: fast_hash::Hasher = d!();
    to_parse.hash(&mut hasher);
    return hasher.finish();
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

        let mut parser_map: ParserMap = d!();
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
    pub use platform_types::{ByteIndex, Spans, SpanView, SpanKind, sk, sv};

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

    #[perf_viz::record]
    pub fn plaintext_spans_for(s: ToParse<'_>) -> Spans {
        Spans::from(vec![plaintext_end_span_for(s)])
    }
    
    fn plaintext_end_span_for(s: ToParse<'_>) -> SpanView {
        sv!(i s.len(), k PLAIN)
    }

    #[perf_viz::record]
    pub fn spans_for<'to_parse>(
        tree: &Tree,
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
        tree: &Tree,
        query: &Query,
        to_parse: &'to_parse str,
        span_kind_from_match: fn(Match) -> SpanKind,
    ) -> Spans {
        let mut spans = Vec::with_capacity(get_spans_capacity(
            to_parse
        ));
    
        let mut span_stack: Vec<SpanView> = Vec::with_capacity(16);
    
        // TODO maybe test this in isolation from the tree-sitter query stuff.
        let mut receive_match = |m: Match| {
            let node = m.capture.node;
            let kind = span_kind_from_match(m);
        
            struct PotentialSpanView {
                one_past_end: usize,
                kind: SpanKind,
            }

            macro_rules! get_prev {
                () => {
                    span_stack.last().map(|s| PotentialSpanView {
                        one_past_end: s.one_past_end.0,
                        kind: s.kind,
                    }).unwrap_or_else(|| PotentialSpanView {
                        kind: SpanKind::PLAIN,
                        one_past_end: 0xFFFF_FFFF_FFFF_FFFF
                    })
                }
            }
        
            if get_prev!().one_past_end <= node.start_byte() {
                if let Some(s) = span_stack.pop() {
                    spans.push(s);
                }
            }
        
            spans.push(SpanView {
                one_past_end: ByteIndex(node.start_byte()),
                kind: get_prev!().kind,
            });
        
            span_stack.push(SpanView {
                one_past_end: ByteIndex(node.end_byte()),
                kind,
            });
        };
    
        let mut query_cursor = QueryCursor::new();

        let text_callback = |n: Node| {
            let r = n.range();
            &to_parse[r.start_byte..r.end_byte]
        };

        for q_match in query_cursor.matches(
            query,
            tree.root_node(),
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
    
        while let Some(s) = span_stack.pop() {
            spans.push(s);
        }
    
        cap_off_spans(&mut spans, ByteIndex(to_parse.len()));
    
        // TODO there is probably a clever way to change the above code so this line,
        // and the associted O(n log n) running time, is unnecessary.
        spans.sort_by(|s1, s2|{
            s1.one_past_end.cmp(&s2.one_past_end)
        });

        filter_spans(&mut spans);
    
        Spans::from(spans)
    }

    fn cap_off_spans(spans: &mut Vec<SpanView>, to_parse_byte_len: ByteIndex) {
        // If there's no span covering the end, we should add a span. But if one
        // that already covers the end is there then we shouldn't bother.

        if spans.last()
            .map(|s| s.one_past_end < to_parse_byte_len)
            .unwrap_or(true) 
        {
            spans.push(SpanView {
                one_past_end: to_parse_byte_len,
                kind: SpanKind::PLAIN,
            });
        }
    }
    
    #[perf_viz::record]
    pub fn totally_classified_spans_for<'to_parse>(
        tree: &Tree,
        to_parse: &'to_parse str,
    ) -> Spans {
        totally_classified_spans_for_inner(
            tree,
            to_parse,
            rust_extra_span_kind_from_node,
        )
    }

    fn totally_classified_spans_for_inner<'to_parse>(
        tree: &Tree,
        to_parse: &'to_parse str,
        span_kind_from_node: fn(Node) -> SpanKindSpec,
    ) -> Spans {
        let mut spans = Vec::with_capacity(get_spans_capacity(
            to_parse
        ));
    
        let mut drop_until_end_byte = None;
    
        perf_viz::start_record!("DepthFirst::new(tree)");
        for (_, node) in DepthFirst::new(tree) {
            //perf_viz::record_guard!("for (_, node) in DepthFirst::new(tree) body");
            use SpanKindSpec::*;
    
            //perf_viz::start_record!("node.end_byte()");
            let one_past_end = ByteIndex(node.end_byte());
            //perf_viz::end_record!("node.end_byte()");
    
            {
                //perf_viz::record_guard!("set drop_until_end_byte or continue");
                if let Some(end_byte) = drop_until_end_byte {
                    if one_past_end > end_byte {
                        drop_until_end_byte = None;
                    } else {
                        continue;
                    }
                }
            }
    
            //perf_viz::start_record!("spans.push match");
            match span_kind_from_node(node) {
                DropNode => {}
                Kind(kind) => {
                    spans.push(SpanView {
                        one_past_end,
                        kind,
                    });
                }
                KindAndDropBelow(kind) => {
                    drop_until_end_byte = Some(one_past_end);
    
                    spans.push(SpanView {
                        one_past_end,
                        kind,
                    });
                }
            }
            //perf_viz::end_record!("spans.push match");
        }
        perf_viz::end_record!("DepthFirst::new(tree)");
    
        cap_off_spans(&mut spans, ByteIndex(to_parse.len()));

        perf_viz::start_record!("spans.sort_by");
        // TODO maybe there's a way to do this without an O(n log n) sort?
        spans.sort_by(|s1, s2|{
            s1.one_past_end.cmp(&s2.one_past_end)
        });
        perf_viz::end_record!("spans.sort_by");
    
        filter_spans(&mut spans);
    
        spans.into()
    }
    
    type Depth = u8;
    
    #[perf_viz::record]
    pub fn tree_depth_spans_for<'to_parse>(
        tree: &Tree,
        to_parse: &'to_parse str,
    ) -> Spans {
        let mut spans = Vec::with_capacity(get_spans_capacity(
            to_parse
        ));
    
        tree_depth_extract_sorted(tree, &mut spans);
    
        cap_off_spans(&mut spans, ByteIndex(to_parse.len()));

        let len = spans.len();
        if len <= 1 {
            filter_spans(&mut spans);
            return spans.into();
        }
    
        // scan backwards getting rid of the ones that are overlapped.
        let mut prev_max = ByteIndex::default();
    
        for i in (0..(spans.len() - 1)).rev() {
            if spans[i].one_past_end > prev_max {
                prev_max = spans[i].one_past_end;
            }
            if spans[i + 1].kind != spans[i].kind {
                if spans[i + 1].one_past_end <= spans[i].one_past_end
                && spans[i].one_past_end >= prev_max {
                    spans.remove(i);
                }
    
                prev_max = spans[i].one_past_end;
            }
        }
        
        filter_spans(&mut spans);
        
        spans.into()
    }
    
    fn tree_depth_extract_sorted(
        tree: &Tree,
        spans: &mut Vec<SpanView>,
    ) {
        for (depth, node) in DepthFirst::new(tree) {
            let new_end_byte_index = ByteIndex(node.end_byte());
            let new = SpanView {
                kind: sk!(depth),
                one_past_end: new_end_byte_index
            };
    
            if let Some(previous) = spans.pop() {
                if previous.one_past_end >= new_end_byte_index {
                    spans.push(new);
                    if previous.one_past_end != new_end_byte_index {
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
            s1.one_past_end.cmp(&s2.one_past_end)
        });
    }
    
    struct DepthFirst<'tree> {
        depth: Depth,
        cursor: TreeCursor<'tree>,
        done: bool,
    }
    
    impl <'tree> DepthFirst<'tree> {
        fn new(tree: &'tree Tree) -> Self {
            DepthFirst {
                depth: 0,
                cursor: tree.walk(),
                done: false,
            }
        }
    }
    
    impl <'tree> Iterator for DepthFirst<'tree> {
        type Item = (Depth, Node<'tree>);
    
        fn next(&mut self) -> Option<Self::Item> {
            perf_viz::record_guard!("DepthFirst::next");
            if self.done {
                return None;
            }
            let depth = &mut self.depth;
            let output = Some((
                *depth,
                {
                    perf_viz::record_guard!("self.cursor.node()");
                    self.cursor.node()
                }
            ));

            if {
                perf_viz::record_guard!("self.cursor.goto_first_child()");
                self.cursor.goto_first_child()
            } {
                *depth = depth.wrapping_add(1);
            } else {
                perf_viz::record_guard!("while !goto_next_sibling");
                while {
                    perf_viz::record_guard!("!self.cursor.goto_next_sibling()");
                    !self.cursor.goto_next_sibling()
                } {
                    if {
                        perf_viz::record_guard!("self.cursor.goto_parent()");
                        self.cursor.goto_parent()
                    } {
                        *depth = depth.wrapping_sub(1);
                    } else {
                        self.done = true;
                        break;
                    }
                }
            }
    
            output
        }
    }
    
    #[perf_viz::record]
    fn filter_spans(spans: &mut Vec<SpanView>) {
        dedup_by_end_byte_keeping_last(spans);
        dedup_by_kind_keeping_last(spans);
        
        for i in (0..spans.len()).rev() {
            if spans[i].one_past_end == 0 {
                spans.remove(i);
            }
        }
    }
    
    fn dedup_by_kind_keeping_last(spans: &mut Vec<SpanView>) {
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
    
    fn dedup_by_end_byte_keeping_last(spans: &mut Vec<SpanView>) {
        let mut write = 0;
        for i in 0..spans.len() {
            let prev_end_byte_index = spans[write].one_past_end;
            if prev_end_byte_index != spans[i].one_past_end {
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
            recursive_dbg(Some(get_rust_tree!($code).root_node()));
        }
    }

    #[cfg(test)]
    mod tests;
}

#[cfg(test)]
mod tests;