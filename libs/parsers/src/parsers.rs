//#![deny(unused)]
#![allow(unreachable_code)]
use macros::{d, dbg, fmt_debug, fmt_display, some_or, u};
use platform_types::{BufferName, Rope, Spans};
use edit::{Edit, RangeEdits};

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

use std::{borrow::Cow};

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

impl DoubleEndedIterator for Style {
    fn next_back(&mut self) -> Option<Self::Item> {
        use Style::*;
        match self {
            Extra => {
                None
            },
            Basic => {
                Some(Extra)
            },
            TreeDepth => {
                Some(Basic)
            }
        }.map(|s| {
            *self = s;
            s
        })
    }
}

impl Style {
    pub const LAST: Self = Self::TreeDepth;
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ParserKind {
    Plaintext,
    Rust(Style),
    C/*rane*/(Style)
}
d!(for ParserKind: ParserKind::Plaintext);

fmt_display!(
    for ParserKind: match p {
        Plaintext => "txt".to_string(),
        Rust(s) => format!("rs({})", s),
        C(s) => format!("c({})", s),
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
                Some(
                    style.next()
                        .map_or_else(|| C(d!()), Rust)
                )
            },
            C(style) => {
                style.next()
                    .map(C)
            }
        }.map(|p| {
            *self = p;
            p
        })
    }
}

impl DoubleEndedIterator for ParserKind {
    fn next_back(&mut self) -> Option<Self::Item> {
        use ParserKind::*;
        match self {
            Plaintext => {
                None
            },
            Rust(style) => {
                Some(
                    style.next_back()
                        .map_or_else(|| Plaintext, C)
                )
            },
            C(style) => {
                Some(
                    style.next_back()
                        .map_or_else(|| Rust(Style::LAST), Rust)
                )
            }
        }.map(|p| {
            *self = p;
            p
        })
    }
}
impl ParserKind {
    #[must_use]
    pub fn default_from_name(name: &BufferName) -> Self {
        u!{ParserKind}
        match name.get_extension_or_empty() {
            "rs" => Rust(d!()),
            "c"|"h" => C(d!()),
            _ => Plaintext,
        }
    }

    #[must_use]
    fn to_ts_name(self) -> Option<TSName> {
        match self {
            Self::Rust(_) => Some(TSName::Rust),
            Self::C(_) => Some(TSName::C),
            Self::Plaintext => None,
        }
    }

    pub const LAST: Self = Self::C(Style::LAST);
}

/// Tree-Sitter Name
#[derive(Clone, Copy, PartialEq, Debug)]
enum TSName {
    Rust,
    C
}

const TS_NAME_COUNT: usize = 2;

impl TSName {
    #[must_use]
    const fn to_index(self) -> usize {
        match self {
            Self::Rust => 0,
            Self::C => 1,
        }
    }
}

type ByTSName<A> = [A; TS_NAME_COUNT];

macro_rules! map_to_by_ts_name {
    ($name: ident $(,)? $code: block) => {
        [
            {
                let $name = TSName::Rust;
                $code
            },
            {
                let $name = TSName::C;
                $code
            },
        ]
    }
}

/// Convenience macro for By TS Name indexing operation
macro_rules! btsn {
    ($variant: expr, $by_ts_name_in: expr) => {{
        let name: TSName = $variant;
        $by_ts_name_in[name.to_index()]
    }};
    ($variant: ident in $by_ts_name_in: expr) => {{
        $by_ts_name_in[TSName::$variant.to_index()]
    }}
}

#[cfg(test)]
const TS_NAME_BY_TS_NAME: ByTSName<TSName> = map_to_by_ts_name!(name {name});

#[test]
fn to_index_assigns_the_correct_indexes() {
    for i in 0..TS_NAME_BY_TS_NAME.len() {
        assert_eq!(TS_NAME_BY_TS_NAME[i].to_index(), i);
    }
}

#[derive(Debug)]
pub enum Parsers {
    NotInitializedYet,
    Initialized(Box<InitializedParsers>),
    FailedToInitialize(InitializationError)
}
d!(for Parsers: Parsers::NotInitializedYet);

fmt_display!(
    for Parsers: match p {
        NotInitializedYet => "---".to_string(),
        Initialized(i_p) => {
            format!("{:?}", i_p)
        },
        FailedToInitialize(InitializationError(e)) => e.clone(),
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
    ts_name: TSName,
}

#[cfg(not(feature = "fast_hash"))]
use std::collections::HashMap;

#[cfg(feature = "fast_hash")]
use fast_hash::Map as HashMap;

type ParserMap = HashMap<BufferName, BufferState>;

pub struct InitializedParsers {
    parser_map: ParserMap,
    basic_queries: ByTSName<Query>,
    languages: ByTSName<Language>
}

impl std::fmt::Debug for InitializedParsers {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("InitializedParsers")
           .field("parser_map", &("TODO parser_map: Debug".to_string()))
           .field("basic_queries", &self.basic_queries)
           .field("languages", &self.languages)
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
        if cfg!(feature = "skip_parsing") {
            return query::plaintext_spans_for(&to_parse);
        }

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
                query::plaintext_spans_for(&to_parse)
            }
        }
    }

    /// # Errors
    /// This returns an `Err` if the parsers could not be initializd, or there is an
    /// internal error from the underlying c library.
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

    /// This method should be called whenever the `ToParse` associated with a buffer
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
        use Parsers::*;
        self.attempt_init();

        match self {
            Initialized(initialized) => {
                let buffer_state = get_or_init_buffer_state(
                    &mut initialized.parser_map,
                    buffer_name,
                    kind,
                    &initialized.languages,
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
                    let range_edits = edit.range_edits();
                    for i in 0..range_edits.len() {
                        let input_edit = {
                            perf_viz::record_guard!("convert edit");
                            let RangeEdits{ delete_range, insert_range } =
                                some_or!(range_edits.get(i), cont!());

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

                            dbg!(InputEdit{
                                start_byte,
                                old_end_byte,
                                new_end_byte,
                                // I think these only matter for whitespace (newline) 
                                // sensitive languages. It would be something like
                                // call rope.byte_to_line to get the row,
                                // then call line_to_byte to get the start of the
                                // row, then subtract that from the byte to get the
                                // column. That's O(log n), which seems worth
                                // avoiding, given it happens every keystroke!
                                start_position: Point{
                                    row: 0,
                                    column: 0,
                                },
                                old_end_position: Point{
                                    row: 0,
                                    column: 0,
                                },
                                new_end_position: Point{
                                    row: 0,
                                    column: 0,
                                },
                            })
                        };

                        perf_viz::start_record!("tree.edit");
                        tree.edit(&input_edit);
                        perf_viz::end_record!("tree.edit");
                    }
                }
            },
            NotInitializedYet => {
                debug_assert!(false, "acknowledge_edit called on NotInitializedYet Parsers");
            },
            FailedToInitialize(_) => {
                debug_assert!(false, "acknowledge_edit called on FailedToInitialize Parsers with this error: {}", self);
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
    parser_kind: ParserKind,
    langs: &ByTSName<Language>,
) -> &'map mut BufferState {
    u!{ParserKind}
    let buffer_state = parser_map
        .entry(buffer_name.clone())
        .or_insert_with(BufferState::default);

    let parser_kind_ts_name = parser_kind.to_ts_name();

    // We want to avoid using the old buffer_state.parser if we switch languages
    // because we previously got whole program aborts due to tree-sitter assertions
    // when we did that.
    match (buffer_state.spans.as_ref().map(|s| s.ts_name), parser_kind_ts_name) {
        (Some(name1), Some(name2)) if name1 == name2 => {
            // The language was apparently the same, so nothing to do.
        },
        (None, None) => {
            // Neither is a tree-sitter parser so nothing to do.
        },
        _ => {
            // We might be able to get away with not doing this when we switch
            // to or from a non-tree-sitter language, but this way seems safest.
            *buffer_state = d!();
        }
    }

    match parser_kind_ts_name {
        Some(name) => {
            // We can assume that `set_language` will return `Ok` because we should
            // have already tried it once in `InitializedParsers::new`
            let res = buffer_state.parser.set_language(
                btsn!(name, langs)
            );
            debug_assert!(
                res.is_ok(),
                "Failed to set language for {}",
                parser_kind
            );
        },
        None => {},
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
        use ParserKind::*;
        match kind {
            Plaintext => {
                Ok(query::plaintext_spans_for(&to_parse))
            }
            Rust(style) => {
                self.get_spans_with_ts_name(
                    to_parse,
                    buffer_name,
                    kind,
                    style,
                    TSName::Rust,
                )
            }
            C(style) => {
                self.get_spans_with_ts_name(
                    to_parse,
                    buffer_name,
                    kind,
                    style,
                    TSName::C,
                )
            }
        }
    }

    fn get_spans_with_ts_name<'to_parse>(
        &mut self,
        to_parse: ToParse<'to_parse>,
        buffer_name: &BufferName,
        kind: ParserKind,
        style: Style,
        ts_name: TSName
    ) -> SpansResult<'to_parse> {
        use Style::*;

        let state = get_or_init_buffer_state(
            &mut self.parser_map,
            buffer_name,
            kind,
            &self.languages,
        );

        perf_viz::start_record!("hash for caching");
        let fresh_hash = hash_to_parse(&to_parse);
        perf_viz::end_record!("hash for caching");

        // This was written right after this caching was introduced.
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

        if let Some(CachedSpans{ spans, hash, .. }) = state.spans.as_ref() {
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
        // currently set a timeout, and, we don't currently cancel parses,
        // this assert should not ever fail.
        debug_assert!(state.tree.is_some(), "parse failed");

        perf_viz::end_record!("state.parser.parse");

        if let Some(tree) = state.tree.as_ref() {
            let spans = match style {
                Basic => {
                    query::spans_for(
                        tree,
                        &self.basic_queries[ts_name.to_index()],
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
                ts_name,
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

#[test]
fn after_calling_get_spans_with_ts_name_then_get_or_init_buffer_state_returns_a_buffer_state_with_spans_on_it() {
    let mut parsers: InitializedParsers = InitializedParsers::new().expect("InitializedParsers failed");
    let to_parse: ToParse = d!();
    let buffer_name = d!();
    let style = d!();
    let kind = ParserKind::Rust(style);
    let ts_name = TSName::Rust;

    {
        let state = get_or_init_buffer_state(
            &mut parsers.parser_map,
            &buffer_name,
            kind,
            &parsers.languages,
        );

        assert!(state.spans.is_none());
    }

    for _ in 0..16 {
        let _spans = parsers.get_spans_with_ts_name(
            to_parse.clone(),
            &buffer_name,
            kind,
            style,
            ts_name
        );

        {
            let state = get_or_init_buffer_state(
                &mut parsers.parser_map,
                &buffer_name,
                kind,
                &parsers.languages,
            );

            assert!(state.spans.is_some());
        }
    }
}

#[test]
fn after_calling_get_spans_with_ts_name_then_get_or_init_buffer_state_returns_a_buffer_state_with_spans_on_it_and_the_cache_use_case_gets_hit() {
    let mut parsers: InitializedParsers = InitializedParsers::new().expect("InitializedParsers failed");
    let to_parse: ToParse = "fn main() {}".into();
    let buffer_name = d!();
    let style = d!();
    let kind = ParserKind::Rust(style);
    let ts_name = TSName::Rust;

    {
        let state = get_or_init_buffer_state(
            &mut parsers.parser_map,
            &buffer_name,
            kind,
            &parsers.languages,
        );

        assert!(state.spans.is_none());
    }

    for _ in 0..16 {
        let _spans = parsers.get_spans_with_ts_name(
            to_parse.clone(),
            &buffer_name,
            kind,
            style,
            ts_name
        );

        {
            let state = get_or_init_buffer_state(
                &mut parsers.parser_map,
                &buffer_name,
                kind,
                &parsers.languages,
            );

            assert!(state.spans.is_some());

            let fresh_hash = hash_to_parse(&to_parse);

            let mut hit_cache = false;
            if let Some(CachedSpans{ hash, .. }) = state.spans.as_ref() {
                if *hash == fresh_hash {
                    hit_cache = true;
                }
            }

            assert!(hit_cache);
        }
    }
}

#[test]
fn after_calling_get_spans_with_ts_name_then_get_or_init_buffer_state_returns_a_buffer_state_with_spans_on_it_and_the_cache_use_case_gets_hit_reduction() {
    let mut parsers: InitializedParsers = InitializedParsers::new().expect("InitializedParsers failed");
    let to_parse: ToParse = "fn main() {}".into();
    let buffer_name = d!();
    let style = d!();
    let kind = ParserKind::Rust(style);
    let ts_name = TSName::Rust;

    {
        let state = get_or_init_buffer_state(
            &mut parsers.parser_map,
            &buffer_name,
            kind,
            &parsers.languages,
        );

        assert!(state.spans.is_none());
    }

    let _spans = parsers.get_spans_with_ts_name(
        to_parse.clone(),
        &buffer_name,
        kind,
        style,
        ts_name
    );

    let parser_map: &mut ParserMap = &mut parsers.parser_map;
    let buffer_name: &BufferName = &buffer_name;
    let parser_kind: ParserKind = kind;
    let langs: &ByTSName<Language> = &parsers.languages;

    u!{ParserKind}
    let buffer_state = parser_map
        .entry(buffer_name.clone())
        .or_insert_with(BufferState::default);

    let parser_kind_ts_name = parser_kind.to_ts_name();

    // We want to avoid using the old buffer_state.parser if we switch languages
    // because we previously got whole program aborts due to tree-sitter assertions
    // when we did that.
    match (buffer_state.spans.as_ref().map(|s| s.ts_name), parser_kind_ts_name) {
        (Some(name1), Some(name2)) if name1 == name2 => {
            // The language was apparently the same, so nothing to do.
        },
        (None, None) => {
            // Neither is a tree-sitter parser so nothing to do.
        },
        _ => {
            assert!(false, "Defaulted!");
            // We might be able to get away with not doing this when we switch
            // to or from a non-tree-sitter language, but this way seems safest.
            *buffer_state = d!();
        }
    }

    match parser_kind_ts_name {
        Some(name) => {
            // We can assume that `set_language` will return `Ok` because we should
            // have already tried it once in `InitializedParsers::new`
            let _res = buffer_state.parser.set_language(
                btsn!(name, langs)
            );
            debug_assert!(
                _res.is_ok(),
                "Failed to set language for {}",
                parser_kind
            );
        },
        None => {},
    }

    assert!(buffer_state.spans.is_some(), "reduction");
}

fn hash_to_parse(to_parse: &ToParse<'_>) -> u64 {
    use core::hash::{Hash, Hasher};

    let mut hasher: fast_hash::Hasher = d!();
    to_parse.hash(&mut hasher);
    hasher.finish()
}

extern "C" {
    fn tree_sitter_rust() -> Language;
    fn tree_sitter_c() -> Language;
}

type LangFn = unsafe extern "C" fn() -> Language;

const LANG_FNS: ByTSName<LangFn> = [
    tree_sitter_rust,
    tree_sitter_c
];

impl Parsers {
    fn attempt_init(&mut self) {
        use Parsers::*;
        match self {
            NotInitializedYet => {
                match InitializedParsers::new() {
                    Ok(p) => { *self = Initialized(Box::new(p)); }
                    Err(e) => { *self = FailedToInitialize(e); }
                }
            },
            Initialized(_) | FailedToInitialize(_) => {}
        }
    }
}

impl InitializedParsers {
    fn new() -> Result<Self, InitializationError> {
        let mut parser_map: ParserMap = d!();

        let languages = map_to_by_ts_name!{name {
            let index = name.to_index();
            let lang_fn = LANG_FNS[index];
            // SAFETY: This should only be unsafe because it is an extern function.
            let lang = unsafe { lang_fn() };

            let mut first: BufferState = d!();
            // We make sure that each supported language works at the start
            // so we can assume `set_language` always returns `Ok` afterwards.
            first.parser.set_language(
                lang
            )?;

            parser_map.insert(d!(), first);

            lang
        }};

        let basic_queries = map_to_by_ts_name!{name {
            let index = name.to_index();
            Query::new(
                languages[index],
                BASIC_QUERY_SOURCES[index]
            )?
        }};

        Ok(InitializedParsers {
            parser_map,
            basic_queries,
            languages,
        })
    }
}

const BASIC_QUERY_SOURCES: ByTSName<&str> = map_to_by_ts_name!{name {
    use TSName::*;
    match name {
        Rust => "
            (line_comment) @comment
            (block_comment) @comment
            (string_literal) @string
            (char_literal) @string
            (raw_string_literal) @string
        ",
        C => "
            (string_literal) @string
            (char_literal) @string
        ",
    }
}};

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
            s if s.starts_with("string")
                | s.starts_with("raw_string")
                | s.starts_with("char") => {
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
    pub(crate) fn plaintext_spans_for(s: &ToParse<'_>) -> Spans {
        Spans::from(vec![plaintext_end_span_for(s)])
    }

    fn plaintext_end_span_for(s: &ToParse<'_>) -> SpanView {
        sv!(i s.len(), k PLAIN)
    }

    #[perf_viz::record]
    pub(crate) fn spans_for<'to_parse>(
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
            struct PotentialSpanView {
                one_past_end: usize,
                kind: SpanKind,
            }

            let node = m.capture.node;
            let kind = span_kind_from_match(m);

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
            .map_or(true, |s| s.one_past_end < to_parse_byte_len)
        {
            spans.push(SpanView {
                one_past_end: to_parse_byte_len,
                kind: SpanKind::PLAIN,
            });
        }
    }

    #[perf_viz::record]
    pub(crate) fn totally_classified_spans_for(
        tree: &Tree,
        to_parse: &str,
    ) -> Spans {
        totally_classified_spans_for_inner(
            tree,
            to_parse,
            rust_extra_span_kind_from_node,
        )
    }

    fn totally_classified_spans_for_inner(
        tree: &Tree,
        to_parse: &str,
        span_kind_from_node: fn(Node) -> SpanKindSpec,
    ) -> Spans {
        let mut spans = Vec::with_capacity(get_spans_capacity(
            to_parse
        ));

        let mut drop_until_end_byte = None;

        perf_viz::start_record!("DepthFirst::new(tree)");
        for node in nodes_from_tree(tree) {
            perf_viz::record_guard!("for (_, node) in DepthFirst::new(tree) body");
            use SpanKindSpec::*;

            perf_viz::start_record!("node.end_byte()");
            let one_past_end = ByteIndex(node.end_byte());
            perf_viz::end_record!("node.end_byte()");

            {
                perf_viz::record_guard!("set drop_until_end_byte or continue");
                if let Some(end_byte) = drop_until_end_byte {
                    if one_past_end > end_byte {
                        drop_until_end_byte = None;
                    } else {
                        continue;
                    }
                }
            }

            perf_viz::start_record!("spans.push match");
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
            perf_viz::end_record!("spans.push match");
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

    #[cfg(feature = "tree-traversal")]
    fn nodes_from_tree(tree: &Tree) -> impl Iterator<Item = Node> {
        DepthFirst::new(tree).map(|(_, node)| node)
    }

    // TODO: I suspect, (with little justification) that this way is faster for large
    // amounts of nodes, when lots of branching happens during each iteration, due to
    // better cache coherence. I should really confirm this for sure, since the
    // difference, if there is one, is not dramatic enough to show up on the
    // flamegraphs. And if this is indeed faster, I should avoid allocating
    // this Vec every time.
    #[cfg(not(feature = "tree-traversal"))]
    fn nodes_from_tree(tree: &Tree) -> Vec<Node> {
        DepthFirst::new(tree).map(|(_, node)| node).collect::<Vec<_>>()
    }

    type Depth = u8;

    #[perf_viz::record]
    pub(crate) fn tree_depth_spans_for(
        tree: &Tree,
        to_parse: &str,
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
            // I want to be able to leave the perf_viz annotations here, even if they
            // expand to nothing sometimes
            #![allow(clippy::blocks_in_if_conditions)]

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
        recursive_dbg_helper(node, 0);
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