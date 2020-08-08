/// This module was originally created to make sure every change to the current index went 
/// through a single path so we could more easily track down a bug where the index was 
/// improperly set.
use editor_types::{Cursor};
use g_i::{SelectableVec1};
use macros::{d, dbg, fmt_debug, u};
use platform_types::*;
use parsers::{ParserKind};
use text_buffer::{TextBuffer};
use search::{SearchResults};
use panic_safe_rope::{RopeSlice, RopeSliceTrait};

use std::path::PathBuf;

#[derive(Clone, Default, PartialEq)]
pub struct EditorBuffer {
    pub text_buffer: TextBuffer,
    pub name: BufferName,
    //TODO: Set `current_range` to something as close as possible to being on screen of haystack
    // whenever this changes
    pub search_results: SearchResults,
    // If this is none, then it was not set by the user, and
    // we will use the default.
    pub parser_kind: Option<ParserKind>,
}

fmt_debug!(collapse default for EditorBuffer: me {
    blank_if_default!(text_buffer);
    blank_if_default!(name);
    blank_if_default!(search_results);
    blank_if_default!(parser_kind);
});

impl EditorBuffer {
    fn rope_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.text_buffer.rope_hash(state);
    }

    fn non_rope_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.text_buffer.non_rope_hash(state);
        use std::hash::Hash;
        self.parser_kind.hash(state);
    }
}

impl From<&EditorBuffer> for String {
    fn from(e_b: &EditorBuffer) -> Self {
        (&e_b.text_buffer).into()
    }
}

impl From<&mut EditorBuffer> for String {
    fn from(e_b: &mut EditorBuffer) -> Self {
        (&e_b.text_buffer).into()
    }
}

impl <I: Into<TextBuffer>> From<(BufferName, I)> for EditorBuffer {
    fn from((n, i): (BufferName, I)) -> Self {
        Self::new(n, i)
    }
}

impl EditorBuffer {
    pub fn new<I: Into<TextBuffer>>(name: BufferName, s: I) -> Self {
        Self {
            name,            text_buffer: s.into(),
            ..d!()
        }
    }

    pub fn get_parser_kind(&self) -> ParserKind {
        u!{ParserKind}
        self.parser_kind.unwrap_or_else(|| {
            match self.name.get_extension_or_empty() {
                "rs" => Rust(d!()),
                _ => Plaintext,
            }
        })
    }

    pub fn next_language(&mut self) {
        self.parser_kind = Some(
            self.get_parser_kind().next().unwrap_or_default()
        );
    }

    pub fn advance_or_refresh_search_results(&mut self, needle: RopeSlice) {
        if needle == self.search_results.needle {
            self.advance_to_next_search_result(needle);
        } else {
            dbg!("advance_or_refresh_search_results");
            self.refresh_search_results(needle);
            self.advance_to_next_search_result(needle);
        }
    }

    fn advance_to_next_search_result(&mut self, needle: RopeSlice) {
        dbg!(needle.len_bytes());
        if needle.len_bytes() > 0 {
            let search_results = &mut self.search_results;
            let len = search_results.ranges.len();
            search_results.current_range += 1;
            if search_results.current_range >= len {
                search_results.current_range = 0;
            }

            if let Some(pair) = self
                .search_results
                .ranges
                .get(self.search_results.current_range)
            {
                let c: Cursor = pair.into();
                self                    .text_buffer
                    .set_cursor(c, ReplaceOrAdd::Replace);            
            }
        }
    }

    pub fn refresh_search_results(&mut self, needle: RopeSlice) {        
        self.search_results.refresh(
            needle,
            self.text_buffer.borrow_rope()
        );
    }
}

/// The collection of files opened for editing, and/or in-memory scratch buffers.
/// Guaranteed to have at least one buffer in it at all times.
#[derive(Clone, Default, PartialEq)]
pub struct EditorBuffers {
    buffers: SelectableVec1<EditorBuffer>,
    last_non_rope_hash: u64,
    last_full_hash: Option<u64>,
}

fmt_debug!(collapse default for EditorBuffers: me {
    blank_if_default!(buffers);
    blank_if_default!(last_non_rope_hash, me.last_non_rope_hash == 0);
    blank_if_default!(last_full_hash);
});

impl EditorBuffers {
    pub fn new<I: Into<EditorBuffer>>(buffer: I) -> Self {
        Self {
            buffers: SelectableVec1::new(buffer.into()),
            ..d!()
        }
    }
}

impl EditorBuffers {
    #[perf_viz::record]
    pub fn should_render_buffer_views(&mut self) -> bool {
        if cfg!(feature = "no-cache") {
            return true;
        }
        use std::hash::{Hasher};
        let mut hasher: rustc_hash::FxHasher = d!();
        self.non_rope_hash(&mut hasher);
        let new_non_rope_hash = hasher.finish();

        if new_non_rope_hash == self.last_non_rope_hash {
            self.rope_hash(&mut hasher);
            let new_full_hash = Some(hasher.finish());
            let output = new_full_hash != self.last_full_hash;

            self.last_full_hash = new_full_hash;
            
            output
        } else {
            self.last_non_rope_hash = new_non_rope_hash;
            self.last_full_hash = None;

            true
        }
    }

    #[perf_viz::record]
    fn rope_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for b in self.buffers.iter() {
            b.rope_hash(state);
        }
    }

    #[perf_viz::record]
    fn non_rope_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for b in self.buffers.iter() {
            b.non_rope_hash(state);
        }
        // If this isn't here, then we get hash collision bugs when
        // we try to switch tabs.
        self.buffers.non_element_hash(state);
    }
}


impl EditorBuffers {
    /// Since there is always at least one buffer, this always returns at least 1.
    pub fn len(&self) -> g_i::Length {
        self.buffers.len()
    }

    /// The index of the currectly selected buffer.
    pub fn current_index(&self) -> g_i::Index {
        self.buffers.current_index()
    }

    pub fn current_index_part(&self) -> g_i::IndexPart {
        self.buffers.current_index_part()
    }

    pub fn set_current_index(&mut self, index: g_i::Index) -> bool {
        self.buffers.set_current_index(index)
    }

    pub fn get_current_buffer(&self) -> &EditorBuffer {
        self.buffers.get_current_element()
    }

    pub fn get_current_buffer_mut(&mut self) -> &mut EditorBuffer {
        self.buffers.get_current_element_mut()
    }

    pub fn push_and_select_new(&mut self, buffer: EditorBuffer) {
        self.buffers.push_and_select_new(buffer);
    }

    pub fn index_with_name(&self, name: &BufferName) -> Option<g_i::Index> {
        let mut index = None;
        for (i, buffer) in self.buffers.iter_with_indexes() {
            if buffer.name == *name {
                index = Some(i);
                break;
            }
        }
        index
    }

    pub fn add_or_select_buffer(&mut self, name: BufferName, str: String) {
        if let Some(index) = self.index_with_name(&name) {
            self.set_current_index(index);

            if name == d!() && usize::from(self.buffers.len()) <= 1 {
                let buffer = &mut self.get_current_buffer_mut().text_buffer;
                if buffer.has_no_edits() {
                    *buffer = str.into();
                }
            }
        } else {
            self.buffers.push_and_select_new(EditorBuffer::new(name, str));
        };
    }

    /// Sets the path and marks the buffer as unedited iff such a buffer exists.
    /// Returns `Some` iff changes were made.
    pub fn saved_as(&mut self, index: g_i::Index, path: PathBuf) -> Option<()> {
        if let Some(b) = self.buffers.get_mut(index) {
            (*b).name = BufferName::Path(path);
            b.text_buffer.set_unedited();
            Some(())
        } else {
            None
        }
    }

    pub fn adjust_selection(&mut self, adjustment: SelectionAdjustment) {
        self.buffers.adjust_selection(adjustment);
    }

    pub fn close_buffer(&mut self, index: g_i::Index) {
        self.buffers.remove_if_present(index);
    }

    pub fn buffers(&self) -> &SelectableVec1<EditorBuffer> {
        &self.buffers
    }
}

impl EditorBuffers {
    pub fn iter(&self) -> std::slice::Iter<EditorBuffer> {
        self.buffers.iter()
    }

    pub fn iter_with_indexes(&self) -> g_i::IterWithIndexes<EditorBuffer> {
        self.buffers.iter_with_indexes()
    }
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests {
    use super::*;
    pub mod arb {
        use super::*;
        use g_i::{svec1};
        use proptest::collection::vec;
        use pub_arb_text_buffer::{text_buffer_with_valid_cursors};
        use pub_arb_platform_types::{
            BufferNameSpec,
            buffer_name_with_spec,
            position, 
            selectable_vec1,
        };
        use proptest::prelude::{prop_compose, Just, any};

        prop_compose!{
            pub fn search_results(max_len: usize)(
                needle in ".*",
                ranges_vec in vec((position(), position()), 1..max_len),
            )(
                needle in Just(needle), 
                current_range in 0..=(ranges_vec.len() - 1), 
                ranges in Just(ranges_vec)
            ) -> SearchResults {
                SearchResults {
                    needle,
                    ranges,
                    current_range,
                }
            }
        }

        prop_compose!{
            pub fn editor_buffers()(
                buffers in selectable_vec1(editor_buffer(), 16),
                last_non_rope_hash in any::<u64>(),
                last_full_hash in proptest::option::of(any::<u64>()),
            ) -> EditorBuffers {
                EditorBuffers {
                    buffers,
                    last_full_hash,
                    last_non_rope_hash,
                }
            }
        }

        prop_compose!{
            pub fn editor_buffers_blank_hash()(
                buffers in selectable_vec1(editor_buffer(), 16),
            ) -> EditorBuffers {
                EditorBuffers {
                    buffers,
                    ..d!()
                }
            }
        }

        prop_compose!{
            pub fn editor_buffers_with_one_path_one_scratch()(
                e1 in editor_buffer_with_spec(
                    BufferNameSpec::Path.into()
                ),
                e2 in editor_buffer_with_spec(
                    BufferNameSpec::Scratch.into()
                ),
                last_non_rope_hash in any::<u64>(),
                last_full_hash in proptest::option::of(any::<u64>()),
            ) -> EditorBuffers {
                EditorBuffers {
                    buffers: svec1!(e1, e2),
                    last_full_hash,
                    last_non_rope_hash,
                }
            }
        }

        prop_compose!{
            pub fn editor_buffers_with_one_default_path_one_scratch()(
                e2 in editor_buffer_with_spec(
                    BufferNameSpec::Scratch.into()
                ),
                last_non_rope_hash in any::<u64>(),
                last_full_hash in proptest::option::of(any::<u64>()),
            ) -> EditorBuffers {
                let e1 = EditorBuffer {
                    name: BufferName::Path(".fakefile".into()),
                    ..d!()
                };
                EditorBuffers {
                    buffers: svec1!(e1, e2),
                    last_full_hash,
                    last_non_rope_hash,
                }
            }
        }

        prop_compose!{
            pub fn editor_buffer()(
                e in editor_buffer_with_spec(d!())
            ) -> EditorBuffer {
                e
            }
        }

        #[derive(Clone, Copy, Default)]
        pub struct EditorBufferSpec {
            name: BufferNameSpec,
        }

        impl From<BufferNameSpec> for EditorBufferSpec {
            fn from(name: BufferNameSpec) -> Self {
                Self {
                    name
                }
            }
        }

        prop_compose!{
            pub fn editor_buffer_with_spec(spec: EditorBufferSpec)(
                text_buffer in text_buffer_with_valid_cursors(),
                name in buffer_name_with_spec(spec.name),
                s_r in search_results(16),
            ) -> EditorBuffer {
                EditorBuffer {
                    text_buffer,
                    name,
                    search_results: s_r,
                    parser_kind: None, // TODO if it ever matters
                }
            }
        }
    }
}