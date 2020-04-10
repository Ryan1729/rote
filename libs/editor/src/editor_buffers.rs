/// This module was originally created to make sure every change to the current index went 
/// through a single path so we could more easily track down a bug where the index was 
/// improperly set.
use editor_types::{Cursor};
use g_i::SelectableVec1;
use macros::{d, u};
use platform_types::{screen_positioning::*, *};
use parsers::{ParserKind};
use text_buffer::{TextBuffer};
use search::{SearchResults};
use panic_safe_rope::{RopeSlice, RopeSliceTrait};

use std::path::PathBuf;

#[derive(Clone, Debug, Default)]
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

    pub fn reset_cursor_states(&mut self) {
        self.text_buffer.reset_cursor_states();
    }

    pub fn update_search_results(&mut self, needle: RopeSlice) {
        if needle == self.search_results.needle {
            // advance to next search result
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
                    self                        .text_buffer
                        .set_cursor(c, ReplaceOrAdd::Replace);            
                }
            }
        } else {
            self.search_results = SearchResults::new(
                needle,
                self.text_buffer.borrow_rope()
            );
        }
    }
}

/// The collection of files opened for editing, and/or in-memory scratch buffers.
/// Guaranteed to have at least one buffer in it at all times.
#[derive(Clone, Debug, Default)]
pub struct EditorBuffers {
    buffers: SelectableVec1<EditorBuffer>,
}

impl EditorBuffers {
    pub fn new(buffer: EditorBuffer) -> Self {
        Self {
            buffers: SelectableVec1::new(buffer),
            ..d!()
        }
    }

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

    pub fn append_index(&self) -> g_i::Index {
        self.buffers.append_index()
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
        } else {
            self.buffers.push_and_select_new(EditorBuffer::new(name, str));
        };
    }

    pub fn set_path(&mut self, index: g_i::Index, path: PathBuf) {
        if let Some(b) = self.buffers.get_mut(index) {
            (*b).name = BufferName::Path(path);
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
        use proptest::collection::vec;
        use pub_arb_text_buffer::{text_buffer_with_valid_cursors};
        use pub_arb_platform_types::{buffer_name, position, selectable_vec1, scroll_xy, usual};
        use proptest::prelude::{prop_compose, Just};

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
            ) -> EditorBuffers {
                EditorBuffers {
                    buffers
                }
            }
        }

        prop_compose!{
            pub fn editor_buffer()(
                scrollable in scrollable_buffer(),
                name in buffer_name(),
                s_r in search_results(16),
            ) -> EditorBuffer {
                EditorBuffer {
                    scrollable,
                    name,
                    search_results: s_r,
                    parser_kind: None, // TODO if it ever matters
                }
            }
        }
    
        prop_compose!{
            pub fn scrollable_buffer()(
                t_b in text_buffer_with_valid_cursors(),
                scroll in scroll_xy(usual()),
            ) -> ScrollableBuffer {
                ScrollableBuffer {
                    ..d!()
                    /*text_buffer: t_b,
                    scroll,*/
                }
            }
        }
    }
}