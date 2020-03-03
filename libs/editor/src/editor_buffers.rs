/// This module was originally created to make sure every change to the current index went 
/// through a single path so we could more easily track down a bug where the index was 
/// improperly set.
use g_i::SelectableVec1;
use macros::{d, u};
use platform_types::{screen_positioning::*, *};
use parsers::{ParserKind};
use text_buffer::{get_search_ranges, TextBuffer};

use std::path::PathBuf;

#[derive(Debug, Default)]
pub struct ScrollableBuffer {
    pub text_buffer: TextBuffer,
    pub scroll: ScrollXY,
}

impl ScrollableBuffer {
    pub fn try_to_show_cursors_on(
        &mut self,
        xywh: TextBoxXYWH,
        text_char_dim: CharDim,
    ) -> VisibilityAttemptResult {
        let scroll = &mut self.scroll;
        let cursors = self.text_buffer.borrow_cursors_vec();
    
        // We try first with this smaller xywh to make the cursor appear
        // in the center more often.
        let mut small_xywh = xywh.clone();
        //small_xywh.xy.x += small_xywh.wh.w / 4.0;
        //small_xywh.wh.w /= 2.0;
        small_xywh.xy.y += small_xywh.wh.h / 4.0;
        small_xywh.wh.h /= 2.0;
    
        let apron: Apron = text_char_dim.into();
    
        let text_space = position_to_text_space(cursors.last().get_position(), text_char_dim);
    
        let mut attempt_result;
        attempt_result = attempt_to_make_xy_visible(
            scroll,
            small_xywh,
            apron.clone(),
            text_space,
        );
    
        dbg!(&cursors, attempt_result);
    
        if attempt_result != VisibilityAttemptResult::Succeeded {
            attempt_result = attempt_to_make_xy_visible(
                scroll,
                xywh,
                apron,
                text_space,
            );
            dbg!(&cursors, attempt_result);
        }
    
        attempt_result
    }
}

#[derive(Debug, Default)]
pub struct SearchResults {
    pub needle: String,
    pub ranges: Vec<(Position, Position)>,
    pub current_range: usize,
}

pub fn update_search_results(needle: &TextBuffer, haystack: &mut EditorBuffer) {
    perf_viz::record_guard!("update_search_results");
    let ranges = get_search_ranges(
        needle.borrow_rope().full_slice(),
        &haystack.scrollable.text_buffer.borrow_rope(),
        d!(),
        d!(),
    );

    //TODO: Set `current_range` to something as close as possible to being on screen of haystack
    haystack.search_results = SearchResults {
        needle: needle.into(),
        ranges,
        current_range: 0,
    };
}

#[derive(Debug, Default)]
pub struct EditorBuffer {
    pub scrollable: ScrollableBuffer,
    pub name: BufferName,
    pub search_results: SearchResults,
    // If this is none, then it was not set by the user, and
    // we will use the default.
    pub parser_kind: Option<ParserKind>,
}

impl EditorBuffer {
    pub fn new<I: Into<TextBuffer>>(name: BufferName, s: I) -> Self {
        Self {
            name,
            scrollable: ScrollableBuffer {
                text_buffer: s.into(),
                ..d!()
            },
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
        self.scrollable.text_buffer.reset_cursor_states();
    }
}

/// The collection of files opened for editing, and/or in-memory scratch buffers.
/// Guaranteed to have at least one buffer in it at all times.
#[derive(Debug, Default)]
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

    pub fn set_current_index(&mut self, index: g_i::Index) {
        if self.buffers.set_current_index(index) {
            if let Some(buffer) = self.buffers.get_mut(self.current_index()) {
                // These need to be cleared so that the `platform_types::View` that is passed down
                // can be examined to detemine if the user wants to navigate away from the given
                // buffer. We do this with each buffer, even though a client might only care about
                // buffers of a given menu kind, since a different client might care about different
                // ones, including plain `Text` buffers.
                buffer.reset_cursor_states();
            }
        }
    }

    pub fn get_current_buffer(&self) -> Option<&EditorBuffer> {
        self.buffers.get_current_element()
    }

    pub fn get_current_buffer_mut(&mut self) -> Option<&mut EditorBuffer> {
        self.buffers.get_current_element_mut()
    }

    pub fn push_and_select_new(&mut self, buffer: EditorBuffer) {
        self.buffers.push_and_select_new(buffer);
    }

    pub fn add_or_select_buffer(&mut self, name: BufferName, str: String) {
        let matching_buffer_index = {
            let mut index = None;
            for (i, buffer) in self.buffers.iter_with_indexes() {
                if buffer.name == name {
                    index = Some(i);
                    break;
                }
            }
            index
        };

        if let Some(index) = matching_buffer_index {
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

    pub fn index_state(&self) -> g_i::State {
        self.buffers.index_state()
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