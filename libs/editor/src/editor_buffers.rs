/// This module was originally created to make sure every change to the current index went 
/// through a single path so we could more easily track down a bug where the index was 
/// improperly set.
use super::*;

#[derive(Debug, Default)]
pub struct ScrollableBuffer {
    pub text_buffer: TextBuffer,
    pub scroll: ScrollXY,
}

pub fn try_to_show_cursors_on(
    buffer: &mut ScrollableBuffer,
    xywh: TextBoxXYWH,
    char_dim: CharDim,
) -> VisibilityAttemptResult {
    let scroll = &mut buffer.scroll;
    let cursors = buffer.text_buffer.borrow_cursors_vec();

    // We try first with this smaller xywh to make the cursor appear
    // in the center more often.
    let mut small_xywh = xywh.clone();
    //small_xywh.xy.x += small_xywh.wh.w / 4.0;
    //small_xywh.wh.w /= 2.0;
    small_xywh.xy.y += small_xywh.wh.h / 4.0;
    small_xywh.wh.h /= 2.0;

    let mut attempt_result;
    attempt_result = attempt_to_make_sure_at_least_one_cursor_is_visible(
        scroll,
        small_xywh,
        char_dim,
        cursors,
    );

    if attempt_result != VisibilityAttemptResult::Succeeded {
        dbg!();
        attempt_result = attempt_to_make_sure_at_least_one_cursor_is_visible(
            scroll,
            xywh,
            char_dim,
            cursors,
        );
    }

    attempt_result
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
    buffers: SelectableVec1<A>,
}

impl EditorBuffers {
    pub fn new(buffer: EditorBuffer) -> Self {
        Self {
            buffers: SelectableVec1::new(buffer),
            ..d!()
        }
    }

    /// Since there is always at least one buffer, this always returns at least 1.
    /*pub fn len(&self) -> g_i::Length {
        debug_assert!(self.buffers.len() <= g_i::Length::max_value());
        g_i::Length::or_max(self.buffers.len())
    }

    /// The index of the first buffer.
    pub fn first_index(&self) -> g_i::Index {
        self.index_state.new_index(g_i::IndexPart::or_max(0))
    }

    /// The index of the last buffer.
    pub fn last_index(&self) -> g_i::Index {
        let len: usize = self.len().into();
        self.index_state.new_index(g_i::IndexPart::or_max(len - 1))
    }

    /// The index of the currectly selected buffer.
    pub fn current_index(&self) -> g_i::Index {
        self.current_index
    }

    pub fn get_mut(&mut self, index: g_i::Index) -> Option<&mut EditorBuffer> {
        index
            .get(self.index_state)
            .and_then(move |i| self.buffers.get_mut(i))
    }

    pub fn get(&self, index: g_i::Index) -> Option<&EditorBuffer> {
        index
            .get(self.index_state)
            .and_then(|i| self.buffers.get(i))
    }

    pub fn set_current_index(&mut self, index: g_i::Index) {
        if index < self.len() {
            self.current_index = index;

            if let Some(buffer) = self.get_mut(self.current_index) {
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
        self.get(self.current_index())
    }

    pub fn get_current_buffer_mut(&mut self) -> Option<&mut EditorBuffer> {
        self.get_mut(self.current_index())
    }

    pub fn push(&mut self, buffer: EditorBuffer) {
        let will_fit = self.buffers.len() < g_i::Length::max_value();
        debug_assert!(will_fit);
        if will_fit {
            self.buffers.push(buffer);
        }
    }

    pub fn move_buffer(&mut self, buffer_move: BufferMove) {
        u!{BufferMove}
        let target_index = match buffer_move {
            Left => self.previous_index(),
            Right => self.next_index(),
            ToStart => self.first_index(),
            ToEnd => self.last_index(),
        };

        self.swap_or_ignore(
            target_index,
            self.current_index,
        );
    }

    pub fn close_buffer(&mut self, index: g_i::Index) {
        self.remove_if_present(index);

        self.set_current_index(
            self.index_state
            .migrate(self.current_index)
            .or_else(|| self.index_state.migrate(self.previous_index()))
            .unwrap_or_else(||
                // if the current index is zero and we remove it we end up pointing at the new
                // first element. In this case, this is desired.
                self.index_state.new_index(d!())
            )
        );
    }

    pub fn select_next(&mut self) {
        self.set_current_index(self.next_index());
    }

    pub fn select_previous(&mut self) {
        self.set_current_index(self.previous_index());
    }

    pub fn next_index(&self) -> g_i::Index {
        (self.current_index.saturating_add(1)) % self.len()
    }

    pub fn previous_index(&self) -> g_i::Index {
        let current_buffer_index = self.current_index;
        let i: usize = current_buffer_index.into();
        if i == 0 {
            self.last_index()
        } else {
            current_buffer_index.saturating_sub(1)
        }
    }

    pub fn swap_or_ignore(&mut self, index1: g_i::Index, index2: g_i::Index) {
        if index1 < self.len() && index2 < self.len() {
            if let Some((i1, i2)) = index1.get(self.index_state)
                .and_then(|i1| {
                    index2.get(self.index_state)
                        .map(|i2| (i1, i2))
                }) {
                self.buffers.swap(i1, i2);
                self.index_state.swapped_at_or_ignore(index1, index2);
            }
        }
    }

    pub fn remove_if_present(&mut self, index: g_i::Index) -> Option<EditorBuffer> {
        if index < self.len() {
            index.get(self.index_state).and_then(|i| {
                let output = self.buffers.try_remove(i).ok();

                if output.is_some() {
                    // No reason to update the index state if we didn't remove anything.
                    self.index_state.removed_at(index);
                }

                output
            })
        } else {
            None
        }
    }

    pub fn index_state(&self) -> g_i::State {
        self.index_state
    }
*/
}

impl EditorBuffers {
    pub fn iter(&self) -> std::slice::Iter<EditorBuffer> {
        self.buffers.iter()
    }

    pub fn iter_with_indexes(&self) -> g_i::IterWithIndexes<EditorBuffer> {
        self.buffers.iter_with_indexes()
    }
}