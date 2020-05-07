#![deny(dead_code)]
#![deny(unused_imports)]
#![deny(unused_macros)]

use platform_types::{*};

use g_i::SelectableVec1;
use macros::{d, u};
use text_buffer::{TextBuffer};

#[derive(Clone, Debug, Default)]
struct EditorBuffer {
    pub text_buffer: TextBuffer,
    pub name: BufferName,
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
    fn new<I: Into<TextBuffer>>(name: BufferName, s: I) -> Self {
        Self {
            name,            text_buffer: s.into(),
            ..d!()
        }
    }
}

/// The collection of files opened for editing, and/or in-memory scratch buffers.
/// Guaranteed to have at least one buffer in it at all times.
#[derive(Clone, Debug, Default)]
struct EditorBuffers {
    buffers: SelectableVec1<EditorBuffer>,
}

impl EditorBuffers {
    /// Since there is always at least one buffer, this always returns at least 1.
    fn len(&self) -> g_i::Length {
        self.buffers.len()
    }

    /// The index of the currectly selected buffer.
    fn current_index(&self) -> g_i::Index {
        self.buffers.current_index()
    }

    fn get_current_buffer_mut(&mut self) -> &mut EditorBuffer {
        self.buffers.get_current_element_mut()
    }

    fn append_index(&self) -> g_i::Index {
        self.buffers.append_index()
    }

    fn index_with_name(&self, name: &BufferName) -> Option<g_i::Index> {
        let mut index = None;
        for (i, buffer) in self.buffers.iter_with_indexes() {
            if buffer.name == *name {
                index = Some(i);
                break;
            }
        }
        index
    }

    fn buffers(&self) -> &SelectableVec1<EditorBuffer> {
        &self.buffers
    }
}

#[derive(Debug, Default)]
struct State {    buffers: EditorBuffers,
    view: View,
}

fn update_and_render(state: &mut State, input: Input) -> UpdateAndRenderOutput {
    perf_viz::record_guard!("update_and_render");

    macro_rules! text_buffer_call {
        (sync $buffer: ident . $($method_call:tt)*) => {
            text_buffer_call!($buffer . $($method_call)*);
        };
        ($buffer: ident . $($method_call:tt)*) => {
            text_buffer_call!($buffer {$buffer.$($method_call)*})
        };
        (sync $buffer: ident $tokens:block) => {{
            text_buffer_call!($buffer $tokens);
        }};
        ($buffer: ident $tokens:block) => {{
            let $buffer = &mut state.buffers.get_current_buffer_mut().text_buffer;
                $tokens;
        }}
    }

    if cfg!(feature = "extra-prints")
    {
        if_changed::dbg!(&input);
    }

    let mut cmd = Cmd::NoCmd;
    
    state.view.edited_transitions.clear();

    u!{EditedTransition};

    macro_rules! mark_edited_transition {
        (current, $transition: expr) => {
            mark_edited_transition!(
                state.buffers.current_index(),
                $transition
            )
        };
        (append, $transition: expr) => {
            mark_edited_transition!(
                state.buffers.append_index(),
                $transition
            )
        };
        ($index: expr, $transition: expr) => {{
            // Since this may be an expression with sidee effects, 
            // we want this to be evaluated whether or not we want
            // to record the transition.
            let transition = $transition;

                let transition: Option<EditedTransition> = transition.into();
    
                if let Some(transition) = transition {
                    state.view.edited_transitions.push((
                        $index,
                        transition,
                    ));
                }
            
        }};
    }

    u!{Input}
    match input {
        AddOrSelectBuffer(name, str) => {
            state.buffers.buffers.push_and_select_new(EditorBuffer::new(name, str));

            mark_edited_transition!(current, ToUnedited);
        }
        AdjustBufferSelection(adjustment) => {
            state.buffers.buffers.adjust_selection(adjustment);
        }
        DeleteLines => text_buffer_call!(sync b {
            mark_edited_transition!(current, b.delete_lines());
        }),
        _ => {}
    }

    (state.view.clone(), cmd)
}

#[cfg(test)]
mod tests;