use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread::JoinHandle;

use macros::u;
use platform_types::{BufferName, Cmd, EditorAPI, Input, LoadBufferViewsResult, View};
use wimp_types::{BufferInfo, BufferStatusTransition, CustomEvent, EditedFilesThread, EditorThreadInput};
use window_layer::EventLoopProxy;

// I suspect we wouldn't notice the difference if we were to box this, and this way
// has fewer moving parts.
// TODO Actually try boxing this and measure the difference.
#[allow(clippy::large_enum_variant)]
pub enum Output {
    Rendered((View, Cmd, Cmd, LoadBufferViewsResult)),
    Pid(u32)
}

/// # Panics
/// This fn panics if the `load_buffer_views` field of `EditorAPI` doesn't always 
/// return a `Vec` of at least length one when passed a length one slice.
pub fn start(
    editor_api: EditorAPI,
    proxy: EventLoopProxy<CustomEvent>,
    edited_files_in_sink: Sender<EditedFilesThread>,
) -> (Sender<EditorThreadInput>, Receiver<Output>, JoinHandle<()>) {
    // into the editor thread
    let (editor_in_sink, editor_in_source) = channel();
    // out of the editor thread
    let (editor_out_sink, editor_out_source) = channel();

    let editor_join_handle = std::thread::Builder::new()
        .name("editor".to_string())
        .spawn(move || {
            let EditorAPI {update_and_render, load_buffer_views}
                = editor_api;
            {
                let _hope_it_gets_there = editor_out_sink.send(
                    Output::Pid(std::process::id())
                );
            }

            while let Ok(editor_input) = editor_in_source.recv() {
                u!{EditorThreadInput}

                macro_rules! call_update_and_render_and_possibly_quit {
                    ($input: expr) => {{
                        let input = $input;
                        let was_quit = Input::Quit == input;

                        let should_make_active_tab_visible = match input {
                            Input::None
                            | Input::Quit
                            | Input::Insert(..)
                            | Input::Delete
                            | Input::DeleteLines
                            | Input::ScrollVertically(..)
                            | Input::ScrollHorizontally(..)
                            | Input::MoveAllCursors(..)
                            | Input::ExtendSelectionForAllCursors(..)
                            | Input::SelectAll
                            | Input::SetCursor(..)
                            | Input::DragCursors(..)
                            | Input::SelectCharTypeGrouping(..)
                            | Input::ExtendSelectionWithSearch
                            | Input::Undo
                            | Input::Redo
                            | Input::Cut
                            | Input::Copy
                            | Input::Paste(..)
                            | Input::InsertNumbersAtCursors
                            | Input::TabIn
                            | Input::TabOut
                            | Input::StripTrailingWhitespace
                            | Input::NextLanguage
                            | Input::PreviousLanguage
                            | Input::ToggleSingleLineComments => false,
                            Input::Escape
                            | Input::ResetScroll
                            | Input::SetSizeDependents(..)
                            | Input::SavedAs(..)
                            | Input::AddOrSelectBuffer(..)
                            | Input::AddOrSelectBufferThenGoTo(..)
                            | Input::NewScratchBuffer(..)
                            | Input::AdjustBufferSelection(..)
                            | Input::SelectBuffer(..)
                            | Input::OpenOrSelectBuffer(..)
                            | Input::CloseBuffer(..)
                            | Input::SetMenuMode(..)
                            | Input::SubmitForm
                            | Input::ShowError(..) => true,
                        };

                        let (v, c1) = (update_and_render)(input);
                        let (_i, label) = v.current_text_index_and_buffer_label();
                        let visible_buffer_name: BufferName = label.name.clone();

                        let mut results = (load_buffer_views)(&[visible_buffer_name]);

                        // TODO: could pass and return Vec1 to avoid this.
                        debug_assert_eq!(results.len(), 1);

                        let result = results.pop().unwrap();

                        let c2 = if should_make_active_tab_visible {
                            Cmd::MakeActiveTabVisible
                        } else {
                            Cmd::None
                        };

                        let _hope_it_gets_there = editor_out_sink.send(
                            Output::Rendered((v, c1, c2, result))
                        );
                        if was_quit {
                            return;
                        }
                    }}
                }

                match editor_input {
                    Render(input) => {
                        call_update_and_render_and_possibly_quit!(
                            input
                        );
                    },
                    SaveBuffers(index_state, names, mut statuses) => {
                        debug_assert_eq!(names.len(), statuses.len());
                        let mut infos = Vec::with_capacity(names.len());

                        let mut buffers = (load_buffer_views)(&names);

                        debug_assert_eq!(statuses.len(), buffers.len());

                        while let (Some(result), Some(status)) = (
                            buffers.pop(),
                            statuses.pop()
                        ) {
                            match result {
                                Ok(bv) => {
                                    infos.push(BufferInfo {
                                        name: bv.label.name.clone(),
                                        name_string: bv.label.name_string.clone(),
                                        chars: bv.data.chars,
                                        status,
                                    });
                                },
                                Err(err) => {
                                    let _hope_it_gets_there = proxy.send_event(
                                        CustomEvent::EditedBufferError(
                                            err
                                        )
                                    );
                                },
                            }
                        }

                        let _hope_it_gets_there = edited_files_in_sink.send(
                            EditedFilesThread::Buffers(
                                index_state,
                                infos
                            )
                        );
                    }
                    SaveToDisk(path, label, index) => {
                        let mut results = (load_buffer_views)(
                            &[label.name]
                        );

                        // TODO: could pass and return Vec1 to avoid this.
                        debug_assert_eq!(results.len(), 1);

                        let result = results.pop().unwrap();

                        match result.and_then(|b|
                            std::fs::write(&path, b.data.chars)
                                .map_err(|e| e.to_string())
                        ) {
                            Ok(_) => {
                                let _hope_that_gets_there = proxy.send_event(
                                    CustomEvent::MarkBufferStatusTransition(
                                        path,
                                        index,
                                        BufferStatusTransition::Save,
                                    )
                                );
                            }
                            Err(err) => {
                                call_update_and_render_and_possibly_quit!(
                                    Input::ShowError(err)
                                );
                            }
                        }
                    },
                }
            }
        })
        .expect("Could not start editor thread!");

    (editor_in_sink, editor_out_source, editor_join_handle)
}
