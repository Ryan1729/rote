//use super::*;
use super::{update_and_render, g_i};
use crate::{
    State,
    EditorBuffer,
    EditedTransition,
};
use platform_types::{Input, SelectionAdjustment, SelectionMove, BufferName};
use macros::{d, u, dbg};

/// This test predicate simulates what we expected clients to do if they want to keep track 
/// of which buffers are currently different from what is on disk. This is a little 
/// complicated because the editor is the one who knows about the undo history, and the 
/// client is the one who knows about when things are saved to disk or not.
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
    mut state: State,
    inputs: Vec<Input>,
) {
    let original_buffers = state.buffers.buffers();
    let mut unedited_buffer_states: g_i::Map<EditorBuffer> = g_i::Map::with_capacity(original_buffers.len());
    {
        let state = original_buffers.index_state();
        for (i, buffer) in original_buffers.iter_with_indexes() {
            unedited_buffer_states.insert(state, i, buffer.clone());
        }
    }

    let buffer_count = state.buffers.len();
    dbg!(&unedited_buffer_states, buffer_count);
    let mut expected_edited_states: g_i::Map<bool> = g_i::Map::with_capacity(buffer_count);

    {
        let buffers = state.buffers.buffers();
        let index_state= buffers.index_state();
        for (i, _) in buffers.iter_with_indexes() {
            expected_edited_states.insert(index_state, i, false);
        }
    }

    for (i, input) in inputs.into_iter().enumerate() {
        dbg!(i, &input);
        let index_state = state.buffers.buffers().index_state();
        u!{Input}
        match input {
            AddOrSelectBuffer(ref name, ref data) => {
                if state.buffers.index_with_name(name).is_none() {
                    unedited_buffer_states.insert(
                        index_state,
                        state.buffers.append_index(),
                        EditorBuffer::new(name.clone(), data.clone()),
                    );
                    dbg!(&unedited_buffer_states);
                }
            },

            _ => {}
        }

        let (view, _) = update_and_render(&mut state, input);
        dbg!(&view.edited_transitions);
        let index_state = state.buffers.buffers().index_state();
        
        for (i, transition) in view.edited_transitions {
            u!{EditedTransition}
            match transition {
                ToEdited => {
                    expected_edited_states.insert(index_state, i, true);
                }
                ToUnedited => {
                    expected_edited_states.insert(index_state, i, false);
                }
            }
        }
    }

    dbg!(&state.buffers);
    assert_eq!(
        expected_edited_states.len(),
        state.buffers.len(), 
        "expected_edited_states len does not match state.buffers. expected_edited_states: {:#?}",
        expected_edited_states
    );

    for (i, is_edited) in expected_edited_states {
        let buffers = state.buffers.buffers();
        let actual_data: String = buffers.get(i).expect("actual_data was None").into();
        let original_data: Option<String> = unedited_buffer_states
            .get(buffers.index_state(), i)
            .map(|s| s.into());
        if is_edited {
            assert_ne!(
                Some(actual_data),
                original_data,
                "({:?}, {:?})",
                i,
                is_edited
            );
        } else {
            assert_eq!(
                actual_data,
                original_data
                    .expect(&format!("original_data was None ({:?}, {:?})", i, is_edited)),
                "({:?}, {:?})",
                i,
                is_edited
            );
        }
    }
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_a_path_file_is_added_then_the_selection_is_changed() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "¡".to_owned()),
            AdjustBufferSelection(Move(Left)),
            DeleteLines,
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_a_path_file_is_added_then_the_selection_is_changed_reduction() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "¡".to_owned()),
            AdjustBufferSelection(Move(Left)),
            DeleteLines,
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_a_path_file_is_added_then_the_selection_is_changed_sanity_check() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove}
    assert_eq!(
        format!("{:?}", AddOrSelectBuffer(Path(".fakefile".into()), "¡".to_owned())),
        "AddOrSelectBuffer(Path(\".fakefile\"), \"¡\")".to_owned()
    );
}
