use super::*;
use macros::{dbg};
use text_buffer::Editedness;

proptest!{
    #[ignore] // took 9.313s
    #[test]
    fn from_an_arb_state(
        state in arb::state(),
        inputs in proptest::collection::vec(arb::input(), 0..=16),
    ) {
        on(
            state,
            inputs
        )
    }
}

proptest!{
    #[test]
    fn from_a_default_state(
        inputs in proptest::collection::vec(arb::input(), 0..=16),
    ) {
        on(
            d!(),
            inputs
        )
    }
}

proptest!{
    #[ignore] // took 7.410s
    #[test]
    fn with_heavy_saving(
        state in arb::state(),
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        on(
            state,
            inputs
        )
    }
}

proptest!{
    #[ignore] // took 7.226s
    #[test]
    fn with_heavy_saving_from_editor_buffers(
        buffers in arb::editor_buffers(),
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        on(
            arb::state_from_editor_buffers(buffers),
            inputs
        )
    }
}

proptest!{
    #![proptest_config(ProptestConfig {
        source_file: Some(file!()),
        failure_persistence: Some(Box::new(proptest::test_runner::FileFailurePersistence::WithSource("regressions"))),
        .. ProptestConfig::default()
    })]
    #[test]
    fn with_heavy_saving_when_there_is_one_path_buffer_and_one_scratch_buffer(
        buffers in arb::editor_buffers_with_one_path_one_scratch(),
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        on(
            arb::state_from_editor_buffers(buffers),
            inputs
        )
    }

    #[test]
    fn with_heavy_saving_when_there_is_one_default_path_buffer_and_one_scratch_buffer(
        buffers in arb::editor_buffers_with_one_default_path_one_scratch(),
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        on(
            arb::state_from_editor_buffers(buffers),
            inputs
        )
    }

    #[ignore] // took 7.742s
    #[test]
    fn with_heavy_saving_when_the_hash_starts_blank(
        buffers in arb::editor_buffers_blank_hash(),
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        on(
            arb::state_from_editor_buffers(buffers),
            inputs
        )
    }
}

proptest!{
    #[test]
    fn with_heavy_saving_from_a_default_state(
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        on(
            d!(),
            inputs
        )
    }
}

proptest!{
    #[test]
    fn with_full_saving_from_a_default_state(
        inputs in proptest::collection::vec(
            arb::saved_as(),
            0..=16
        ),
    ) {
        on(
            d!(),
            inputs
        )
    }
}

// This test was written and subsequently edited down in order to debug a assert
// failing inside tree-sitter, which caused an abort.
proptest!{
    #[test]
    fn given_this_set_of_possible_inputs(
        input1 in arb::input(),
        index_state in arb::g_i_state_with_default_invalidation(),
    ) {
        u!{Input}
        on(
            d!(),
            vec![input1, SavedAs(index_state.new_index(d!()), ".fakefile".into())]
        )
    }
}

// This test was also written in order to debug that assert failing.
proptest!{
    #[test]
    fn given_this_smaller_set_of_possible_inputs(
        index_state in arb::g_i_state_with_default_invalidation(),
    ) {
        u!{Input}
        on(
            d!(),
            vec![NextLanguage, SavedAs(index_state.new_index(d!()), ".fakefile".into())]
        )
    }
}

proptest!{
    #[test]
    fn after_this_paste(
        buffers in arb::editor_buffers_with_one_path_one_scratch(),
    ) {
        on(
            arb::state_from_editor_buffers(buffers),
            vec![Input::Paste(Some("¡".to_string()))]
        )
    }
}

#[test]
fn in_the_zero_case() {
    on(
        d!(),
        d!()
    )
}

#[test]
fn after_inserting_the_letter_a() {
    u!{Input}
    on(
        d!(),
        vec![
            Insert('a'),
        ]
    )
}

#[test]
fn if_we_select_then_tab_in() {
    u!{BufferIdKind, BufferName, Input}
    on(
        d!(),
        vec![
            SelectBuffer(BufferId { kind: Find, index: d!() }),
            TabIn
        ]
    )
}

#[test]
fn if_we_save_a_new_file() {
    u!{BufferIdKind, BufferName, Input}
    let state: g_i::State = d!();
    on(
        d!(),
        vec![
            SavedAs(state.new_index(g_i::IndexPart::or_max(1)), ".fakefile".into()),
        ]
    )
}

#[test]
fn if_we_insert_numbers_delete_then_redo() {
    u!{Input}
    on(
        d!(),
        vec![InsertNumbersAtCursors, Delete, Redo]
    )
}

#[test]
fn if_we_open_or_select_a_buffer() {
    u!{Input}
    on(
        d!(),
        vec![OpenOrSelectBuffer(".fakefile".into())]
    )
}

#[test]
fn if_we_insert_then_report_a_file_was_saved_at_the_default_index() {
    u!{Input}
    on(
        d!(),
        vec![
            InsertNumbersAtCursors,
            SavedAs(d!(), ".fakefile".into())
        ]
    )
}

#[test]
fn if_we_insert_then_report_a_file_was_saved_at_index_1() {
    u!{Input}
    let state: g_i::State = d!();
    on(
        d!(),
        vec![
            InsertNumbersAtCursors,
            SavedAs(state.new_index(g_i::IndexPart::or_max(1)), ".fakefile".into())
        ]
    )
}

#[test]
fn in_this_generated_heavy_saving_case() {
    u!{Input}
    let state: g_i::State = d!();
    on(
        d!(),
        vec![
            SavedAs(state.new_index(g_i::IndexPart::or_max(0)), "&¥:&q\"N\u{baccd}.fakefile".into()),
            SavedAs(state.new_index(g_i::IndexPart::or_max(7)), "\u{202e}𪫩\u{feff}Ѩ\u{4ac61}*\t3\u{e30e1}.{0.fakefile".into()),
            SavedAs(state.new_index(g_i::IndexPart::or_max(3)), "齂🕴!ヵ..fakefile".into()),
        ]
    )
}

#[test]
fn in_the_default_add_or_select_buffer_then_go_to_case() {
    u!{BufferName, Input}

    on(
        d!(),
        vec![AddOrSelectBufferThenGoTo(Scratch(0), "".to_string(), pos!{l 0 o 0})]
    )
}

#[test]
fn if_we_search_for_the_empty_string() {
    u!{BufferIdKind, Input}
    on(
        d!(),
        vec![
            SelectBuffer(BufferId { kind: Find, index: d!() }),
            SubmitForm
        ]
    )
}

#[test]
fn if_a_file_is_added_to_a_blank_state() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![AddOrSelectBuffer(Path(".fakefile".into()), "".to_owned())]
    )
}

#[test]
fn if_a_file_is_added_to_a_blank_state_then_becomes_unedited_later() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "".to_owned()),
            Insert('A'),
            Delete
        ]
    )
}

#[test]
fn if_a_scratch_file_is_added_then_a_path_file_is_added() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            NewScratchBuffer(Option::None),
            AddOrSelectBuffer(Path(".fakefile".into()), "".to_owned())
        ]
    )
}

#[test]
fn if_an_empty_path_file_is_added() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "".to_owned()),
        ]
    )
}

#[test]
fn if_a_path_file_is_added_with_an_a() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "a".to_owned()),
        ]
    )
}

#[test]
fn if_a_path_file_is_added_with_this_non_ascii_content() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "¡".to_owned()),
        ]
    )
}

#[test]
fn if_a_path_file_is_added_then_the_selection_is_changed() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "¡".to_owned()),
            AdjustBufferSelection(Move(Left)),
            DeleteLines,
        ]
    )
}

#[test]
fn if_a_path_file_is_added_then_the_selection_is_changed_twice() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "".to_owned()),
            AdjustBufferSelection(Move(Left)),
            AdjustBufferSelection(Move(Left)),
        ]
    )
}

#[test]
fn if_a_file_is_edited_then_saved_then_edited() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove}

    let state: g_i::State = d!();
    let path: PathBuf = ".fakefile".into();
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(path.clone()), "".to_owned()),
            Insert('a'),
            SavedAs(
                state.new_index(g_i::IndexPart::or_max(1)),
                path.clone()
            ),
            Insert('b'),
        ]
    )
}

#[test]
fn if_a_non_blank_file_is_added_then_edited() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "A".to_owned()),
            CloseBuffer(d!()),
            Insert('B'),
        ]
    )
}

#[test]
fn if_the_default_scratch_file_is_added_then_delete_is_inputted() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Scratch(d!()), "0".into()),
            Delete,
        ]
    )
}

#[test]
fn if_the_default_scratch_file_is_added_then_the_content_is_deleted() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Scratch(d!()), "0".into()),
            MoveAllCursors(Move::ToBufferEnd),
            Delete,
        ]
    )
}

#[test]
fn if_the_default_scratch_file_is_just_added() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Scratch(d!()), "0".into())
        ]
    )
}

#[test]
fn if_a_new_scratch_file_is_added_then_delete_is_inputted() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Scratch(1), "0".into()),
            Delete,
        ]
    )
}

#[test]
fn if_a_new_scratch_file_is_added_then_the_content_is_deleted() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Scratch(1), "0".into()),
            MoveAllCursors(Move::ToBufferEnd),
            Delete,
        ]
    )
}

#[test]
fn if_the_default_scratch_file_is_added_blank_then_0_is_inserted() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Scratch(d!()), "".into()),
            Insert('0'),
        ]
    )
}

#[test]
fn if_a_path_file_is_added_then_delete_is_inputted() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "0".into()),
            Delete
        ]
    )
}

#[test]
fn if_a_path_file_is_added_then_the_content_is_deleted() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "0".into()),
            MoveAllCursors(Move::ToBufferEnd),
            Delete
        ]
    )
}

#[test]
fn if_a_new_scratch_file_with_an_a_is_added() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            NewScratchBuffer(Some("a".into())),
        ]
    )
}


#[test]
fn if_a_new_scratch_file_with_an_a_is_followed_by_an_undo() {
    u!{BufferName, Input}
    on(
        d!(),
        vec![
            NewScratchBuffer(Some("a".into())),
            Undo
        ]
    )
}


#[test]
fn in_this_case_where_larger_chars_are_pasted() {
    u!{BufferName, Input}
    on(
        arb::state_from_editor_buffers(EditorBuffers::new((d!(), ""))),
        vec![
            Paste(Some("¡".to_string())),
        ]
    )
}

#[test]
fn in_this_insert_numbers_at_cursors_case() {
    u!{BufferName, Input}
    let name = Path(".fakefile".into());
    on(
        arb::state_from_editor_buffers(EditorBuffers::new((name.clone(), String::new()))),
        vec![
            InsertNumbersAtCursors, 
            AddOrSelectBuffer(name, String::new())
        ]
    )
}

#[test]
fn if_we_do_this_cut() {
    u!{BufferName, Input}
    let mut state = arb::state_from_editor_buffers(EditorBuffers::new((d!(), "")));
    
    update_and_render(&mut state, Insert('a'));
    update_and_render(&mut state, SelectAll);

    on(
        state,
        vec![
            Cut
        ]
    )
}

#[test]
fn if_we_show_an_error_from_a_default_state() {
    u!{BufferName, Input}
    let state = d!();

    on(
        state,
        vec![
            ShowError("Test Error".to_owned())
        ]
    )
}

/// This test predicate simulates what we expected clients to do if they want to keep track 
/// of which buffers are currently different from what is on disk. This is a little 
/// complicated because the editor is the one who knows about the undo history, and the 
/// client is the one who knows about when things are saved to disk or not.
// We put this below the tests that use it so I don't need to scroll so damn much.
fn on(
    mut state: State,
    inputs: Vec<Input>,
) { 
    u!{BufferName}
    // After finding I need to come and fix failures of this test over and over 
    // again, I find myself wonderingif it would not be better to just move the
    // responsibility for tracking whether a buffer is saved into the editor thread.
    // That way seems way simpler, although potentially at the cost of an extra 
    // `Input` vairiant or two. Making that change would remove the need for this 
    // particular test predicate altogther, but we could change the tests into ones
    // that simulate a filesystem responding with the fact that the file is saved,
    // after some time, then checks whether the simulated state of the 
    // filesystem matches the state of the buffers.


    let original_buffers = state.buffers.buffers();
    let mut initial_buffer_states: g_i::Map<EditorBuffer> = g_i::Map::with_capacity(original_buffers.len());
    {
        let state = original_buffers.index_state ();
        for (i, buffer) in original_buffers.iter_with_indexes() {
            initial_buffer_states.insert(state, i, buffer.clone());
        }
    }

    // If a map tracks only the transitions from the view, it should be able 
    // to tell which buffers are edited and which are not, given the inital state
    // of the map corresponded to the edited state of the buffers in the first place.
    let mut expected_editedness_map: g_i::Map<Editedness> = 
        g_i::Map::with_capacity(original_buffers.len());
    {
        let index_state = original_buffers.index_state ();
        for (i, buffer) in original_buffers.iter_with_indexes() {
            expected_editedness_map.insert(
                index_state,
                i,
                buffer.text_buffer.editedness()
            );
        }
    }

    let mut saved_as_names = Vec::with_capacity(inputs.len() / 2);

    for input in inputs {
        u!{Input}
        let index_state = state.buffers.buffers().index_state();
        match input {
            AddOrSelectBuffer(ref name, ref data) => {
                if state.buffers.index_with_name(name).is_none() {
                    // I'm not certain, but I think that checking what type of 
                    // buffer name we have here is fine, since for the purposes
                    // of this test we only care whether the editedness is correct
                    match name {
                        Scratch(_) => {
                            initial_buffer_states.insert(
                                index_state,
                                state.buffers.buffers().append_index(),
                                EditorBuffer::new(name.clone(), ""),
                            );
                        }
                        Path(_) => {
                            initial_buffer_states.insert(
                                index_state,
                                state.buffers.buffers().append_index(),
                                EditorBuffer::new(name.clone(), data.clone()),
                            );
                        }
                    }
                    dbg!(&initial_buffer_states);
                }
            },
            NewScratchBuffer(ref data) => {
                initial_buffer_states.insert(
                    index_state,
                    state.buffers.buffers().append_index(),
                    EditorBuffer::new(
                        BufferName::Scratch(state.next_scratch_buffer_number()),
                        data.clone().unwrap_or_default()
                    )
                )
            }, 
            SavedAs(index, ref p) => {
                dbg!("SavedAs()", index, expected_editedness_map.get(index_state, index).is_some());
                // We need to trust the platform layer to only call
                // this when the file is saved under the given path.
                if expected_editedness_map.get(index_state, index).is_some() {
                    let buffer = initial_buffer_states
                        .get_mut(index_state, index)
                        .expect("SavedAs invalid initial_buffer_states index");

                    *buffer = state.buffers.buffers()
                        .get(index)
                        .expect("SavedAs invalid state.buffers.buffers index")
                        .clone();

                    saved_as_names.push(Path(p.clone()));
                }
            },
            ShowError(ref error_string) => {
                initial_buffer_states.insert(
                    index_state,
                    state.buffers.buffers().append_index(),
                    EditorBuffer::new(
                        BufferName::Scratch(state.next_scratch_buffer_number()),
                        error_string.clone()
                    )
                )
            }

            _ => {}
        }

        let (view, _) = update_and_render(&mut state, input);
        dbg!(&view.edited_transitions);
        let index_state = state.buffers.buffers().index_state();
        
        for (i, transition) in view.edited_transitions {
            u!{Editedness, EditedTransition}
            match transition {
                ToEdited => {
                    dbg!(format!("expected_editedness_map.insert Edited"));
                    expected_editedness_map.insert(index_state, i, Edited);
                }
                ToUnedited => {
                    dbg!(format!("expected_editedness_map.insert Unedited"));
                    expected_editedness_map.insert(index_state, i, Unedited);
                }
            }
        }
        expected_editedness_map.migrate_all(index_state);
        initial_buffer_states.migrate_all(index_state);
    }

    assert_eq!(
        expected_editedness_map.len(),
        state.buffers.len(),
        "expected_editedness_map len does not match state.buffers. expected_editedness_map: {:#?}",
        expected_editedness_map
    );

    let mut expected_editedness_map: Vec<_> = expected_editedness_map.into_iter().collect();

    expected_editedness_map.sort_by_key(|p| p.0);

    for (i, expected_editedness) in expected_editedness_map {
        u!{Editedness}
        let buffers = state.buffers.buffers();
        
        let (actual_name, actual_editedness, actual_data): (BufferName, Editedness, String) = 
            buffers
            .get(i)
            .map(|s: &EditorBuffer| (
                s.name.clone(),
                s.text_buffer.editedness(),
                String::from(s)
            ))
            .expect("actual_data was None");
        let (original_name, original_editedness, original_data): (BufferName, Editedness, String) = 
            initial_buffer_states
            .get(buffers.index_state(), i)
            .map(|s: &EditorBuffer| (
                s.name.clone(),
                s.text_buffer.editedness(),
                String::from(s)
            ))
            .expect(&format!("original_data was None ({:?}, {:?})", i, expected_editedness));
        
        assert_eq!(actual_editedness, expected_editedness, "actual_editedness did not match expected_editedness");

        // TODO remove this whole if statement
        if let BufferName::Scratch(_) = actual_name {
            use std::time::{Duration, SystemTime};
            if SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            < Duration::from_secs(1598990000) {
                continue;
            }
        }

        match (original_editedness, expected_editedness) {
            (Edited, Edited) => {
                // In this case the buffer may or may not be the same as it was, so 
                // we cannot assert anything.
            },
            (Edited, Unedited) => {
                if original_name == actual_name {
                    assert_ne!(
                        actual_data,
                        original_data,
                        "({:?}, {:?}) the data was reported going from edited to unedited, but the data matches.",
                        i,
                        expected_editedness
                    );
                } else {
                    // As of this writing, the only way that `actual_name` may not
                    // equal `original_name`, is if there was a `SavedAs` input.
                    let ended_as_path = matches!(actual_name, Path(_));
                    let was_saved_as = saved_as_names.contains(&actual_name);
                    assert!(
                        ended_as_path && was_saved_as,
                        "original_name ({:?}) does not match actual_name ({:?}) for an unexpected reason! {}{}",
                        original_name,
                        actual_name,
                        if ended_as_path {
                            ""
                        } else {
                            "buffer name was a Path variant"
                        },
                        if was_saved_as {
                            ""
                        } else {
                            "buffer name was not recorded in SavedAs case"
                        },
                    )
                }
            },
            (Unedited, Edited) => {
                assert_ne!(
                    actual_data,
                    original_data,
                    "({:?}, {:?}) the data was reported going from unedited to edited, but the data matches.",
                    i,
                    expected_editedness
                );
            },
            (Unedited, Unedited) => {
                assert_eq!(
                    actual_data,
                    original_data,
                    "({:?}, {:?}) the data was reported as not edited, but the data does not match.",
                    i,
                    expected_editedness
                );
            },
        }
    }
}

