use super::*;
use macros::{dbg};


proptest!{
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
        state in arb::g_i_state_with_default_invalidation(),
    ) {
        u!{Input}
        on(
            d!(),
            vec![input1, SavedAs(state.new_index(d!()), ".fakefile".into())]
        )
    }
}

// This test was also written in order to debug that assert failing.
proptest!{
    #[test]
    fn given_this_smaller_set_of_possible_inputs(
        state in arb::g_i_state_with_default_invalidation(),
    ) {
        u!{Input}
        on(
            d!(),
            vec![NextLanguage, SavedAs(state.new_index(d!()), ".fakefile".into())]
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
fn if_we_insert_then_report_a_file_was_saved_at_the_first_index() {
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

/// This test predicate simulates what we expected clients to do if they want to keep track 
/// of which buffers are currently different from what is on disk. This is a little 
/// complicated because the editor is the one who knows about the undo history, and the 
/// client is the one who knows about when things are saved to disk or not.
// We put this below the tests that use it so I don't need to scroll so damn much.
fn on(
    mut state: State,
    inputs: Vec<Input>,
) { 
    // we ensure that every buffer starts off unedited since that makes the 
    // expected editedness easier to track.
    let selected_index = state.buffers.current_index();

    let indexes: Vec<_> = state.buffers.iter_with_indexes()
        .map(|(i, _)| i)
        .collect();

    for i in indexes {
        state.buffers.set_current_index(i);

        let buffer = state.buffers.get_current_buffer_mut();
        buffer.text_buffer.set_unedited();
    }

    state.buffers.set_current_index(selected_index);

    let original_buffers = state.buffers.buffers();
    let mut unedited_buffer_states: g_i::Map<EditorBuffer> = g_i::Map::with_capacity(original_buffers.len());
    {
        let state = original_buffers.index_state ();
        for (i, buffer) in original_buffers.iter_with_indexes() {
            unedited_buffer_states.insert(state, i, buffer.clone());
        }
    }

    let buffer_count = state.buffers.len();
    dbg!(&unedited_buffer_states, buffer_count);
    let mut expected_edited_states: g_i::Map<bool> = g_i::Map::with_capacity(buffer_count);

    {
        let buffers = state.buffers.buffers();
        let index_state = buffers.index_state();
        for (i, _) in buffers.iter_with_indexes() {
            expected_edited_states.insert(index_state, i, false);
        }
    }

    for input in inputs {
        let index_state = state.buffers.buffers().index_state();
        u!{Input}
        match input {
            AddOrSelectBuffer(ref name, ref data) => {
                if state.buffers.index_with_name(name).is_none() {
                    unedited_buffer_states.insert(
                        index_state,
                        state.buffers.buffers().append_index(),
                        EditorBuffer::new(name.clone(), data.clone()),
                    );
                    dbg!(&unedited_buffer_states);
                }
            },
            NewScratchBuffer(ref data) => {
                unedited_buffer_states.insert(
                    index_state,
                    state.buffers.buffers().append_index(),
                    EditorBuffer::new(
                        BufferName::Scratch(state.next_scratch_buffer_number()),
                        data.clone().unwrap_or_default()
                    )
                )
            }, 
            SavedAs(index, _) => {
                dbg!("SavedAs()", index, expected_edited_states.get(index_state, index).is_some());
                // We need to trust the platform layer to only call
                // this when the file is saved under the given path.
                if expected_edited_states.get(index_state, index).is_some() {
                    
                    let buffer = unedited_buffer_states
                        .get_mut(index_state, index)
                        .expect("SavedAs invalid unedited_buffer_states index");
                    *buffer = state.buffers.buffers()
                        .get(index)
                        .expect("SavedAs invalid state.buffers.buffers index")
                        .clone();
                }
            }

            _ => {}
        }

        let (view, _) = update_and_render(&mut state, input);
        dbg!(&view.edited_transitions);
        let index_state = state.buffers.buffers().index_state();
        
        for (i, transition) in view.edited_transitions {
            u!{EditedTransition}
            match transition {
                ToEdited => {
                    dbg!("expected_edited_states.insert", index_state, i, true);
                    expected_edited_states.insert(index_state, i, true);
                }
                ToUnedited => {
                    dbg!("expected_edited_states.insert", index_state, i, false);
                    expected_edited_states.insert(index_state, i, false);
                }
            }
        }
        expected_edited_states.migrate_all(index_state);
        unedited_buffer_states.migrate_all(index_state);
    }

    dbg!(&state.buffers, &expected_edited_states);
    assert_eq!(
        expected_edited_states.len(),
        state.buffers.len(), 
        "expected_edited_states len does not match state.buffers. expected_edited_states: {:#?}",
        expected_edited_states
    );

    let mut expected_edited_states: Vec<_> = expected_edited_states.into_iter().collect();

    expected_edited_states.sort_by_key(|p| p.0);

    for (i, is_edited) in expected_edited_states {
        let buffers = state.buffers.buffers();
        dbg!(i, is_edited);
        let actual_data: String = buffers.get(i).expect("actual_data was None").into();
        let original_data: Option<String> = unedited_buffer_states
            .get(buffers.index_state(), i)
            .map(|s| s.into());
        if is_edited {
            assert_ne!(
                Some(actual_data),
                original_data,
                "({:?}, {:?}) the data was reported as edited, but the data matches.",
                i,
                is_edited
            );
        } else {
            assert_eq!(
                actual_data,
                original_data
                    .expect(&format!("original_data was None ({:?}, {:?})", i, is_edited)),
                "({:?}, {:?}) the data was reported as not edited, but the data does not match.",
                i,
                is_edited
            );
        }
    }
}


