use super::*;
use macros::{u, dbg};
use search::{SearchResults};

fn shows_the_cursor_when_pressing_ctrl_home_on(text: &str, buffer_xywh: TextBoxXYWH, char_dim: CharDim) {
    let mut state: State = text.into();
    state.buffer_xywh = buffer_xywh;
    dbg!(state.buffer_xywh);
    state.font_info = FontInfo {
        text_char_dim: char_dim,
        status_char_dim: char_dim,
        tab_char_dim: char_dim,
        find_replace_char_dim: char_dim,
    };

    update_and_render(&mut state, Input::MoveAllCursors(Move::ToBufferEnd));
    dbg!(state.buffer_xywh);
    {
        let buffer = get_text_buffer_mut!(state, BufferIdKind::Text).unwrap();
        assert_eq!(
            *buffer.borrow_cursors().first(), cur!{pos!{l 0, o text.len()}},
            "*** Cursor Precondition failure! ***"
        );
        assert_ne!(buffer.scroll.x, 0.0, "*** Scroll X Precondition failure! ***");
    }
    

    update_and_render(&mut state, Input::MoveAllCursors(Move::ToBufferStart));
    dbg!(state.buffer_xywh);
    let buffer = get_text_buffer_mut!(state, BufferIdKind::Text).unwrap();
    assert_eq!(buffer.scroll.x, 0.0, "buffer.scroll.x");
    assert_eq!(buffer.scroll.y, 0.0, "buffer.scroll.y");
}

#[test]
fn shows_the_cursor_when_pressing_ctrl_home_in_this_case() {
    shows_the_cursor_when_pressing_ctrl_home_on(
        CURSOR_SHOW_TEXT,
        example_tbxywh(),
        example_char_dim()
    );
}

#[test]
fn shows_the_cursor_when_searching_in_this_case() {
    let example_char_dim = example_char_dim();
    let example_tbxywh = example_tbxywh();
    // Arrange
    let mut text = String::new();
    
    // The goal is enough text that it is definitely offscreen
    for i in 0..(example_tbxywh.wh.h.get() / example_char_dim.h.get()) as u128 * 8 {
        for _ in 0..i {
            text.push('.');
        }

        text.push('\n');
    }

    const NEEDLE: &str = "needle";
    text.push_str(NEEDLE);

    let mut state: State = text.into();
    state.buffer_xywh = example_tbxywh;
    state.font_info = example_font_info();

    {
        let buffer = get_text_buffer_mut!(state, BufferIdKind::Text).unwrap();
        assert_eq!(
            *buffer.borrow_cursors().first(), cur!{pos!{l 0 o 0}},
            "*** Cursor Precondition failure! ***"
        );
        assert_eq!(buffer.scroll.y, 0.0, "*** Scroll Y Precondition failure! ***");
    }
    

    update_and_render(&mut state, Input::SetMenuMode(MenuMode::FindReplace(d!())));

    let first_needle_char = NEEDLE.chars().next().unwrap();
    update_and_render(&mut state, Input::Insert(first_needle_char));
    assert_eq!(
        state.find.borrow_rope().chars().next().expect("*** find was empty! Precondition failure! ***"),
        first_needle_char
    );

    // Act
    update_and_render(&mut state, Input::SubmitForm);

    // Assert
    let buffer = get_text_buffer_mut!(state, BufferIdKind::Text).unwrap();
    assert_ne!(buffer.scroll.y, 0.0);
}

fn passes_preconditions(text: &str, buffer_xywh: TextBoxXYWH, char_dim: CharDim) -> bool {
    let mut state: State = text.into();
    state.buffer_xywh = buffer_xywh;
    state.font_info = FontInfo {
        text_char_dim: char_dim,
        status_char_dim: char_dim,
        tab_char_dim: char_dim,
        find_replace_char_dim: char_dim,
    };
    

    dbg!(get_text_buffer_mut!(state));

    update_and_render(&mut state, Input::MoveAllCursors(Move::ToBufferEnd));

    dbg!(get_text_buffer_mut!(state));

    let buffer = get_text_buffer_mut!(state).unwrap();

    let passed = *buffer.borrow_cursors().first() == cur!{pos!{l 0, o text.len()}}
    && buffer.scroll.x != 0.0;

    if !passed {
        panic!(
            "fails preconditions:\n{} && {}\n\nbuffer: {:?}\n\nbuffer_xywh: {:?}\n\nchar_dim: {:?}\n\n", 
            *buffer.borrow_cursors().first() == cur!{pos!{l 0, o text.len()}},
            buffer.scroll.x != 0.0,
            buffer,
            buffer_xywh,
            char_dim,
        );
    }

    passed
}

#[test]
fn shows_the_cursor_when_pressing_ctrl_home() {
    use proptest::test_runner::{TestRunner, TestCaseError};
    let mut runner = TestRunner::default();

    runner.run(&(
        arb::abs_length(),
        arb::abs_length(),
        arb::abs_length(),
        arb::abs_length(),
    ), |(box_w, box_h, w, h)| {

        let w_max = box_w.halve();
        let w_min = box_w / abs::Ratio::from(CURSOR_SHOW_TEXT.len() / 2);

        let w = if w > w_max {
            w_max
        } else if w >= w_min {
            w
        } else {
            // NaN ends up here
            w_min
        };

        let h_max = box_h.halve();
        let h = if h > h_max {
            h_max
        } else {
            // NaN ends up here
            h
        };

        if passes_preconditions(
            CURSOR_SHOW_TEXT,
            tbxywh!(raw 0.0, 0.0, box_w, box_h),
            CharDim { w, h }
        ) {
            shows_the_cursor_when_pressing_ctrl_home_on(
                CURSOR_SHOW_TEXT,
                tbxywh!(raw 0.0, 0.0, box_w, box_h),
                CharDim { w, h },
            );
            Ok(())
        } else {
            Err(TestCaseError::Reject("failed preconditions".into()))
        }
    }).unwrap();
}

#[test]
/// The property this test checks for makes it easier for editor clients to implement keyboard navigation by examining
/// the cursors. It also looks mildly nicer.
fn resets_the_cursor_states_in_this_case() {
    // Arrange
    let mut state: State = String::new().into();
    state.buffer_xywh = example_tbxywh();
    state.font_info = example_font_info();

    update_and_render(&mut state, Input::SetMenuMode(MenuMode::FileSwitcher));
    update_and_render(&mut state, Input::MoveAllCursors(Move::Down));

    {
        let buffer = get_text_buffer_mut!(state, BufferIdKind::FileSwitcher).unwrap();
        for c in buffer.borrow_cursors().iter() {
            assert_eq!(
                c.state,
                CursorState::PressedAgainstWall(Move::Down),
                "*** Cursor Precondition failure! ***"
            );
        }
    }

    // Act
    update_and_render(&mut state, Input::MoveAllCursors(Move::Up));
    // We expect the view returned from the to `Move::Up` render to allow the client
    // to trigger a SelectBuffer input, given that their UI places the input box 
    // above the results list. We should not require clients to do this, but if they 
    // do this it should work.

    update_and_render(&mut state, Input::SelectBuffer(b_id!(BufferIdKind::FileSwitcher, d!())));

    // Assert
    let buffer = get_text_buffer_mut!(state, BufferIdKind::FileSwitcher).unwrap();
    for c in buffer.borrow_cursors().iter() {
        assert_eq!(
            c.state,
            d!(),
        );
    }
}

fn places_the_cursor_correctly_after_inserting_after_a_find_between_two_other_chars_on(
    ch1: char,
    ch2: char,
    ch3: char,
    ch4: char,
) {
    u!{Input};
    u!{MenuMode};
    u!{FindReplaceMode};

    // Arrange
    let mut state: State = String::new().into();

    update_and_render(&mut state, Insert(ch1));
    update_and_render(&mut state, Insert(ch2));
    update_and_render(&mut state, Insert(ch3));

    update_and_render(&mut state, MoveAllCursors(Move::Left));
    let (view, _) = update_and_render(&mut state, ExtendSelectionForAllCursors(Move::Left));
    let cursor = single_cursor_view(&view);
    assert_eq!(cursor.position, pos!{l 0 o 1});

    update_and_render(&mut state, SetMenuMode(FindReplace(CurrentFile)));
    update_and_render(&mut state, SubmitForm);
    update_and_render(&mut state, CloseMenuIfAny);

    assert_eq!(state.menu_mode, MenuMode::Hidden);

    update_and_render(&mut state, MoveAllCursors(Move::Right));
    let (view, _) = update_and_render(&mut state, MoveAllCursors(Move::Right));
    let cursor = single_cursor_view(&view);
    assert_eq!(cursor.position, pos!{l 0 o 3});

    // Act
    let (view, _) = update_and_render(&mut state, Insert(ch4));

    // Assert
    let cursor = single_cursor_view(&view);
    assert_eq!(cursor.position, pos!{l 0 o 4});
}

proptest!{
    #[test]
    fn places_the_cursor_correctly_after_inserting_after_a_find_between_two_other_chars(
        ch1 in non_line_break_char(),
        ch2 in non_line_break_char(),
        ch3 in non_line_break_char(),
        ch4 in non_line_break_char(),
    ) {
        places_the_cursor_correctly_after_inserting_after_a_find_between_two_other_chars_on(
            ch1,
            ch2,
            ch3,
            ch4,
        )
    }
}

#[test]
fn retains_the_scratch_buffer_data_if_the_name_does_not_match_after_an_insert_then_a_saved_as_with_the_same_index() {
    u!{Input}
    let mut state: State = d!();
    let inputs = vec![Insert('a'), SavedAs(d!(), ".fakefile".into())];

    for input in inputs {
        let _ = update_and_render(&mut state, input);
    }

    assert_eq!(usize::from(state.buffers.len()), 1usize);

    assert_eq!(first_editor_buffer_char(&state).unwrap(), 'a');
}

#[test]
fn retains_the_path_buffer_data_if_the_name_does_not_match_after_an_insert_then_a_saved_as_with_the_same_index() {
    u!{Input}
    let mut state: State = d!();
    let inputs = vec![SavedAs(d!(), "a.fakefile".into()), Insert('a'), SavedAs(d!(), ".fakefile".into())];

    for input in inputs {
        let _ = update_and_render(&mut state, input);
    }

    assert_eq!(usize::from(state.buffers.len()), 1usize);

    assert_eq!(first_editor_buffer_char(&state).unwrap(), 'a');
}

fn keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state_on(
    mut state: State,
    inputs: Vec<Input>,
) {
    assert_eq!(
        state.buffers.buffers().index_state(),
        state.view.buffers.index_state(),
        "Precondition failure!"
    );

    for (i, input) in inputs.into_iter().enumerate() {
        let input_clone = input.clone();

        let _ = update_and_render(&mut state, input);

        assert_eq!(
            state.buffers.buffers().index_state(),
            state.view.buffers.index_state(),
            "the index state didn't match on input {}, ({:?})",
            i,
            input_clone,
        );
    }
}

proptest!{
    #[test]
    fn keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state(
        state in arb::state(),
        inputs in proptest::collection::vec(arb::input(), 0..=16),
    ) {
        keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state_on(
            state,
            inputs
        )
    }
}

proptest!{
    #[test]
    fn keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state_if_you_add_then_remove_a_buffer(
        state in arb::state(),
    ) {
        u!{Input}
        keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state_on(
            state,
            vec![NewScratchBuffer(Option::None), CloseBuffer(d!())]
        );
    }
}

#[test]
fn keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state_if_you_add_then_remove_a_buffer_on_a_default_state() {
    u!{Input}
    keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state_on(
        d!(),
        vec![NewScratchBuffer(Option::None), CloseBuffer(d!())]
    );
}

#[test]
fn keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state_if_you_insert_numbers_at_cursors_then_delete_on_a_default_state() {
    u!{Input}
    keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state_on(
        d!(),
        vec![NextLanguage, InsertNumbersAtCursors, Delete]
    );
}

#[test]
fn keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state_if_you_add_then_remove_a_buffer_on_this_state() {
    u!{Input}
    let inputs = vec![NewScratchBuffer(Option::None), CloseBuffer(d!())];

    let mut state = d!();

    update_and_render(
        &mut state,
        inputs[0].clone()
    );
    update_and_render(
        &mut state,
        inputs[1].clone()
    );

    keeps_the_state_buffers_index_state_the_same_as_the_view_buffers_index_state_on(
        state,
        inputs
    );
}

/* I think tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers supercedes this
fn reports_a_change_at_the_correct_edited_index_on(
    state: State,
    input: Input,
) {
    u!{Input}
    let expected = match input {
        None |
        Quit |
        CloseMenuIfAny | 
        ResetScroll |
        ScrollVertically(_) |
        ScrollHorizontally(_) |
        SetSizeDependents(_) |
        MoveAllCursors(_) |
        ExtendSelectionForAllCursors(_) |
        SelectAll |
        SetCursor(_, _) |
        DragCursors(_) |
        SelectCharTypeGrouping(_, _) |
        ExtendSelectionWithSearch |
        Copy | 
        AdjustBufferSelection(_) |
        CloseBuffer(_) |
        NextLanguage |
        SelectBuffer(_) |
        SetMenuMode(_) |
        SubmitForm => {
            vec![]
        },
        Insert(_) |
        Delete |
        DeleteLines |
        SavedAs(_, _) |
        Undo |
        Redo |
        Cut |
        Paste(_) |
        InsertNumbersAtCursors |
        TabIn |
        TabOut
        => {
            vec![state.buffers.current_index()]
        },        AddOrSelectBuffer(ref name, _) => {
            if state.buffers.index_with_name(name).is_some() {
                vec![]
            } else {
                vec![state.buffers.append_index()]
            }
        },
        NewScratchBuffer(_) => {
            vec![state.buffers.append_index()]
        },
        OpenOrSelectBuffer(ref path) => {
            if state.buffers.index_with_name(&BufferName::Path(path.clone())).is_some() {
                vec![]
            } else {
                vec![state.buffers.append_index()]
            }
        },
    };
    
    let (view, _) = update_and_render(&mut state, input);

    assert_eq!(
        view.edited_transitions
            .into_iter()
            .map(|(i, _)| {
                i
            }).collect::<Vec<_>>(),
        expected,
    )
}

/// This is important since clients want to be able to report this information
/// (or information derived from it) to the user.
proptest!{
    #[test]
    fn reports_a_change_at_the_correct_edited_index(
        mut state in arb::state(),
        input in arb::input(),
    ) {
        reports_a_change_at_the_correct_edited_index_on(
            state,
            input
        )
    }
}

#[test]
fn reports_a_change_at_the_correct_edited_index() {
    reports_a_change_at_the_correct_edited_index_on(
        state,
        Input::Undo
    )
}
*/

#[test]
fn does_not_allow_clients_to_enter_text_into_the_find_field_outside_of_find_mode() {
    u!{BufferIdKind, Input}
    let mut state: State = d!();

    assert_eq!(state.menu_mode, MenuMode::Hidden, "Precondition failure.");
    assert_eq!(state.current_buffer_kind, Text, "Precondition failure.");
    assert_eq!(first_char(&state.find), Option::None, "Precondition failure.");
    assert_eq!(first_editor_buffer_char(&state), Option::None, "Precondition failure.");

    let inputs = vec![SelectBuffer(BufferId { kind: Find, index: d!() }), Insert('a')];

    for input in inputs {
        let _ = update_and_render(&mut state, input);
        assert_eq!(state.menu_mode, MenuMode::Hidden);
        assert_eq!(state.current_buffer_kind, Text);
    }

    assert_eq!(usize::from(state.buffers.len()), 1usize);

    assert_eq!(first_char(&state.find), Option::None);
}

fn returns_edited_if_a_file_is_loaded_then_changed_to_a_different_string_on(
    s1: String, s2: String
) {
    assert_ne!(s1, s2, "precondition failure");

    u!{BufferIdKind, BufferName, EditedTransition, Input}
    let mut state: State = d!();

    let _ = update_and_render(&mut state, AddOrSelectBuffer(Path(".fakefile".into()), s1));

    let _ = update_and_render(&mut state, SelectAll);

    let mut last_transition: Option<EditedTransition> = Option::None;

    if s2.len() == 0 { // we know s1 must not be empty.
        let (view, _) = update_and_render(&mut state, Delete);
        for (_, t) in view.edited_transitions.clone() {
            last_transition = Some(t);
        }
    } else {
        for c in s2.chars() {
            let (view, _) = update_and_render(&mut state, Insert(c));
            for (_, t) in view.edited_transitions.clone() {
                last_transition = Some(t);
            }
        }
    }

    assert_eq!(last_transition, Some(ToEdited));
}

/* this is rather slow
proptest!{
    #[test]
    fn returns_edited_if_a_file_is_loaded_then_changed_to_a_different_string(
        (s1, s2) in pub_arb_std::distinct_strings()
    ) {
        returns_edited_if_a_file_is_loaded_then_changed_to_a_different_string_on(
            s1, s2
        );
    }
}
*/

#[test]
fn returns_edited_if_a_file_is_loaded_then_changed_to_a_different_string_in_this_empty_to_non_empty_example() {
    returns_edited_if_a_file_is_loaded_then_changed_to_a_different_string_on(
        "".to_owned(), "123".to_owned()
    );
}

#[test]
fn returns_edited_if_a_file_is_loaded_then_changed_to_a_different_string_in_this_non_empty_to_empty_example() {
    returns_edited_if_a_file_is_loaded_then_changed_to_a_different_string_on(
        "123".to_owned(), "".to_owned()
    );
}

/// We want this to be true because we hope it will make bugs where the same index is sent down
/// twice harder to write. If nothing else, bugs like that will be easier to notice in the debug
/// printouts. Also, if thread channel memory usage ever becomes an issue, if this property holds,
/// we only have to send down the generation once.
// TODO assert all edited_transitions refer to different elements
fn the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_on(
    mut state: State,
    inputs: Vec<Input>,
) {
    for input in inputs {
        dbg!(&input);
        let (view, _) = update_and_render(&mut state, input.clone());
        
        let index_state = state.buffers.buffers().index_state();

        let expected_generation = index_state.get_generation();
        
        for (i, _) in view.edited_transitions.clone() {
            let generation = i.get_generation();
            assert_eq!(
                generation,
                expected_generation,
                "expected all to use {:?}. edited_transitions: {:#?}, input: {:#?}",
                expected_generation,
                &view.edited_transitions,
                input
            );
        }
    }
}

proptest!{
    #[test]
    fn the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers(
        state in arb::state(),
        inputs in proptest::collection::vec(arb::input(), 0..=16),
    ) {
        the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_on(
            state,
            inputs
        )
    }
}

#[test]
fn the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_if_we_add_a_buffer_then_shift_it_left() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove}
    the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "".to_owned()),
            AdjustBufferSelection(Move(Left)),
        ]
    )
}

#[test]
fn the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_during_this_generated_delete_containing_set_of_inputs() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove, MenuMode, ReplaceOrAdd}

    let mut state: State = "🌀".into();
    let buffer = &mut state
        .buffers
        .get_current_buffer_mut();
    
    buffer.text_buffer.set_cursor(cur!{l 0 o 1 s_o 0}, Replace);
    buffer.search_results = SearchResults {
        ranges: vec![(pos!{l 0 o 0}, pos!{l 0 o 80894094724419})],
        ..d!()
    };

    the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_on(
        state,
        vec![
            NextLanguage,
            SetCursor(tbsxy!(1145868901.08979127858765423, 812184193.07755859731696546), Add),
            Cut,
            CloseBuffer(
                arb::g_i_index_from_parts(
                    542558679,
                    g_i::IndexPart::or_max(7)
                )
            ),
            TabIn,
            SetMenuMode(Hidden),
            Undo,
            Delete,
            Redo,
            DeleteLines,
            Cut,
            SelectCharTypeGrouping(
                tbsxy!(-135667381.5168384555727243, 908634525.6051329053007066),
                Replace
            ),
            DeleteLines, 
            SetMenuMode(GoToPosition)
        ]
    )
}

#[test]
fn the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_during_this_generated_delete_containing_set_of_inputs_reduction() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove, MenuMode, ReplaceOrAdd}

    the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_on(
        "🌀".into(),
        vec![
            NextLanguage,
            MoveAllCursors(super::Move::Right),
            SetCursor(d!(), Add),
            Delete,
        ]
    )
}

#[test]
fn the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_during_this_generated_set_of_inputs() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove, MenuMode, ReplaceOrAdd}

    let mut state: State = "0🌀".into();
    state
        .buffers
        .get_current_buffer_mut()
        .text_buffer
        .set_cursor(cur!{l 0 o 2 s_o 0}, Replace);

    the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_on(
        state,
        vec![NextLanguage, SetCursor(tbsxy!(0.0, 0.0), Add), DeleteLines]
    )
}

#[test]
fn the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_during_this_generated_set_of_inputs_reduction() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove, MenuMode, ReplaceOrAdd}

    the_edited_transitions_sent_down_with_the_view_all_use_the_same_generation_as_the_buffers_on(
        "0🌀".into(),
        vec![
            NextLanguage,
            MoveAllCursors(super::Move::Right),
            MoveAllCursors(super::Move::Right),
            SetCursor(tbsxy!(0.0, 0.0), Add),
            DeleteLines,
        ]
    )
}

proptest!{
    #[test]
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers(
        state in arb::state(),
        inputs in proptest::collection::vec(arb::input(), 0..=16),
    ) {
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
            state,
            inputs
        )
    }
}

proptest!{
    #[test]
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_with_heavy_saving(
        state in arb::state(),
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
            state,
            inputs
        )
    }
}

proptest!{
    #[test]
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_with_heavy_saving_from_editor_buffers(
        buffers in arb::editor_buffers(),
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
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
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_with_heavy_saving_when_there_is_one_path_buffer_and_one_scratch_buffer(
        buffers in arb::editor_buffers_with_one_path_one_scratch(),
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
            arb::state_from_editor_buffers(buffers),
            inputs
        )
    }

    #[test]
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_with_heavy_saving_when_there_is_one_default_path_buffer_and_one_scratch_buffer(
        buffers in arb::editor_buffers_with_one_default_path_one_scratch(),
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
            arb::state_from_editor_buffers(buffers),
            inputs
        )
    }

    #[test]
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_with_heavy_saving_when_the_hash_starts_blank(
        buffers in arb::editor_buffers_blank_hash(),
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
            arb::state_from_editor_buffers(buffers),
            inputs
        )
    }
}

proptest!{
    #[test]
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_with_heavy_saving_from_a_default_state(
        inputs in proptest::collection::vec(
            prop_oneof![1 => arb::input(), 3 => arb::saved_as()],
            0..=16
        ),
    ) {
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
            d!(),
            inputs
        )
    }
}

proptest!{
    #[test]
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_with_full_saving_from_a_default_state(
        inputs in proptest::collection::vec(
            arb::saved_as(),
            0..=16
        ),
    ) {
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
            d!(),
            inputs
        )
    }
}

// This test was written and subsequently edited down in order to debug a assert
// failing inside tree-sitter, which caused an abort.
proptest!{
    #[test]
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_given_this_set_of_possible_inputs(
        input1 in arb::input(),
        state in arb::g_i_state_with_default_invalidation(),
    ) {
        u!{Input}
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
            d!(),
            vec![input1, SavedAs(state.new_index(d!()), ".fakefile".into())]
        )
    }
}

// This test was also written in order to debug that assert failing.
proptest!{
    #[test]
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_given_this_smaller_set_of_possible_inputs(
        state in arb::g_i_state_with_default_invalidation(),
    ) {
        u!{Input}
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
            d!(),
            vec![NextLanguage, SavedAs(state.new_index(d!()), ".fakefile".into())]
        )
    }
}

proptest!{
    #[test]
    fn saving_with_a_default_invalidation_after_switching_the_language_does_not_panic(
        state in arb::g_i_state_with_default_invalidation(),
    ) {
        u!{Input}
        let mut s = d!();
        for input in vec![
            NextLanguage,
            SavedAs(state.new_index(d!()), ".fakefile".into())
        ] {
            update_and_render(&mut s, input);
        }
    }
}

#[test]
fn saving_with_this_index_after_switching_the_language_does_not_panic() {
    u!{Input}
    let mut s = d!();
    for input in vec![
        NextLanguage,
        SavedAs(arb::g_i_index_from_parts(2, d!()), ".fakefile".into())
    ] {
        update_and_render(&mut s, input);
    }
}

proptest!{
    #[test]
    fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_after_this_paste(
        buffers in arb::editor_buffers_with_one_path_one_scratch(),
    ) {
        tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
            arb::state_from_editor_buffers(buffers),
            vec![Input::Paste(Some("¡".to_string()))]
        )
    }
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_in_the_zero_case() {
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        d!()
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_after_inserting_the_letter_a() {
    u!{Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            Insert('a'),
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_we_select_then_tab_in() {
    u!{BufferIdKind, BufferName, Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            SelectBuffer(BufferId { kind: Find, index: d!() }),
            TabIn
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_we_save_a_new_file() {
    u!{BufferIdKind, BufferName, Input}
    let state: g_i::State = d!();
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            SavedAs(state.new_index(g_i::IndexPart::or_max(1)), ".fakefile".into()),
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_we_insert_numbers_delete_then_redo() {
    u!{Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![InsertNumbersAtCursors, Delete, Redo]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_we_open_or_select_a_buffer() {
    u!{Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![OpenOrSelectBuffer(".fakefile".into())]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_we_insert_then_report_a_file_was_saved_at_the_first_index() {
    u!{Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            InsertNumbersAtCursors,
            SavedAs(d!(), ".fakefile".into())
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_we_insert_then_report_a_file_was_saved_at_index_1() {
    u!{Input}
    let state: g_i::State = d!();
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            InsertNumbersAtCursors,
            SavedAs(state.new_index(g_i::IndexPart::or_max(1)), ".fakefile".into())
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_in_this_generated_heavy_saving_case() {
    u!{Input}
    let state: g_i::State = d!();
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            SavedAs(state.new_index(g_i::IndexPart::or_max(0)), "&¥:&q\"N\u{baccd}.fakefile".into()),
            SavedAs(state.new_index(g_i::IndexPart::or_max(7)), "\u{202e}𪫩\u{feff}Ѩ\u{4ac61}*\t3\u{e30e1}.{0.fakefile".into()),
            SavedAs(state.new_index(g_i::IndexPart::or_max(3)), "齂🕴!ヵ..fakefile".into()),
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_we_search_for_the_empty_string() {
    u!{BufferIdKind, Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            SelectBuffer(BufferId { kind: Find, index: d!() }),
            SubmitForm
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_a_file_is_added_to_a_blank_state() {
    u!{BufferName, Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![AddOrSelectBuffer(Path(".fakefile".into()), "".to_owned())]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_a_file_is_added_to_a_blank_state_then_becomes_unedited_later() {
    u!{BufferName, Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "".to_owned()),
            Insert('A'),
            Delete
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_a_scratch_file_is_added_then_a_path_file_is_added() {
    u!{BufferName, Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            NewScratchBuffer(Option::None),
            AddOrSelectBuffer(Path(".fakefile".into()), "".to_owned())
        ]
    )
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
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_a_path_file_is_added_then_the_selection_is_changed_twice() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "".to_owned()),
            AdjustBufferSelection(Move(Left)),
            AdjustBufferSelection(Move(Left)),
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_a_file_is_edited_then_saved_then_edited() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove}

    let state: g_i::State = d!();
    let path: PathBuf = ".fakefile".into();
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
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
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_a_non_blank_file_is_added_then_edited() {
    u!{BufferName, Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            AddOrSelectBuffer(Path(".fakefile".into()), "A".to_owned()),
            CloseBuffer(d!()),
            Insert('B'),
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_larger_chars_are_pasted() {
    u!{BufferName, Input}
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        arb::state_from_editor_buffers(EditorBuffers::new((d!(), "𐫀"))),
        vec![
            Paste(Some("¡".to_string())),
        ]
    )
}

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_we_do_this_cut() {
    u!{BufferName, Input}
    let mut state = arb::state_from_editor_buffers(EditorBuffers::new((d!(), "")));
    
    update_and_render(&mut state, Insert('a'));
    update_and_render(&mut state, SelectAll);

    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
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
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
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
        let index_state= buffers.index_state();
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
            /*OpenOrSelectBuffer(ref path) => {
                if !state.buffers.index_with_name(&BufferName::Path(path.clone())).is_some() {
                    expected_edited_states.insert(state.buffers.append_index(), true);
                }
            },
*/        
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

#[test]
fn inserting_after_a_re_save_marks_the_buffer_as_edited_in_this_case() {
    u!{BufferName, Input, SelectionAdjustment, SelectionMove, EditedTransition}

    let index_state: g_i::State = d!();

    let mut state = d!();
    let path: PathBuf = ".fakefile".into();
    std::dbg!();
    let _ = update_and_render(
        &mut state,
        AddOrSelectBuffer(Path(path.clone()), "".to_owned())
    );
    std::dbg!();
    let (view, _) = update_and_render(
        &mut state,
        Insert('a')
    );

    assert_eq!(Some(ToEdited), view.edited_transitions.into_iter().next().map(|(_, t)| t));
    std::dbg!();
    let (view, _) = update_and_render(
        &mut state,
        SavedAs(
            index_state.new_index(g_i::IndexPart::or_max(1)),
            path.clone()
        )
    );

    assert_eq!(Some(ToUnedited), view.edited_transitions.into_iter().next().map(|(_, t)| t), "precondition failure");
    
    let (view, _) = update_and_render(
        &mut state,
        Insert('b')
    );

    assert_eq!(Some(ToEdited), view.edited_transitions.into_iter().next().map(|(_, t)| t));
}

fn sets_the_views_buffer_selected_index_correctly_after_moving_selection_right_on(
    state: &mut State,
) {
    u!{Input};

    // Arrange
    let buffers = dbg!(&state.view.buffers);
    let previous_generation_expected_index = buffers.current_index();

    // Act
    let (_, _) = update_and_render(
        state, 
        AdjustBufferSelection(
            SelectionAdjustment::Move(SelectionMove::Right)
        )
    );

    let expected_index = state.buffers
        .buffers()
        .index_state()
        .migrate(previous_generation_expected_index)
        .expect("previous_generation_expected_index was un-migratable");

    // Assert
    assert_eq!(
        state.view.buffers.current_index(), 
        expected_index
    );
}

proptest!{
    #[test]
    fn sets_the_views_buffer_selected_index_correctly_after_moving_selection_right(
        mut state in arb::state(),
    ) {
        sets_the_views_buffer_selected_index_correctly_after_moving_selection_right_on(
            &mut state
        )
    }
}

#[test]
fn sets_the_views_buffer_selected_index_correctly_after_moving_selection_right_on_two_scratch_buffers() {
    u!{Input}
    let mut state = d!();
    
    update_and_render(&mut state, NewScratchBuffer(Option::None));

    // Do this twice to get both going from 1 to 0 and 0 to 1.
    sets_the_views_buffer_selected_index_correctly_after_moving_selection_right_on(
        &mut state
    );

    sets_the_views_buffer_selected_index_correctly_after_moving_selection_right_on(
        &mut state
    );
}

#[test]
fn sets_the_views_buffer_selected_index_correctly_after_moving_selection_right_on_two_scratch_buffers_reduction() {
    u!{Input}
    let mut state = d!();
    
    update_and_render(&mut state, NewScratchBuffer(Option::None));


    {
    let buffers = dbg!(&state.view.buffers);
    let previous_generation_expected_index = buffers.current_index();

    state.buffers.adjust_selection(SelectionAdjustment::Move(SelectionMove::Right));

    let &mut State {
        ref mut buffers,
        ref mut view,
        ..
    } = &mut state;

    if dbg!(buffers.should_render_buffer_views())
    {
        let bufs = buffers.buffers();
        dbg!(&bufs);

        view.buffers.replace_with_mapped(
            bufs,
            |editor_buffer| {
                perf_viz::record_guard!("render BufferView");
                let name = &editor_buffer.name;
                BufferView {
                    name: name.clone(),
                    name_string: name.to_string(),
                    ..d!()
                }
            }
        );

        dbg!(&view.buffers);
    }
    //editor_view::render(&mut state);

    let expected_index = state.buffers
        .buffers()
        .index_state()
        .migrate(previous_generation_expected_index)
        .expect("previous_generation_expected_index was un-migratable");

    assert_eq!(
        state.view.buffers.current_index(), 
        expected_index
    );
    }

    {
    let buffers = dbg!(&state.view.buffers);
    let previous_generation_expected_index = buffers.current_index();

    state.buffers.adjust_selection(SelectionAdjustment::Move(SelectionMove::Right));

    let &mut State {
        ref mut buffers,
        ref mut view,
        ..
    } = &mut state;

    if dbg!(buffers.should_render_buffer_views())
    {
        let bufs = buffers.buffers();
        dbg!(&bufs);

        view.buffers.replace_with_mapped(
            bufs,
            |editor_buffer| {
                perf_viz::record_guard!("render BufferView");
                let name = &editor_buffer.name;
                BufferView {
                    name: name.clone(),
                    name_string: name.to_string(),
                    ..d!()
                }
            }
        );

        dbg!(&view.buffers);
    }
    //editor_view::render(&mut state);

    let expected_index = state.buffers
        .buffers()
        .index_state()
        .migrate(previous_generation_expected_index)
        .expect("previous_generation_expected_index was un-migratable");

    assert_eq!(
        state.view.buffers.current_index(), 
        expected_index
    );
    }
}

#[test]
fn the_view_contains_the_right_spans_after_typing_fn_below_this_fn_def() {
    u!{BufferName, Input}
    let mut state = arb::state_from_editor_buffers(
        EditorBuffers::new((Path("fakefile.rs".into()), "fn foo() {}\n"))
    );

    update_and_render(&mut state, MoveAllCursors(Move::ToBufferEnd));
    
    update_and_render(&mut state, Insert('\n'));

    assert_eq!(
        state.view.buffers.get_current_element().data.chars,
        "fn foo() {}\n\n",
        "precondition failure"
    );

    assert_eq!(
        state.view.buffers.get_current_element().data.spans,
        vec![
            SpanView { one_past_end_byte_index: 2, kind: sk!(PLAIN) },
            SpanView { one_past_end_byte_index: 6, kind: sk!(3) },
            SpanView { one_past_end_byte_index: 13, kind: sk!(PLAIN) },
        ],
        "added \\n"
    );

    update_and_render(&mut state, Insert('f'));

    assert_eq!(
        state.view.buffers.get_current_element().data.chars,
        "fn foo() {}\n\nf",
        "precondition failure"
    );

    // We really only care that the spans show all the characters, and that the
    // first line has the same spans the whole way through. This is just the 
    // simplest way to check both of those properties, but it does slightly 
    // over-assert.
    assert_eq!(
        state.view.buffers.get_current_element().data.spans,
        vec![
            SpanView { one_past_end_byte_index: 2, kind: sk!(PLAIN) },
            SpanView { one_past_end_byte_index: 6, kind: sk!(3) },
            SpanView { one_past_end_byte_index: 14, kind: sk!(PLAIN) },
        ],
        "added f"
    );

    update_and_render(&mut state, Insert('n'));

    assert_eq!(
        state.view.buffers.get_current_element().data.chars,
        "fn foo() {}\n\nfn",
        "precondition failure"
    );

    assert_eq!(
        state.view.buffers.get_current_element().data.spans,
        vec![
            SpanView { one_past_end_byte_index: 2, kind: sk!(PLAIN) },
            SpanView { one_past_end_byte_index: 6, kind: sk!(3) },
            SpanView { one_past_end_byte_index: 15, kind: sk!(PLAIN) }
        ],
        "added n"
    );
}
