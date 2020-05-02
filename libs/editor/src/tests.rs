use super::*;

use platform_types::pos;

use editor_types::{cur, Cursor};
use macros::{u, dbg};
use proptest::prelude::{proptest};

use std::collections::HashMap;

use pub_arb_std::non_line_break_char;

mod arb {
    use super::*;
    use proptest::prelude::{prop_compose, Strategy};

    prop_compose!{
        pub fn state()(
            buffers in editor_buffers::tests::arb::editor_buffers(),
            /* TODO since we don't need the rest for the current tests
            buffer_xywh in tbxywh(),
            current_buffer_kind in buffer_id_kind(),
            mm in menu_mode(),
            file_switcher in scrollable_buffer(),
            fsr in file_switcher_results(),
            find in scrollable_buffer(),
            find_xywh in tbxywh(),
            replace in scrollable_buffer(),
            replace_xywh in tbxywh(),
            go_to_position in scrollable_buffer(),
            go_to_position_xywh in tbxywh(),
            fi in font_info(),
            ch in clipboard_history(),
            p in parsers(),
            */
        ) -> State {
            State {
                //buffers,
                ..d!()
                /*
                buffer_xywh,
                current_buffer_kind,
                menu_mode: mm,
                file_switcher,
                file_switcher_results: fsr,
                find,
                find_xywh,
                replace,
                replace_xywh,
                go_to_position,
                go_to_position_xywh,
                font_info: fi,
                clipboard_history: ch,
                parsers: p,
                */
            }
        }
    }

    pub fn at_least_one() -> impl Strategy<Value = f32> {
        proptest::num::f32::POSITIVE.prop_map(|n| 
            if n >= 1.0 {
               n 
            } else {
                // NaN ends up here
                1.0
            }
        )
    }

    pub use pub_arb_platform_types::{menu_mode, view, input};
}

const CURSOR_SHOW_TEXT: &'static str = "            abcdefghijklmnopqrstuvwxyz::abcdefghijk::abcdefghijklmnopqrstuvwxyz";

const EXAMPLE_TBXYWH: TextBoxXYWH = tbxywh!(0.0, 0.0, 256.0, 192.0);

const EXAMPLE_CHAR_DIM: CharDim = CharDim { w: 4.0, h: 8.0 };

fn update_and_render_shows_the_cursor_when_pressing_home_on(text: &str, buffer_xywh: TextBoxXYWH, char_dim: CharDim) {
    let mut state: State = text.into();
    state.buffer_xywh = buffer_xywh;
    state.font_info = FontInfo {
        text_char_dim: char_dim,
        status_char_dim: char_dim,
        tab_char_dim: char_dim,
        find_replace_char_dim: char_dim,
    };

    update_and_render(&mut state, Input::MoveAllCursors(Move::ToBufferEnd));

    {
        let buffer = get_text_buffer_mut!(state, BufferIdKind::Text).unwrap();
        assert_eq!(
            *buffer.borrow_cursors().first(), cur!{pos!{l 0, o text.len()}},
            "*** Cursor Precondition failure! ***"
        );
        assert_ne!(buffer.scroll.x, 0.0, "*** Scroll X Precondition failure! ***");
    }
    

    update_and_render(&mut state, Input::MoveAllCursors(Move::ToBufferStart));

    let buffer = get_text_buffer_mut!(state, BufferIdKind::Text).unwrap();
    assert_eq!(buffer.scroll.x, 0.0, "buffer.scroll.x");
    assert_eq!(buffer.scroll.y, 0.0, "buffer.scroll.y");
}

#[test]
fn update_and_render_shows_the_cursor_when_pressing_home_in_this_case() {
    update_and_render_shows_the_cursor_when_pressing_home_on(
        CURSOR_SHOW_TEXT,
        EXAMPLE_TBXYWH,
        EXAMPLE_CHAR_DIM
    );
}

#[test]
fn update_and_render_shows_the_cursor_when_searching_in_this_case() {
    // Arrange
    let mut text = String::new();
    
    // The goal is enough text that it is definitely offscreen
    for i in 0..(EXAMPLE_TBXYWH.wh.h / EXAMPLE_CHAR_DIM.h) as u128 * 8 {
        for _ in 0..i {
            text.push('.');
        }

        text.push('\n');
    }

    const NEEDLE: &str = "needle";
    text.push_str(NEEDLE);

    let mut state: State = text.into();
    state.buffer_xywh = EXAMPLE_TBXYWH;
    state.font_info = FontInfo {
        text_char_dim: EXAMPLE_CHAR_DIM,
        status_char_dim: EXAMPLE_CHAR_DIM,
        tab_char_dim: EXAMPLE_CHAR_DIM,
        find_replace_char_dim: EXAMPLE_CHAR_DIM,
    };

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

macro_rules! max_one {
    ($n: expr) => {{
        let n = $n;
        if n >= 1.0 {
            n
        } else {
            // NaN ends up here
            1.0
        }
    }}
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

    *buffer.borrow_cursors().first() == cur!{pos!{l 0, o text.len()}}
    && buffer.scroll.x != 0.0
}

#[test]
fn update_and_render_shows_the_cursor_when_pressing_home() {
    use proptest::test_runner::{TestRunner, TestCaseError};
    let mut runner = TestRunner::default();

    runner.run(&(
        arb::at_least_one(),
        arb::at_least_one(),
        arb::at_least_one(),
        arb::at_least_one(),
    ), |(box_w, box_h, w, h)| {

        let w_max = max_one!(box_w / 2.0);
        let w = if w > w_max {
            w_max
        } else {
            // NaN ends up here
            w
        };

        let h_max = max_one!(box_h / 2.0);
        let h = if h > h_max {
            h_max
        } else {
            // NaN ends up here
            h
        };

        if passes_preconditions(
            CURSOR_SHOW_TEXT,
            tbxywh!(0.0, 0.0, box_w, box_h),
            CharDim { w, h }
        ) {
            update_and_render_shows_the_cursor_when_pressing_home_on(
                CURSOR_SHOW_TEXT,
                tbxywh!(0.0, 0.0, box_w, box_h),
                CharDim { w, h },
            );
            Ok(())
        } else {
            Err(TestCaseError::Reject("failed preconditions".into()))
        }
    }).unwrap();
}

#[test]
fn attempt_to_make_sure_at_least_one_cursor_is_visible_reports_correctly_in_this_case() {
    let mut scroll = ScrollXY {
            x: 320.0,
            y: 0.0,
        };

    let xywh = tbxywh!(480.0, 270.0, 960.0, 540.0);

    let text_char_dim = CharDim { w: 16.0, h: 32.0 };

    let apron: Apron = text_char_dim.into();
    
    let text_space = position_to_text_space(pos!{}, text_char_dim);

    let attempt_result = attempt_to_make_xy_visible(
        &mut scroll,
        xywh,
        apron,
        text_space,
    );

    if scroll.x != 320.0 {
        assert_eq!(attempt_result, VisibilityAttemptResult::Succeeded, "false negative");
    } else {
        assert_ne!(attempt_result, VisibilityAttemptResult::Succeeded, "false positive");
    }
}

#[test]
fn attempt_to_make_xy_visible_reports_correctly_in_this_case() {
    let mut scroll = ScrollXY {
            x: 320.0,
            y: 0.0,
        };

    let xywh = tbxywh!(480.0, 270.0, 960.0, 540.0);

    let attempt_result = attempt_to_make_xy_visible(
        &mut scroll,
        xywh,
        CharDim { w: 16.0, h: 32.0 }.into(),
        TextSpaceXY {
            x: 0.0,
            y: 0.0,
        },
    );

    if scroll.x != 320.0 {
        assert_eq!(attempt_result, VisibilityAttemptResult::Succeeded, "false negative x = {}", scroll.x);
    } else {
        assert_ne!(attempt_result, VisibilityAttemptResult::Succeeded, "false positive x = {}", scroll.x);
    }
}

// There was a bug that came down to this not working, (well, really the two parameter version not existing, but still.)
proptest!{
    #[test]
    fn get_text_buffer_mut_selects_text_buffer_when_asked_no_matter_what_mode_it_is_in(
        mode in arb::menu_mode()
    ) {
        let some_text = "get_text_buffer_mut_selects_text_buffer_when_asked";
        let mut state: State = some_text.into();
    
        update_and_render(&mut state, Input::SetMenuMode(mode));
    
        // precondition
        assert_eq!(state.menu_mode, mode);
    
        let buffer = get_text_buffer_mut!(state, BufferIdKind::Text)
            .expect("get_text_buffer_mut returned None");
    
        let state_str: String = buffer.into();

        assert_eq!(
            &state_str,
            some_text
        );
    }
}

#[test]
/// The property this test checks for makes it easier for editor clients to implement keyboard navigation by examining
/// the cursors. It also looks mildly nicer.
fn update_and_render_resets_the_cursor_states_in_this_case() {
    // Arrange
    let mut state: State = String::new().into();
    state.buffer_xywh = EXAMPLE_TBXYWH;
    state.font_info = FontInfo {
        text_char_dim: EXAMPLE_CHAR_DIM,
        status_char_dim: EXAMPLE_CHAR_DIM,
        tab_char_dim: EXAMPLE_CHAR_DIM,
        find_replace_char_dim: EXAMPLE_CHAR_DIM,
    };

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

fn single_cursor_view(view: &View) -> CursorView {
    assert_eq!(usize::from(view.buffers.len()), 1);

    let cursors = &view.buffers
        .get_current_element()
        .data
        .cursors;
    assert_eq!(usize::from(cursors.len()), 1);

    cursors.first().unwrap().clone()
}

fn update_and_render_places_the_cursor_correctly_after_inserting_after_a_find_between_two_other_chars_on(
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
    fn update_and_render_places_the_cursor_correctly_after_inserting_after_a_find_between_two_other_chars(
        ch1 in non_line_break_char(),
        ch2 in non_line_break_char(),
        ch3 in non_line_break_char(),
        ch4 in non_line_break_char(),
    ) {
        update_and_render_places_the_cursor_correctly_after_inserting_after_a_find_between_two_other_chars_on(
            ch1,
            ch2,
            ch3,
            ch4,
        )
    }
}

fn len_chars(editor_buffer: &EditorBuffer) -> usize {
    editor_buffer.text_buffer.borrow_rope().len_chars().0
}

fn first_char(editor_buffer: &EditorBuffer) -> char {
    editor_buffer.text_buffer.borrow_rope().chars().next().unwrap()
}

#[test]
fn update_and_render_retains_the_scratch_buffer_data_if_the_name_does_not_match_after_an_insert_then_a_saved_as_with_the_same_index() {
    u!{Input}
    let mut state: State = d!();
    let inputs = vec![Insert('a'), SavedAs(d!(), ".fakefile".into())];

    for input in inputs {
        let _ = update_and_render(&mut state, input);
    }

    assert_eq!(usize::from(state.buffers.len()), 1usize);

    assert_eq!(first_char(state.buffers.buffers().iter().next().unwrap()), 'a');
}

#[test]
fn update_and_render_retains_the_path_buffer_data_if_the_name_does_not_match_after_an_insert_then_a_saved_as_with_the_same_index() {
    u!{Input}
    let mut state: State = d!();
    let inputs = vec![SavedAs(d!(), "a.fakefile".into()), Insert('a'), SavedAs(d!(), ".fakefile".into())];

    for input in inputs {
        let _ = update_and_render(&mut state, input);
    }

    assert_eq!(usize::from(state.buffers.len()), 1usize);

    let buffer = state.buffers.buffers().iter().next().unwrap();
    assert_eq!(first_char(buffer), 'a');
}

#[allow(dead_code)]
fn single_cursor(buffer: &TextBuffer) -> Cursor {
    let cursors = buffer.borrow_cursors();
    assert_eq!(cursors.len(), 1);

    cursors.first().clone()
}

proptest!{
    #[test]
    fn render_updates_the_amount_of_buffers(
        mut state in arb::state(),
    ) {
        // they can be different or the same here
        editor_view::render(&mut state);

        // but they must be the same here
        assert_eq!(
            state.buffers.len(),
            state.view.buffers.len(),
        )
    }
}

/* I think tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers supercedes this
fn update_and_render_reports_a_change_at_the_correct_edited_index_on(
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
    fn update_and_render_reports_a_change_at_the_correct_edited_index(
        mut state in arb::state(),
        input in arb::input(),
    ) {
        update_and_render_reports_a_change_at_the_correct_edited_index_on(
            state,
            input
        )
    }
}

#[test]
fn update_and_render_reports_a_change_at_the_correct_edited_index() {
    update_and_render_reports_a_change_at_the_correct_edited_index_on(
        state,
        Input::Undo
    )
}
*/
proptest!{
    #[test]
    fn passing_add_or_select_buffer_to_update_and_render_updates_and_selects_the_default_buffer(
        s in  ".*",
    ) {
        let mut state: State = d!();
    
        let name = d!();

        update_and_render(&mut state, Input::AddOrSelectBuffer(name, s.clone()));
    
        assert_eq!(
            usize::from(state.buffers.len()),
            1
        );
        assert_eq!(
            String::from(state.buffers.get_current_buffer()),
            s
        );
    }
}

/// This test predicate simulates what we expected clients to do if they want to keep track 
/// of which buffers are currently different from what is on disk. This is a little 
/// complicated because the editor is the one who knows about the undo history, and the 
/// client is the one who knows about when things are saved to disk or not.
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
    mut state: State,
    inputs: Vec<Input>,
) {
    let mut unedited_buffer_states = state.buffers.buffers().clone();

    let buffer_count = usize::from(state.buffers.len());
    dbg!(&unedited_buffer_states, buffer_count);
    let mut expected_edited_states: HashMap<g_i::Index, bool> = HashMap::with_capacity(buffer_count);

    for (i, _) in state.buffers.buffers().iter_with_indexes() {
        expected_edited_states.insert(i, false);
    }

    for input in inputs {

        u!{Input}
        match input {
        /*
            AddOrSelectBuffer(ref name, _) => {
                if !state.buffers.index_with_name(name).is_some() {
                    expected_edited_states.insert(state.buffers.append_index(), true);
                }
            },
            NewScratchBuffer(_) => {
                expected_edited_states.insert(state.buffers.append_index(), true);
            },
            OpenOrSelectBuffer(ref path) => {
                if !state.buffers.index_with_name(&BufferName::Path(path.clone())).is_some() {
                    expected_edited_states.insert(state.buffers.append_index(), true);
                }
            },
*/        
            SavedAs(index, _) => {
                dbg!("SavedAs()", index, expected_edited_states.contains_key(&index));
                // We need to trust the platform layer to only call
                // this when the file is saved under the given path.
                if expected_edited_states.contains_key(&index) {
                    
                    let buffer = unedited_buffer_states
                        .get_mut(index)
                        .expect("SavedAs invalid unedited_buffer_states index");
                    *buffer = state.buffers.buffers()
                        .get(index)
                        .expect("SavedAs invalid state.buffers.buffers index")
                        .clone();

                    expected_edited_states.insert(index, false);
                }
            }

            _ => {}
        }

        let (view, _) = update_and_render(&mut state, input);
        dbg!(&view.edited_transitions);
        for (i, transition) in view.edited_transitions {
            u!{EditedTransition}
            match transition {
                ToEdited => {
                    expected_edited_states.insert(i, true);
                }
                ToUnedited => {
                    expected_edited_states.insert(i, false);
                }
            }
        }
    }

    dbg!(&state.buffers);
    assert_eq!(expected_edited_states.len(), usize::from(state.buffers.len()), "expected_edited_states len does not match state.buffers");

    for (i, is_edited) in expected_edited_states {
        let actual_data: String = state.buffers.buffers().get(i).expect("actual_data was None").into();
        let original_data: Option<String> = unedited_buffer_states.get(i).map(|s| s.into());
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
                original_data.expect("original_data was None"),
                "({:?}, {:?})",
                i,
                is_edited                
            );
        }
    }
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

#[test]
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_in_the_zero_case() {
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        d!()
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
fn tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_if_we_search_for_the_empty_string() {
    u!{BufferIdKind, BufferName, Input}
    let state: g_i::State = d!();
    tracking_what_the_view_says_gives_the_correct_idea_about_the_state_of_the_buffers_on(
        d!(),
        vec![
            SelectBuffer(BufferId { kind: Find, index: d!() }),
            SubmitForm
        ]
    )
}

