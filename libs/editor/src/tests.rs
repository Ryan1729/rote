use super::*;

use platform_types::pos;

use editor_types::{cur};
use arb_macros::{arb_enum};
use macros::{u};
use proptest::prelude::{proptest};

mod arb {
    use super::*;
    use proptest::collection::vec;
    use proptest::prelude::{prop_compose, Strategy, Just};

    prop_compose!{
        pub fn state()(
            buffers in editor_buffers::tests::arb::editor_buffers(),
            /* TODO since we don't need the rest for the current test
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

    pub use pub_arb_platform_types::{menu_mode, view};
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
        let buffer = get_scrollable_buffer_mut!(state, BufferIdKind::Text).unwrap();
        assert_eq!(
            buffer.text_buffer.borrow_cursors_vec()[0], cur!{pos!{l 0, o text.len()}},
            "*** Cursor Precondition failure! ***"
        );
        assert_ne!(buffer.scroll.x, 0.0, "*** Scroll X Precondition failure! ***");
    }
    

    update_and_render(&mut state, Input::MoveAllCursors(Move::ToBufferStart));

    let buffer = get_scrollable_buffer_mut!(state, BufferIdKind::Text).unwrap();
    assert_eq!(buffer.scroll.x, 0.0);
    assert_eq!(buffer.scroll.y, 0.0);
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
        let buffer = get_scrollable_buffer_mut!(state, BufferIdKind::Text).unwrap();
        assert_eq!(
            buffer.text_buffer.borrow_cursors_vec()[0], cur!{pos!{l 0 o 0}},
            "*** Cursor Precondition failure! ***"
        );
        assert_eq!(buffer.scroll.y, 0.0, "*** Scroll Y Precondition failure! ***");
    }
    

    update_and_render(&mut state, Input::SetMenuMode(MenuMode::FindReplace(d!())));

    // Act
    update_and_render(&mut state, Input::Insert(NEEDLE.chars().next().unwrap()));
    update_and_render(&mut state, Input::SubmitForm);

    // Assert
    let buffer = get_scrollable_buffer_mut!(state, BufferIdKind::Text).unwrap();
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
    

    dbg!(get_scrollable_buffer_mut!(state));

    update_and_render(&mut state, Input::MoveAllCursors(Move::ToBufferEnd));

    dbg!(get_scrollable_buffer_mut!(state));

    let buffer = get_scrollable_buffer_mut!(state).unwrap();

    buffer.text_buffer.borrow_cursors_vec()[0] == cur!{pos!{l 0, o text.len()}}
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
    fn get_scrollable_buffer_mut_selects_text_buffer_when_asked_no_matter_what_mode_it_is_in(
        mode in arb::menu_mode()
    ) {
        let some_text = "get_scrollable_buffer_mut_selects_text_buffer_when_asked";
        let mut state: State = some_text.into();
    
        update_and_render(&mut state, Input::SetMenuMode(mode));
    
        // precondition
        assert_eq!(state.menu_mode, mode);
    
        let buffer = get_scrollable_buffer_mut!(state, BufferIdKind::Text)
            .expect("get_scrollable_buffer_mut returned None");
    
        let state_str: String = (&buffer.text_buffer).into();

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
        let buffer = get_scrollable_buffer_mut!(state, BufferIdKind::FileSwitcher).unwrap();
        for c in buffer.text_buffer.borrow_cursors_vec() {
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
    let buffer = get_scrollable_buffer_mut!(state, BufferIdKind::FileSwitcher).unwrap();
    for c in buffer.text_buffer.borrow_cursors_vec() {
        assert_eq!(
            c.state,
            d!(),
        );
    }
}

proptest!{
    #[test]
    fn render_updates_the_amount_of_buffers(
        mut state in arb::state(),
        mut view in arb::view(),
    ) {
        // they can be different or the same here
        editor_view::render(&mut state, &mut view);

        // but the must be the same here
        assert_eq!(
            state.buffers.len(),
            view.buffers.len(),
        )
    }
}