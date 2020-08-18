use super::*;

use platform_types::pos;

use editor_types::{cur, Cursor};
use macros::{u};
use proptest::prelude::{proptest, prop_oneof, ProptestConfig};

use pub_arb_std::non_line_break_char;

mod arb {
    use super::*;
    use proptest::prelude::{prop_compose};

    prop_compose!{
        pub fn state()(
            buffers in editor_buffers(),
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
            let mut state = State {
                buffers,
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
            };

            // We do this so the view is always in sync.
            editor_view::render(&mut state);

            state
        }
    }

    pub fn state_from_editor_buffers(buffers: EditorBuffers) -> State {
        let mut state = State {
            buffers,
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
        };

        // We do this so the view is always in sync.
        editor_view::render(&mut state);

        state
    }

    pub use editor_buffers::tests::arb::{
        editor_buffers,
        editor_buffers_with_one_path_one_scratch,
        editor_buffers_with_one_default_path_one_scratch,
        editor_buffers_blank_hash,
    };
    pub use pub_arb_abs::{abs_pos, abs_length};
    pub use pub_arb_g_i::{
        index as g_i_index,
        state_with_default_invalidation as g_i_state_with_default_invalidation,
        index_from_parts as g_i_index_from_parts
    };
    pub use pub_arb_pos_f32::{pos_f32};
    pub use pub_arb_pos_f32_trunc::{pos_f32_trunc};
    pub use pub_arb_non_neg_f32::{non_neg_f32};
    pub use pub_arb_platform_types::{
        menu_mode,
        view,
        close_buffer,
        insert,
        input,
        saved_as
    };
    pub use pub_arb_std::{path_buf};
}

const CURSOR_SHOW_TEXT: &'static str = "            abcdefghijklmnopqrstuvwxyz::abcdefghijk::abcdefghijklmnopqrstuvwxyz";

fn example_tbxywh() -> TextBoxXYWH { tbxywh!(0.0, 0.0, 256.0, 192.0) }

fn example_char_dim() -> CharDim { char_dim!(4.0 8.0) }

fn example_font_info() -> FontInfo {
    let example_char_dim = example_char_dim();

    FontInfo {
        text_char_dim: example_char_dim,
        status_char_dim: example_char_dim,
        tab_char_dim: example_char_dim,
        find_replace_char_dim: example_char_dim,
    }
}

#[allow(dead_code)]
fn single_cursor(buffer: &TextBuffer) -> Cursor {
    let cursors = buffer.borrow_cursors();
    assert_eq!(cursors.len(), 1);

    cursors.first().clone()
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

mod update_and_render;

#[test]
fn attempt_to_make_sure_at_least_one_cursor_is_visible_reports_correctly_in_this_case() {
    let mut scroll = slxy!{
        320.0,
        0.0,
    };

    let xywh = tbxywh!(480.0, 270.0, 960.0, 540.0);

    let text_char_dim = char_dim!(16.0 32.0);

    let apron = apron!(
        (text_char_dim.w.get() / xywh.wh.w.get()),
        (text_char_dim.w.get() / xywh.wh.w.get()),
        (text_char_dim.h.get() / xywh.wh.h.get()),
        (text_char_dim.h.get() / xywh.wh.h.get()),
    );

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
    let mut scroll = slxy! {
        320.0,
        0.0,
    };

    let xywh = tbxywh!(480.0, 270.0, 960.0, 540.0);

    let text_char_dim = char_dim!(16.0 32.0);

    let apron = apron!(
        (text_char_dim.w.get() / xywh.wh.w.get()),
        (text_char_dim.w.get() / xywh.wh.w.get()),
        (text_char_dim.h.get() / xywh.wh.h.get()),
        (text_char_dim.h.get() / xywh.wh.h.get()),
    );

    let attempt_result = attempt_to_make_xy_visible(
        &mut scroll,
        xywh,
        apron,
        TextSpaceXY::default(),
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

fn first_char(buffer: &TextBuffer) -> Option<char> {
    buffer.borrow_rope().chars().next()
}

fn first_editor_buffer(state: &State) -> &EditorBuffer {
    state.buffers.buffers().iter().next().unwrap()
}

fn first_editor_buffer_char(state: &State) -> Option<char> {
    first_char(&first_editor_buffer(&state).text_buffer)
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

