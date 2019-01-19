use super::*;
use platform_types::pos;
use editor_types::cur;

use proptest::prelude::{proptest, Strategy};

fn update_and_render_shows_the_cursor_when_pressing_home_on(text: &str, buffer_xywh: TextBoxXYWH, char_dim: CharDim) {
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

    {
        let buffer = get_scrollable_buffer_mut!(state).unwrap();
        assert_eq!(
            buffer.text_buffer.borrow_cursors_vec()[0], cur!{pos!{l 0, o text.len()}},
            "*** Cursor Precondition failure! ***"
        );
        assert_ne!(buffer.scroll.x, 0.0, "*** Scroll Precondition failure! ***");
    }
    

    update_and_render(&mut state, Input::MoveAllCursors(Move::ToBufferStart));

    let buffer = get_scrollable_buffer_mut!(state).unwrap();
    assert_eq!(buffer.scroll.x, 0.0);
}

const CURSOR_SHOW_TEXT: &'static str = "            abcdefghijklmnopqrstuvwxyz::abcdefghijk::abcdefghijklmnopqrstuvwxyz";

#[test]
fn update_and_render_shows_the_cursor_when_pressing_home_in_this_case() {
    update_and_render_shows_the_cursor_when_pressing_home_on(
        CURSOR_SHOW_TEXT,
        tbxywh!(0.0, 0.0, 256.0, 192.0),
        CharDim { w: 4.0, h: 8.0 }
    );
}

mod arb {
    use super::*;

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

proptest!{
    #[test]
    fn update_and_render_shows_the_cursor_when_pressing_home(
        text_w in arb::at_least_one(),
        text_h in arb::at_least_one(),
        w in arb::at_least_one(),
        h in arb::at_least_one(),
    ) {
        update_and_render_shows_the_cursor_when_pressing_home_on(
            CURSOR_SHOW_TEXT,
            tbxywh!(0.0, 0.0, text_w, text_h),
            CharDim { w, h }
        );
    }

    #[test]
    fn update_and_render_shows_the_cursor_when_pressing_home_with_restrictions(
        box_w in arb::at_least_one(),
        box_h in arb::at_least_one(),
        w in arb::at_least_one(),
        h in arb::at_least_one(),
    ) {
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

        update_and_render_shows_the_cursor_when_pressing_home_on(
            CURSOR_SHOW_TEXT,
            tbxywh!(0.0, 0.0, box_w, box_h),
            CharDim { w, h }
        );
    }
}

#[test]
fn update_and_render_shows_the_cursor_when_pressing_home_in_this_realistic_case() {
    update_and_render_shows_the_cursor_when_pressing_home_on(
        CURSOR_SHOW_TEXT,
        tbxywh!(0.0, 0.0, 1920.0, 1080.0),
        CharDim { w: 16.0, h: 32.0 }
    );
}
