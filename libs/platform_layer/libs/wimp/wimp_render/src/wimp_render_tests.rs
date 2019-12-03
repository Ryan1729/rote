use super::*;
use proptest::{
    num::f32,
    prelude::{Just, Strategy},
    prop_compose, proptest,
};
pub fn usual() -> f32::Any {
    f32::POSITIVE | f32::NEGATIVE | f32::NORMAL | f32::ZERO
}

#[test]
fn do_button_logic_does_not_flash_like_it_used_to() {
    use std::f32::INFINITY;
    use ButtonState::*;
    let mut ui: UIState = d!();
    let id = id!(&0);
    let rect = ScreenSpaceRect {
        min: (-INFINITY, -INFINITY),
        max: (INFINITY, INFINITY),
    };
    ui.frame_init();
    assert_eq!(do_button_logic(&mut ui, id, rect), (false, Usual));
    ui.frame_end();
    ui.frame_init();
    assert_eq!(
        do_button_logic(&mut ui, id, rect),
        (false, Hover(InputType::Mouse))
    );
    ui.frame_end();
    ui.left_mouse_state = PhysicalButtonState::PressedThisFrame;
    for i in 0..16 {
        ui.frame_init();
        assert_eq!(
            do_button_logic(&mut ui, id, rect),
            (false, Pressed(InputType::Mouse)),
            "iteration {}",
            i
        );
        ui.frame_end();
    }
    ui.left_mouse_state = PhysicalButtonState::ReleasedThisFrame;
    ui.frame_init();
    assert_eq!(
        do_button_logic(&mut ui, id, rect),
        (true, Hover(InputType::Mouse))
    );
    ui.frame_end();
    for i in 0..16 {
        ui.frame_init();
        assert_eq!(
            do_button_logic(&mut ui, id, rect),
            (false, Hover(InputType::Mouse)),
            "iteration {}",
            i
        );
        ui.frame_end();
    }
}

#[test]
fn render_outline_button_centers_this_example_properly() {
    let mut text_or_rects = Vec::new();

    let x_margin = 5.0;
    let y_margin = 7.0;

    let rect = ScreenSpaceRect {
        min: (8.0, 3.0),
        max: (128.0, 17.0),
    };

    let char_dim = CharDim { w: 4.0, h: 8.0 };

    let text = "test";

    let text_length = text.chars().count();

    let spec = OutlineButtonSpec {
        text: "test",
        size: 8.0,
        char_dim,
        margin: Spacing::Axis(x_margin, y_margin),
        rect,
        z: TAB_Z,
        ..d!()
    };
    render_outline_button(&mut text_or_rects, spec, ButtonState::Usual, 1.0);

    let background_rect = text_or_rects
        .iter()
        .find_map(|e| match e {
            TextOrRect::Rect(r) => Some(r),
            _ => None,
        })
        .unwrap();

    let text_spec = text_or_rects
        .iter()
        .find_map(|e| match e {
            TextOrRect::Text(t) => Some(t),
            _ => None,
        })
        .unwrap();

    let b_rect = background_rect.rect;
    let b_middle_x = (b_rect.min.0 + b_rect.max.0) / 2.0;
    let b_middle_y = (b_rect.min.1 + b_rect.max.1) / 2.0;

    let t_rect = text_spec.spec.rect;
    let t_middle_x = (t_rect.min.0 + t_rect.max.0) / 2.0;
    let t_middle_y = (t_rect.min.1 + t_rect.max.1) / 2.0;

    assert_eq!(
        t_middle_x, b_middle_x,
        "t_rect: {:?} b_rect: {:?}",
        t_rect, b_rect
    );
    assert_eq!(
        t_middle_y, b_middle_y,
        "t_rect: {:?} b_rect: {:?}",
        t_rect, b_rect
    );

    let text_w = char_dim.w * text_length as f32;
    assert_eq!(t_rect.width(), text_w);
}

#[test]
fn center_within_centers_this_no_edge_cases_example_properly() {
    let rect = center_within(
        (13.0, 17.0),
        ScreenSpaceRect {
            min: (10.0, 20.0),
            max: (25.0, 40.0),
        },
    );

    assert_eq!(
        rect,
        ScreenSpaceRect {
            min: (11.0, 21.5),
            max: (11.0 + 13.0, 21.5 + 17.0),
        }
    );
}

fn is_tab_visible(tab_scroll: f32, target_index: usize, tab_width: f32, screen_width: f32) -> bool {
    (target_index + 1) as f32 * tab_width + tab_scroll <= screen_width
        && target_index as f32 * tab_width + tab_scroll >= 0.0
}

macro_rules! tab_visible_assert {
    (
        $tab_scroll: expr,
        $target_index: expr,
        $tab_width: expr,
        $screen_width: expr,
        $tab_count: expr
    ) => {
        let tab_scroll = $tab_scroll;
        let target_index = $target_index;
        let tab_width = $tab_width;
        let screen_width = $screen_width;

        assert!(
            is_tab_visible(tab_scroll, target_index, tab_width, screen_width),
            "tab {}/{} is not visible on a {} wide screen when the tabs are {} wide and the `tab_scroll` is {}",
            target_index, $tab_count, screen_width, tab_width, tab_scroll
        );
    };
}

// We just need some > 0 width in multiple places, and it's more convenient if we use the same one.
const SOME_SCREEN_WIDTH: f32 = 1024.0;

#[derive(Debug)]
struct MakeNthTabVisibleSpec {
    tab_scroll: f32,
    target_index: usize,
    tab_count: usize,
    tab_width: f32,
}

prop_compose! {
    fn arb_make_nth_tab_visible_spec()
        (
            tab_scroll in usual(),
            tab_count in 1..64usize,
            tab_width in usual().prop_map(|f| if f > SOME_SCREEN_WIDTH {
                SOME_SCREEN_WIDTH
             } else if f < 0.0 {
                 0.0
              } else {f}
          ),
        )
        (
            target_index in 0..tab_count,
            tab_scroll in Just(tab_scroll),
            tab_count in Just(tab_count),
            tab_width in Just(tab_width),
        ) -> MakeNthTabVisibleSpec {
         MakeNthTabVisibleSpec {
             tab_scroll,
             target_index,
             tab_count,
             tab_width,
         }
    }
}

fn make_nth_tab_visible_if_present_works_on(
    MakeNthTabVisibleSpec {
        tab_scroll,
        target_index,
        tab_count,
        tab_width,
    }: MakeNthTabVisibleSpec,
) {
    let mut ui = UIState { tab_scroll, ..d!() };

    make_nth_tab_visible_if_present(&mut ui, target_index, tab_count, tab_width);

    println!("{} -> {}", tab_scroll, ui.tab_scroll);

    tab_visible_assert!(
        ui.tab_scroll,
        target_index,
        tab_width,
        SOME_SCREEN_WIDTH,
        tab_count
    );
}

proptest! {
    #[test]
    fn make_nth_tab_visible_if_present_works(
        spec in arb_make_nth_tab_visible_spec()
    ) {
        make_nth_tab_visible_if_present_works_on(spec);
    }
}

#[test]
fn make_nth_tab_visible_if_present_works_on_this_generated_example() {
    make_nth_tab_visible_if_present_works_on(MakeNthTabVisibleSpec {
        tab_scroll: 0.0,
        target_index: 0,
        tab_count: 1,
        tab_width: SOME_SCREEN_WIDTH,
    });
}

#[test]
fn make_nth_tab_visible_if_present_works_on_this_generated_two_tab_example() {
    make_nth_tab_visible_if_present_works_on(MakeNthTabVisibleSpec {
        tab_scroll: 0.0,
        target_index: 1,
        tab_count: 2,
        tab_width: SOME_SCREEN_WIDTH,
    });
}

#[test]
fn make_nth_tab_visible_if_present_works_on_this_generated_three_tab_example() {
    make_nth_tab_visible_if_present_works_on(MakeNthTabVisibleSpec {
        tab_scroll: 4900309700000000.0,
        target_index: 2,
        tab_count: 3,
        tab_width: SOME_SCREEN_WIDTH,
    });
}

#[test]
fn make_nth_tab_visible_if_present_terminates_in_reasonable_time_on_this_example_with_a_large_index(
) {
    let mut ui = d!();
    make_nth_tab_visible_if_present(&mut ui, usize::max_value(), 1, SOME_SCREEN_WIDTH);
}

fn make_nth_tab_visible_if_present_is_idemponent_on(
    MakeNthTabVisibleSpec {
        tab_scroll,
        target_index,
        tab_count,
        tab_width,
    }: MakeNthTabVisibleSpec,
) {
    let mut ui = UIState { tab_scroll, ..d!() };

    make_nth_tab_visible_if_present(&mut ui, target_index, tab_count, tab_width);

    //precondition
    tab_visible_assert!(
        ui.tab_scroll,
        target_index,
        tab_width,
        SOME_SCREEN_WIDTH,
        tab_count
    );
    dbg!("precondition met");

    let old_tab_scroll = ui.tab_scroll;

    make_nth_tab_visible_if_present(&mut ui, target_index, tab_count, tab_width);

    tab_visible_assert!(
        ui.tab_scroll,
        target_index,
        tab_width,
        SOME_SCREEN_WIDTH,
        tab_count
    );
    dbg!(
        ui.tab_scroll,
        target_index,
        tab_width,
        SOME_SCREEN_WIDTH,
        tab_count,
        "visible"
    );
    assert_eq!(ui.tab_scroll, old_tab_scroll);
}

proptest! {
    #[test]
    fn make_nth_tab_visible_if_present_is_idemponent(
        spec in arb_make_nth_tab_visible_spec()
    ) {
        make_nth_tab_visible_if_present_is_idemponent_on(spec);
    }
}

#[test]
fn make_nth_tab_visible_if_present_is_idemponent_on_this_generated_example() {
    make_nth_tab_visible_if_present_is_idemponent_on(MakeNthTabVisibleSpec {
        tab_scroll: 0.0,
        target_index: 1,
        tab_count: 2,
        tab_width: SOME_SCREEN_WIDTH,
    });
}
