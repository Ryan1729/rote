use super::*;
use wimp_types::{ui_id, ui::{do_button_logic, InputType, PhysicalButtonState, Navigation}};
use platform_types::{bvd};

use macros::{dbg, d};

use proptest::{
    prelude::{Just, Strategy},
    prop_compose, proptest,
};

mod arb {
    pub use pub_arb_abs::{abs_pos, abs_length};
}

#[test]
fn do_button_logic_does_not_flash_like_it_used_to() {
    use ButtonState::*;
    let mut ui: ui::State = d!();
    let id = ui_id!(&0);
    let rect = ssr!(
        abs::Pos::MIN,
        abs::Pos::MIN,
        abs::Pos::MAX,
        abs::Pos::MAX,
    );
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

    let x_margin = abs::Length::FIVE;
    let y_margin = abs::Length::SEVEN;

    let rect = ssr!{
        8.0,
        3.0,
        128.0,
        17.0,
    };

    let char_dim = char_dim!(4.0 8.0);

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
    let b_middle_x = (b_rect.min.x + b_rect.max.x).halve();
    let b_middle_y = (b_rect.min.y + b_rect.max.y).halve();

    let t_rect = text_spec.spec.rect;
    let t_middle_x = (t_rect.min.x + t_rect.max.x).halve();
    let t_middle_y = (t_rect.min.y + t_rect.max.y).halve();

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

    let text_w = char_dim.w * abs::Ratio::from(text_length);
    assert_eq!(t_rect.width(), text_w);
}

#[test]
fn center_within_centers_this_no_edge_cases_example_properly() {
    let rect = center_within(
        ((13.0).into(), (17.0).into()),
        ssr! {
            10.0,
            20.0,
            25.0,
            40.0,
        },
    );

    assert_eq!(
        rect,
        ssr!{
            11.0,
            21.5,
            11.0 + 13.0,
            21.5 + 17.0,
        }
    );
}

fn left_edge(tab_scroll: abs::Pos, target_index: usize, tab_width: abs::Length) -> abs::Pos {
    unscrolled_tab_left_edge(target_index, tab_width) - tab_scroll
}

fn right_edge(tab_scroll: abs::Pos, target_index: usize, tab_width: abs::Length) -> abs::Pos {
    unscrolled_tab_right_edge(target_index, tab_width) - tab_scroll
}

fn is_tab_left_edge_visible(tab_scroll: abs::Pos, target_index: usize, tab_width: abs::Length) -> bool {
    left_edge(tab_scroll, target_index, tab_width) >= 0.0
}

fn is_tab_right_edge_visible(tab_scroll: abs::Pos, target_index: usize, tab_width: abs::Length, screen_width: abs::Length) -> bool {
    right_edge(tab_scroll, target_index, tab_width) <= screen_width
}

macro_rules! tab_visible_assert {
    (
        $tab_scroll: expr,
        $target_index: expr,
        $tab_width: expr,
        $screen_width: expr,
    ) => {
        let tab_scroll = $tab_scroll;
        let target_index = $target_index;
        let tab_width = $tab_width;
        let screen_width = $screen_width;

        let left_visible = is_tab_left_edge_visible(tab_scroll, target_index, tab_width);
        let right_visible = is_tab_right_edge_visible(tab_scroll, target_index, tab_width, screen_width);

        assert!(
            left_visible || right_visible,
            "tab {}'s {} edge is not visible on a {} wide screen when the tabs are {} wide and the `tab_scroll` is {}",
            target_index,
            if left_visible { "left" } else { "right" },
            screen_width,
            tab_width,
            tab_scroll,
        );
    };
}

macro_rules! tab_as_visible_as_possible_assert {
    (
        $tab_scroll: expr,
        $target_index: expr,
        $tab_width: expr,
        $screen_width: expr,
    ) => {
        use std::cmp::{min, max};
        let tab_scroll = $tab_scroll;
        let target_index = $target_index;
        let tab_width = $tab_width;
        let screen_width = $screen_width;

        // measure how much of the tab width is visible and assert that the value is
        // at least min(tab_width, screen_width)

        let left_e = left_edge(tab_scroll, target_index, tab_width);
        let right_e = right_edge(tab_scroll, target_index, tab_width);

        let screen_left_edge = abs::Pos::ZERO;
        let screen_right_edge = abs::Pos::from(screen_width);

        let clipped_left_e = max(screen_left_edge, min(left_e, screen_right_edge));
        let clipped_right_e = max(screen_left_edge, min(right_e, screen_right_edge));

        let covers = clipped_right_e - clipped_left_e;
        let needs_to_cover = min(tab_width, screen_width);

        assert!(
            covers >= needs_to_cover,
            "tab {} covers only {} pixels where it should cover at least {} on a {} wide screen when the tabs are {} wide and the `tab_scroll` is {}
            Begins at {} (clipped from {}) and ends at {} (clipped from {}).",
            target_index,
            covers,
            needs_to_cover,
            screen_width,
            tab_width,
            tab_scroll,
            clipped_left_e,
            left_e,
            right_e,
            clipped_right_e,
        );
    };
}

// We just need some > 0 width in multiple places, and it's more convenient if we use the same one.
const SOME_SCREEN_WIDTH: abs::Length = abs::Length::TWO_FIFTY_SIX;

#[derive(Debug, Default)]
struct MakeNthTabVisibleSpec {
    tab_scroll: abs::Pos,
    target_index: usize,
    tab_width: abs::Length,
    screen_width: abs::Length,
}

prop_compose! {
    fn arb_make_nth_tab_visible_spec()
        (
            tab_count in 1..64usize,
            tab_scroll in arb::abs_pos(),
            tab_width in arb::abs_length(),
        )
        (
            target_index in 0..tab_count,
            tab_scroll in Just(tab_scroll),
            screen_width in arb::abs_length().prop_map(move |l| {if l < tab_width {
                tab_width
            } else {
                l
            }}),
            tab_width in Just(tab_width),
        ) -> MakeNthTabVisibleSpec {
        if cfg!(feature = "over-test") {
            // don't clamp
            MakeNthTabVisibleSpec {
                tab_scroll,
                target_index,
                tab_width,
                screen_width,
            }
        } else {
            use std::cmp::min;
            let max_reasonable = abs::Pos::TWO_TO_THE_TWENTY_THREE;
            MakeNthTabVisibleSpec {
                tab_scroll: min(tab_scroll, max_reasonable),
                target_index,
                tab_width: min(tab_width, max_reasonable.into()),
                screen_width: min(screen_width, max_reasonable.into()),
            }
        }
    }
}

proptest! {
    #[test]
    fn arb_make_nth_tab_visible_spec_behaves_properly(
        spec in arb_make_nth_tab_visible_spec()
    ) {
        assert!(spec.tab_width <= spec.screen_width);
    }
}

fn make_nth_tab_visible_if_present_works_on(
    MakeNthTabVisibleSpec {
        tab_scroll,
        target_index,
        tab_width,
        screen_width,
    }: MakeNthTabVisibleSpec,
) {
    let mut ui = ui::State { tab_scroll, ..d!() };

    make_nth_tab_visible_if_present(&mut ui, target_index, tab_width, screen_width);

    println!("{} -> {}", tab_scroll, ui.tab_scroll);

    tab_visible_assert!(
        ui.tab_scroll,
        target_index,
        tab_width,
        screen_width,
    );

    tab_as_visible_as_possible_assert!(
        ui.tab_scroll,
        target_index,
        tab_width,
        screen_width,
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
        tab_width: SOME_SCREEN_WIDTH,
        screen_width: SOME_SCREEN_WIDTH,
        ..d!()
    });
}

#[test]
fn make_nth_tab_visible_if_present_works_on_this_generated_two_tab_example() {
    make_nth_tab_visible_if_present_works_on(MakeNthTabVisibleSpec {
        target_index: 1,
        tab_width: SOME_SCREEN_WIDTH,
        screen_width: SOME_SCREEN_WIDTH,
        ..d!()
    });
}

#[test]
fn make_nth_tab_visible_if_present_works_on_this_generated_two_tab_example_reduction() {
    let mut ui = d!();

    let target_index = 1;

    make_nth_tab_visible_if_present(
        &mut ui,
        target_index,
        SOME_SCREEN_WIDTH,
        SOME_SCREEN_WIDTH
    );

    println!("0 -> {}", ui.tab_scroll);

    tab_visible_assert!(
        ui.tab_scroll,
        target_index,
        SOME_SCREEN_WIDTH,
        SOME_SCREEN_WIDTH,
    );
}

#[test]
fn make_nth_tab_visible_if_present_works_on_this_generated_three_tab_example() {
    make_nth_tab_visible_if_present_works_on(MakeNthTabVisibleSpec {
        tab_scroll: 4900309700000000.0.into(),
        target_index: 2,
        tab_width: SOME_SCREEN_WIDTH,
        screen_width: SOME_SCREEN_WIDTH,
    });
}

#[test]
fn make_nth_tab_visible_if_present_terminates_in_reasonable_time_on_this_example_with_a_large_index(
) {
    let mut ui = d!();
    make_nth_tab_visible_if_present(
        &mut ui,
        usize::max_value(),
        abs::Length::ONE,
        SOME_SCREEN_WIDTH
    );
}

fn make_nth_tab_visible_if_present_is_idempotent_on(
    MakeNthTabVisibleSpec {
        tab_scroll,
        target_index,
        tab_width,
        screen_width,
    }: MakeNthTabVisibleSpec,
) {
    let mut ui = ui::State { tab_scroll, ..d!() };

    make_nth_tab_visible_if_present(&mut ui, target_index, tab_width, screen_width);

    //precondition
    tab_visible_assert!(
        ui.tab_scroll,
        target_index,
        tab_width,
        SOME_SCREEN_WIDTH,
    );
    dbg!("precondition met");

    let old_tab_scroll = ui.tab_scroll;

    make_nth_tab_visible_if_present(&mut ui, target_index, tab_width, screen_width);

    tab_visible_assert!(
        ui.tab_scroll,
        target_index,
        tab_width,
        SOME_SCREEN_WIDTH,
    );
    dbg!(
        ui.tab_scroll,
        target_index,
        tab_width,
        SOME_SCREEN_WIDTH,
        "visible"
    );
    assert_eq!(ui.tab_scroll, old_tab_scroll);
}

proptest! {
    #[test]
    fn make_nth_tab_visible_if_present_is_idempotent(
        spec in arb_make_nth_tab_visible_spec()
    ) {
        make_nth_tab_visible_if_present_is_idempotent_on(spec);
    }
}

#[test]
fn make_nth_tab_visible_if_present_is_idempotent_on_this_generated_example() {
    make_nth_tab_visible_if_present_is_idempotent_on(MakeNthTabVisibleSpec {
        tab_scroll: d!(),
        target_index: 1,
        tab_width: SOME_SCREEN_WIDTH,
        screen_width: SOME_SCREEN_WIDTH,
    });
}

#[test]
/// The idea here is to make sure that going off the top of the list leads to the input box being selected.
fn render_file_switcher_menu_selects_the_fileswitcher_buffer_when_the_navigation_is_up_from_index_0() {
    let index = d!();

    let fs_view = FileSwitcherView {
        search: bvd!("a"),
        results: vec!["a".into(), "ab".into(), "abc".into()],
    };

    let mut ui: ui::State = d!();
    ui.navigation = Navigation::Up;
    ui.keyboard.hot = ui::Id::TaggedListSelection(
        ui::Tag::FileSwitcherResults,
        d!(),
    );

    let mut view_output: ViewOutput = d!();

    render_file_switcher_menu(
        index,
        &fs_view,
        &mut ui,
        b_id!(BufferIdKind::FileSwitcher, index),
        d!(),
        &mut view_output.text_or_rects,
        &mut view_output.action,
    );

    assert_eq!(view_output.action, ViewAction::Input(Input::SelectBuffer(b_id!(BufferIdKind::FileSwitcher, index))));
}

#[test]
fn view_does_not_panic_with_empty_input() {
    view(&mut d!(), &d!(), d!());

    // If we got here, the test passes.
    assert!(true);
}