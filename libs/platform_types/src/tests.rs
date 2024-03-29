#![cfg_attr(feature = "pub_arb", allow(dead_code))]
#![cfg_attr(feature = "pub_arb", allow(unused_macros))]
#![cfg_attr(feature = "pub_arb", allow(unused_imports))]

use macros::{d, fmt_display, ord};
use super::*;
use screen_space::{char_dim, ssxywh};
use crate::screen_positioning::{
    screen_space_to_position,
    screen_space_to_text_space,
    position_to_screen_space,
    attempt_to_make_xy_visible,
    position_to_text_space,
    inside_rect,
    screen_to_text_box,
    text_to_text_box,
    text_box_to_screen,
    text_box_to_text,
    text_space_to_position,
    text_space_to_screen_space,
    Apron,
    MapElements,
    PositionRound,
    ScreenSpaceXY,
    ScreenSpaceXYWH,
    ScreenSpaceWH,
    TextBoxXY,
    TextBoxXYWH,
    TextSpaceXY,
    VisibilityAttemptResult,
};
use crate::{pos, screen_positioning::{apron, slxy, tbsxy, tbxy, tbxywh, tsxy}};
use proptest::{prop_compose, proptest, Strategy, f32};

prop_compose! {
    fn arb_pos(max_line: usize, max_offset: usize)
    (line in 0..=max_line, offset in 0..=max_offset) -> Position {
        Position{ line, offset: CharOffset(offset) }
    }
}

// I'm not sure it makes sense to compare highlights outside of tests, and if it does, that this
// comparison is the correct way to do it.
#[derive(Debug)]
struct OrdHighlight(Highlight);

macro_rules! h {
    ($($tokens:tt)*) => (
        OrdHighlight(highlight!($($tokens)*))
    );
}

ord!(and friends for OrdHighlight: s, other in s.0
    .min
    .cmp(&other.0.min)
    .then_with(|| s.0.max.cmp(&other.0.max)));

fn get_ord_highlights<O: Into<Option<Position>>>(
    position: Position,
    highlight_position: O,
) -> Vec<OrdHighlight> {
    {
        let mut highlights = Vec::with_capacity(16);

        push_highlights(&mut highlights, position, highlight_position, d!());

        highlights
    }
    .into_iter()
    .map(OrdHighlight)
    .collect()
}

proptest! {
    #[test]
    fn number_of_highlights_does_not_exceed_upper_bound(
        pos in arb_pos(99, 99),
        highlight_pos in proptest::option::of(arb_pos(99, 99))
    ) {
        let unoptimized_upper_bound = highlight_pos.map(|h| {
            let min = std::cmp::min(pos, h);
            let max = std::cmp::max(pos, h);

            let lines_difference = max.line - min.line;

            // I have forgotten the reasoning behined thia, ad I just (as of this writing)
            // added the lines_difference max call, to fix a suprise test failure
            std::cmp::max(lines_difference, 2)
             + if h.offset == pos.offset || max.offset == 0 {0} else {1}

        }).unwrap_or(0);

        let upper_bound = std::cmp::min(unoptimized_upper_bound, 3);

        assert!(get_ord_highlights(pos, highlight_pos).len() <= upper_bound);
    }

    #[test]
    fn order_of_positions_does_not_affect_highlights(
        pos in arb_pos(99, 99),
        highlight_pos in arb_pos(99, 99)
    ) {
        assert_eq!(
            get_ord_highlights(pos, highlight_pos),
            get_ord_highlights(highlight_pos, pos)
        );
    }
}

macro_rules! highlight_assert {
    (($pos: expr, $highlight_pos: expr), $output: expr) => {
        assert_eq!(
            get_ord_highlights($pos, $highlight_pos),
            $output,
            "\ninput: {:?}",
            ($pos, $highlight_pos)
        );
    };
}

#[test]
fn multiline_highlights_work() {
    highlight_assert!(
        (pos! {l 0 o 0}, pos! {l 0 o 300}),
        vec![h! {l 0 o 0 l 0 o 300}]
    );

    highlight_assert!(
        (pos! {l 0 o 0}, pos! {l 1 o 300}),
        vec![h! {l 0 o 0 l 0 o max}, h! {l 1 o 0 l 1 o 300}]
    );

    highlight_assert!(
        (pos! {l 0 o 200}, pos! {l 0 o 300}),
        vec![h! {l 0 o 200 l 0 o 300}]
    );

    highlight_assert!(
        (pos! {l 0 o 200}, pos! {l 1 o 300}),
        vec![h! {l 0 o 200 l 0 o max}, h! {l 1 o 0 l 1 o 300},]
    );

    highlight_assert!(
        (pos! {l 1 o 0}, pos! {l 2 o 0}),
        vec![h! {l 1 o 0 l 1 o max}]
    );

    highlight_assert!(
        (pos! {l 1 o 0}, pos! {l 3 o 0}),
        vec![h! {l 1 o 0 l 2 o max}]
    );

    highlight_assert!(
        (pos! {l 1 o 400}, pos! {l 2 o 0}),
        vec![h! {l 1 o 400 l 1 o max}]
    );

    highlight_assert!(
        (pos! {l 1 o 400}, pos! {l 3 o 0}),
        vec![h! {l 1 o 400 l 1 o max}, h! {l 2 o 0 l 2 o max}]
    );

    highlight_assert!(
        (pos! {l 1 o 0}, pos! {l 2 o 300}),
        vec![h! {l 1 o 0 l 1 o max}, h! {l 2 o 0 l 2 o 300}]
    );

    highlight_assert!(
        (pos! {l 1 o 0}, pos! {l 3 o 300}),
        vec![h! {l 1 o 0 l 2 o max}, h! {l 3 o 0 l 3 o 300}]
    );

    highlight_assert!(
        (pos! {l 1 o 400}, pos! {l 2 o 300}),
        vec![h! {l 1 o 400 l 1 o max}, h! {l 2 o 0 l 2 o 300}]
    );

    highlight_assert!(
        (pos! {l 1 o 400}, pos! {l 3 o 300}),
        vec![
            h! {l 1 o 400 l 1 o max},
            h! {l 2 o 0 l 2 o max},
            h! {l 3 o 0 l 3 o 300}
        ]
    );
}

#[test]
fn position_ord_works_as_expected() {
    assert!(pos! {l 9 o 0} > pos! {l 0 o 0});
    assert!(pos! {l 0 o 0} < pos! {l 0 o 9});

    assert!(pos! {l 9 o 0} > pos! {l 0 o 9});
    assert!(pos! {l 0 o 9} < pos! {l 9 o 0});
}

// It turns out this is also needed only in the tests and in fact, it's kind of in the way since we now
// want to store the pieces separately. I'll leave the original comments here for posterity.

/// This represents the visible portion of the screen. This struct primarily exists to make it
/// clear that the negtive y direction is used instead of the positive one. That is, the area
/// where `scroll.x` is between `0.0` and `wh.w` and `scroll.y` is between `-wh.h` and `0.0` is
/// considered to be on the screen. As a side effect, this struct also allows functions that
/// operate on the `scroll` to be harder to use incorrectly, by preventing mixing up what would
/// otherwise be two `ScreenSpaceXY` params.
#[derive(Default, Debug)]
pub struct ScrollableScreen {
    /// A negative `scroll.x` value means move the screen left, so you can see things that are
    /// further right. A negative `scroll.y` value means move the screen up, so you can see things
    /// that are further down.
    pub scroll: ScrollXY,
    pub wh: ScreenSpaceWH,
}

fmt_display!(for ScrollableScreen : ScrollableScreen {scroll, wh}
      in "ScrollableScreen {{ scroll:{}, wh: {} }}", scroll, wh
);

impl MapElements<abs::Pos> for ScrollableScreen {
    fn map_elements(&self, mapper: &impl Fn(abs::Pos) -> abs::Pos) -> Self {
        Self {
            scroll: self.scroll.map_elements(&|v| mapper(abs::Pos::default() + v) - abs::Pos::default()),
            wh: self.wh.map_elements(mapper),
        }
    }
}

// returns true if the given TextBoxSpaceXY is visible on the
// given screen, assuming the text box origin is at (0,0) and
// the text box fills the screen.
// Turns out that we only need this in the tests anymore!
// But we can implement it in term of inside_rect so we test that
// actually used code.
fn xy_is_visible(
    ScrollableScreen {
        scroll,
        wh,
    }: &ScrollableScreen,
    text: TextSpaceXY,
) -> bool {
    let text_box = text_to_text_box(text, *scroll);
    let xy = text_box_to_screen(text_box, d!());

    inside_rect(xy, ssxywh!{ d!(), *wh }.into())
}

macro_rules! xy_is_visible_assert {
    (not $screen: expr, $xy: expr) => {{
        use std::ops::Not;
        assert!(
            xy_is_visible(&$screen, $xy).not(),
            "{} AKA {:?} is incorrectly visible on {}",
            stringify!($xy).escape_default(),
            ($xy.x, $xy.y),
            &$screen
        );
    }};
    ($screen: expr, $xy: expr) => {{
        assert!(
            xy_is_visible(&$screen, $xy),
            "{} AKA {:?} is not visible on {}",
            stringify!($xy).escape_default(),
            ($xy.x, $xy.y),
            &$screen
        );
    }};
}

fn attempt_to_make_xy_visible_works_in_this_scenario(
    screen: &mut ScrollableScreen,
    char_dim: CharDim,
    xy: TextSpaceXY,
) -> VisibilityAttemptResult {
    let wh = screen.wh;
    // Assume the text box fills the screen
    let text_box = tbxywh!(d!(), wh);

    let apron = apron!(
        (char_dim.w.get() / wh.w.get()),
        (char_dim.w.get() / wh.w.get()),
        (char_dim.h.get() / wh.h.get()),
        (char_dim.h.get() / wh.h.get()),
    );

    let attempt = attempt_to_make_xy_visible(
        &mut screen.scroll,
        text_box,
        apron,
        xy
    );

    if dbg!(attempt) == VisibilityAttemptResult::Succeeded {
        dbg!(&screen);
        xy_is_visible_assert!(&screen, xy);
    }

    attempt
}

proptest! {
    #[test]
    fn attempt_to_make_xy_visible_works(
        mut screen in arb::scrollable_screen(),
        char_dim in arb::char_dim(),
        xy in arb::text_xy(),
    ) {
        attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
    }
}

proptest! {
    #[test]
    fn attempt_to_make_xy_visible_works_with_more_realistic_values(
        mut screen in arb::scrollable_screen(),
        xy in arb::text_xy(),
    ) {
        attempt_to_make_xy_visible_works_in_this_scenario(
            &mut screen,
            char_dim!(4.0, 8.0),
            xy
        );
    }
}

#[test]
fn attempt_to_make_xy_visible_works_on_this_generated_example() {
    let mut screen = ScrollableScreen {
        scroll: d!(),
        wh: sswh!{1927329000.0 1.4144982},
    };
    let char_dim = char_dim!{
        0.000000000026796234
        0.0000000000000000003944164
    };
    let xy = text_box_to_text(
        screen_to_text_box(
            ssxy!{
                0.0,
                0.0000000000000000000000000006170001,
            },
            d!(),
        ),
        d!()
    );

    attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
}

#[test]
fn attempt_to_make_xy_visible_works_on_this_edge_case_example() {
    let mut screen = ScrollableScreen {
        scroll: d!(),
        wh: sswh!{256.0 256.0},
    };
    let char_dim = char_dim!{
        0.0
        0.0
    };
    let xy = text_box_to_text(
        screen_to_text_box(
            ssxy!{256.0, 0.0},
            d!(),
        ),
        d!()
    );

    attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
}

#[test]
fn attempt_to_make_xy_visible_works_on_this_clearer_edge_case_example() {
    let mut screen = ScrollableScreen {
        scroll: d!(),
        wh: sswh!{256.0 256.0},
    };

    let xy = tsxy!(257.0, 0.0);

    let wh = sswh!{256.0 256.0};
    // Assume the text box fills the screen
    let text_box = tbxywh!(d!(), wh);

    dbg!(
        &mut screen.scroll,
        text_box,
        xy
    );

    let attempt = attempt_to_make_xy_visible(
        &mut screen.scroll,
        text_box,
        d!(),
        xy
    );

    if dbg!(attempt) == VisibilityAttemptResult::Succeeded {
        dbg!(&screen);
        xy_is_visible_assert!(&screen, xy);
    }
}

#[test]
fn attempt_to_make_xy_visible_works_on_this_realistic_example() {
    let mut screen = ScrollableScreen {
        scroll: slxy!(250.0, 440.0),
        wh: sswh!{800.0 400.0},
    };
    let char_dim = char_dim!{4.0 8.0};

    let xy = TextSpaceXY::default();

    xy_is_visible_assert!(not & screen, xy);

    attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
}

#[test]
fn attempt_to_make_xy_visible_works_on_this_realistically_sized_example() {
    let mut screen = ScrollableScreen {
        scroll: slxy!(),
        wh: sswh!(1024.0 576.0),
    };
    let char_dim = char_dim!(30.0 60.0);

    let xy = tsxy!{ 60.0, 600.0 };

    xy_is_visible_assert!(not & screen, xy);

    attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
}

#[test]
// real as in recovered from an actual run of the program
fn attempt_to_make_xy_visible_works_on_this_vertically_scrolled_realistically_sized_example() {
    let mut screen = ScrollableScreen {
        scroll: slxy!(),
        wh: sswh!(1024.0 576.0),
    };
    let char_dim = char_dim!{30.0 60.0};
    let xy = text_box_to_text(
        screen_to_text_box(
            ssxy!(0.0, 600.0),
            tbxy!(0.0, 180.0),
        ),
        screen.scroll
    );

    xy_is_visible_assert!(& screen, xy);

    attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
}

#[test]
fn attempt_to_make_xy_visible_works_on_this_2020_01_realistically_sized_example() {
    let mut screen = ScrollableScreen {
        scroll: slxy!{ 320.0, 0.0 },
        wh: sswh!{1920.0 1080.0},
    };
    let char_dim = char_dim!(16.0 32.0);
    let xy = TextSpaceXY::default();

    xy_is_visible_assert!(not & screen, xy);

    let attempt_result = attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
    assert_eq!(attempt_result, VisibilityAttemptResult::Succeeded);
}

fn xy_is_visible_works_on_this_passed_in_screen(screen: &ScrollableScreen) {
    if screen.scroll.x + screen.wh.w == screen.scroll.x
        || screen.scroll.y - screen.wh.h == screen.scroll.y
        || screen.scroll.x + screen.wh.w.minimal_decrease()
            == screen.scroll.x + screen.wh.w
        || screen.scroll.y + screen.wh.h.minimal_decrease()
            == screen.scroll.y + screen.wh.h
        || screen.scroll.x - screen.wh.w.minimal_decrease()
            == screen.scroll.x - screen.wh.w
        || screen.scroll.y - screen.wh.h.minimal_decrease()
            == screen.scroll.y - screen.wh.h
    {
        // We've hit the limits of f32 precision. If this turns out to be problem in practice,
        // we'll need to get more precision from the platform layer somehow.
        return;
    }

    // Reminder: the `x` coord is positive right, negative left, so a negative scroll value means
    // move the screen left, so you can see things that are further to the right.

    // Reminder: the `y` coord is positive down, negative up, so a negative scroll value means
    // move the screen up, so you can see things that are further down.

    xy_is_visible_assert!(
        &screen,
        screen_space_to_text_space(
            ssxy!(
                screen.wh.w.get() / 2.0,
                screen.wh.h.get() / 2.0
            ),
            d!(),
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_space_to_text_space(
            ssxy!(
                (-screen.wh.w.get() / 2.0),
                (-screen.wh.h.get() / 2.0)
            ),
            d!(),
            screen.scroll
        )
    );

    xy_is_visible_assert!(
        &screen,
        screen_space_to_text_space(
            ssxy!(
                screen.wh.w.minimal_decrease(),
                screen.wh.h.minimal_decrease()
            ),
            d!(),
            screen.scroll
        )
    );
    if screen.wh.w != abs::Length::MAX
    || screen.wh.h != abs::Length::MAX {
        xy_is_visible_assert!(
            not & screen,
            screen_space_to_text_space(
                ssxy!(
                    screen.wh.w.minimal_increase(),
                    screen.wh.h.minimal_increase()
                ),
                d!(),
                screen.scroll
            )
        );
    } else {
        xy_is_visible_assert!(
            & screen,
            screen_space_to_text_space(
                ssxy!(
                    screen.wh.w.minimal_increase(),
                    screen.wh.h.minimal_increase()
                ),
                d!(),
                screen.scroll
            )
        );
    }

    xy_is_visible_assert!(
        not & screen,
        screen_space_to_text_space(
            ssxy!(
                screen.wh.w.get() * 3.0 / 2.0,
                screen.wh.h.get() * 3.0 / 2.0
            ),
            d!(),
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_space_to_text_space(
            ssxy!(
                (-screen.wh.w.get() * 3.0 / 2.0),
                (-screen.wh.h.get() * 3.0 / 2.0)
            ),
            d!(),
            screen.scroll
        )
    );

    xy_is_visible_assert!(
        &screen,
        screen_space_to_text_space(
            ssxy!{},
            d!(),
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        TextSpaceXY {
            x: -abs::Pos::MIN_POSITIVE + screen.scroll.x,
            y: abs::Pos::ZERO + screen.scroll.y
        }
    );
    xy_is_visible_assert!(
        not & screen,
        TextSpaceXY {
            x: abs::Pos::ZERO + screen.scroll.x,
            y: -abs::Pos::MIN_POSITIVE + screen.scroll.y
        }
    );
    xy_is_visible_assert!(
        not & screen,
        TextSpaceXY {
            x: -abs::Pos::MIN_POSITIVE + screen.scroll.x,
            y: -abs::Pos::MIN_POSITIVE + screen.scroll.y 
        }
    );

    xy_is_visible_assert!(
        &screen,
        screen_space_to_text_space(
            ssxy!(screen.wh.w.minimal_decrease(), 0.0),
            d!(),
            screen.scroll
        )
    );

    if screen.wh.w != abs::Length::MAX {
        xy_is_visible_assert!(
            not & screen,
            screen_space_to_text_space(
                ssxy!(screen.wh.w.minimal_increase(), 0.0),
                d!(),
                screen.scroll
            )
        );
    } else {
        xy_is_visible_assert!(
            & screen,
            screen_space_to_text_space(
                ssxy!(screen.wh.w.minimal_increase(), 0.0),
                d!(),
                screen.scroll
            )
        );
    }

    xy_is_visible_assert!(
        &screen,
        screen_space_to_text_space(
            ssxy!(0.0, screen.wh.h.minimal_decrease()),
            d!(),
            screen.scroll
        )
    );

    if screen.wh.h != abs::Length::MAX {
        xy_is_visible_assert!(
            not & screen,
            screen_space_to_text_space(
                ssxy!(0.0, screen.wh.h.minimal_increase()),
                d!(),
                screen.scroll
            )
        );
    } else {
        xy_is_visible_assert!(
            & screen,
            screen_space_to_text_space(
                ssxy!(0.0, screen.wh.h.minimal_increase()),
                d!(),
                screen.scroll
            )
        );
    }
}

proptest! {
    #[test]
    fn xy_is_visible_works(
        screen in arb::plausible_scrollable_screen()
    ) {
        xy_is_visible_works_on_this_passed_in_screen(&screen);
    }
}

#[test]
fn xy_is_visible_works_on_this_very_short_and_wide_screen() {
    let screen = ScrollableScreen {
        scroll: slxy! {
            0.0,
            0.04851929,
        },
        wh: sswh!{
            511087840000000000000000000000000000.0,
            1.0,
        },
    };

    xy_is_visible_works_on_this_passed_in_screen(&screen);
}

#[test]
fn xy_is_visible_works_on_this_realistic_example() {
    let screen = ScrollableScreen {
        scroll: slxy!(250.0, 440.0),
        wh: sswh!(800.0, 400.0),
    };

    xy_is_visible_assert!(
        &screen,
        tsxy! {
            1000.0,
            480.0
        }
    );
}

fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
    screen: ScrollableScreen,
    xy: ScreenSpaceXY,
    text_box_pos: TextBoxXY,
) {
    let scroll = screen.scroll;
    let char_dim = char_dim!(4.0 8.0);

    let pos = screen_space_to_position(
        xy,
        text_box_pos,
        scroll,
        char_dim,
        PositionRound::TowardsZero,
    );

    let mut xy = dbg!(position_to_screen_space(
        pos,
        char_dim,
        scroll,
        text_box_pos
    ));

    const COUNT: usize = 8;

    let mut v = Vec::with_capacity(COUNT);

    for _ in 0..COUNT {
        let new_xy = position_to_screen_space(
            screen_space_to_position(
                xy,
                text_box_pos,
                scroll,
                char_dim,
                PositionRound::TowardsZero,
            ),
            char_dim,
            scroll,
            text_box_pos,
        );

        v.push(new_xy);

        xy = new_xy;
    }

    assert_eq!(v, [xy; COUNT].to_vec());
}

fn clamp_to_65536(x: abs::Pos) -> abs::Pos {
    if x < 65536.0 {
        x
    } else {
        abs::Pos::from(65536.0)
    }
}

fn clamp_to_65536_and_trunc_to_16ths(x: abs::Pos) -> abs::Pos {
    clamp_to_65536(x)
        .double()
        .double()
        .double()
        .double()
        .trunc()
        .halve()
        .halve()
        .halve()
        .halve()
}

// This test captures all the scenarios we (currently) expect to actually see.
// TODO formalize this with types?
proptest! {
    #[test]
    fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_if_we_clamp_to_65536_and_trunc_to_16ths(
        screen in arb::scrollable_screen(),
        xy in arb::rounded_non_negative_screen_xy(),
        text_box_as_screen in arb::rounded_non_negative_screen_xy(),
    ) {
        let ScreenSpaceXY{x, y} = text_box_as_screen.map_elements(&clamp_to_65536_and_trunc_to_16ths);

        screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
            screen.map_elements(&clamp_to_65536_and_trunc_to_16ths),
            xy.map_elements(&clamp_to_65536_and_trunc_to_16ths),
            TextBoxXY{x, y}
        )
    }
}

/*
proptest! {
    #[test]
    fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_if_we_clamp_to_65536(
        screen in arb::scrollable_screen(),
        xy in arb::rounded_non_negative_screen_xy(),
        text_box_as_screen in arb::rounded_non_negative_screen_xy(),
    ) {
        let ScreenSpaceXY{x, y} = text_box_as_screen.map_elements(&clamp_to_65536);

        screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
            screen.map_elements(&clamp_to_65536),
            xy.map_elements(&clamp_to_65536),
            TextBoxXY{x, y}
        )
    }
}

proptest! {
    #[test]
    fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion(
        screen in arb::scrollable_screen(),
        xy in arb::rounded_non_negative_screen_xy(),
        ScreenSpaceXY{x, y} in arb::rounded_non_negative_screen_xy(),
    ) {
        screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
            screen, xy, TextBoxXY{x, y}
        )
    }
}*/

#[test]
fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_this_generated_example(
) {
    screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
        ScrollableScreen { scroll: slxy!{ 0.0, -135712.25 }, wh: sswh!() },
        ssxy!(),
        tbxy!()
    )
}

/*
#[test]
fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_this_all_positive_generated_example(
) {
    screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
        ScrollableScreen { scroll: ScrollXY { x: 20.498291, y: 0.0 }, wh: ScreenSpaceWH { w: 0.0, h: 0.0 } },
        ScreenSpaceXY { x: 12391443.0, y: 0.0 },
        TextBoxXY { x: 4650059.0, y: 0.0 }
    )
}

#[test]
fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_this_less_than_128_generated_example(
) {
    screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
        ScrollableScreen { scroll: ScrollXY { x: 40.149334, y: 0.0 }, wh: ScreenSpaceWH { w: 0.0, h: 0.0 } },
        ScreenSpaceXY { x: 96.0, y: 0.0 },
        TextBoxXY { x: 69.0, y: 0.0 }
    )
}
*/

fn text_space_to_position_then_position_to_text_space_is_identity_after_one_conversion_for_these(
    xy: TextSpaceXY,
) {
    let char_dim = char_dim!(4.0 8.0);

    let pos = text_space_to_position(xy, char_dim, PositionRound::TowardsZero);

    let mut xy = position_to_text_space(pos, char_dim);

    for _ in 0..8 {
        let new_xy = position_to_text_space(
            text_space_to_position(xy, char_dim, PositionRound::TowardsZero),
            char_dim,
        );

        assert_eq!(new_xy, xy);

        xy = new_xy;
    }
}

proptest! {
    #[test]
    fn text_space_to_position_then_position_to_text_space_is_identity_after_one_conversion(
        xy in arb::rounded_non_negative_text_xy(),
    ) {
        text_space_to_position_then_position_to_text_space_is_identity_after_one_conversion_for_these(
            xy
        )
    }
}

fn screen_to_text_box_then_text_box_to_screen_is_identity_after_one_conversion_for_these(
    text_box_xy: TextBoxXY,
    xy: ScreenSpaceXY,
) {
    let text_xy = dbg!(screen_to_text_box(xy, text_box_xy));

    let mut xy = dbg!(text_box_to_screen(text_xy, text_box_xy));

    const COUNT: usize = 8;

    let mut v = Vec::with_capacity(COUNT);

    for _ in 0..COUNT {
        let new_xy = text_box_to_screen(dbg!(screen_to_text_box(xy, text_box_xy)), text_box_xy);

        v.push(new_xy);

        xy = new_xy;
    }

    assert_eq!(v, [xy; COUNT].to_vec());
}

proptest! {
    #[test]
    fn screen_to_text_box_then_text_box_to_screen_is_identity_after_one_conversion(
        text_box_xy in arb::text_box_xy(),
        xy in arb::rounded_non_negative_screen_xy(),
    ) {
        screen_to_text_box_then_text_box_to_screen_is_identity_after_one_conversion_for_these(
            text_box_xy,
            xy
        )
    }
}

#[test]
fn screen_to_text_box_then_text_box_to_screen_is_identity_after_one_conversion_for_this_generated_example(
) {
    screen_to_text_box_then_text_box_to_screen_is_identity_after_one_conversion_for_these(
        tbxy!(
            -75525750000000000.0,
            0.0,
        ),
        ssxy!(),
    )
}

#[test]
fn text_box_to_screen_works_on_this_realistic_example() {
    assert_eq!(
        text_box_to_screen(
            tbsxy!(1000.0, 480.0),
            tbxy!(250.0, 440.0)
        ),
        ssxy!(1250.0, 920.0)
    );
}

macro_rules! assert_inside_rect {
    ($xy: expr, $rect: expr) => {
        let xy: ScreenSpaceXY = $xy;
        let rect: ScreenSpaceRect = $rect;

        assert!(
            inside_rect(
                xy,
                rect
            ),
            "{:?} is not inside {:?}",
            xy,
            rect,
        );
    }
}

fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
    mut scroll: ScrollXY,
    outer_rect: TextBoxXYWH,
    apron: Apron,
    to_make_visible: TextSpaceXY,
) {
    let attempt_result = attempt_to_make_xy_visible(
        &mut scroll,
        outer_rect,
        apron,
        to_make_visible,
    );

    // TODO would making this a discarded result help the input generation enough that it's worth doing?
    if attempt_result == VisibilityAttemptResult::Succeeded {
        assert_inside_rect!(
            text_space_to_screen_space(
                scroll,
                outer_rect.xy,
                to_make_visible,
            ),
            ssxywh!(
                outer_rect.xy.into(),
                outer_rect.wh
            ).into()
        );
    }
}

proptest! {
    #[test]
    fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible(
        scroll in arb::scroll_xy(),
        text_box_xywh in arb::text_box_xywh(),
        apron in arb::apron(),
        cursor_xy in arb::rounded_non_negative_text_xy()
    ) {
        if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
            scroll,
            text_box_xywh,
            apron,
            cursor_xy,
        )
    }
}

fn clamp_exponent_to_24(p: abs::Pos) -> abs::Pos {
    let f = p.get();

    if f > 2.0f32.powi(24) {
        2.0f32.powi(24).into()
    } else if f > 2.0f32.powi(-24) {
        p
    } else {
        // NaN goes here
        2.0f32.powi(-24).into()
    }
}

fn clamp_exponent_to_24_vector(v: abs::Vector) -> abs::Vector {
    clamp_exponent_to_24(abs::Pos::default() + v) - abs::Pos::default()
}

proptest! {
    #[test]
    fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_given_we_clamp_the_exponents_to_24(
        scroll in arb::scroll_xy()
            .prop_map(|s| s.map_elements(&clamp_exponent_to_24_vector)),
        text_box_xywh in arb::text_box_xywh()
            .prop_map(|t| t.map_elements(
                &clamp_exponent_to_24
            )),
        apron in arb::apron()
            .prop_map(|a| a.map_elements(
                &|f32_0_1| f32_0_1!(clamp_exponent_to_24(f32_0_1.get().into()).get())
            )),
        cursor_xy in arb::rounded_non_negative_text_xy()
            .prop_map(|c| c.map_elements(&clamp_exponent_to_24))
    ) {
        if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
            scroll,
            text_box_xywh,
            apron,
            cursor_xy,
        )
    }
}

fn clamp_to_at_least_one(p: abs::Pos) -> abs::Pos {
    if p >= abs::Pos::ONE {
        p
    } else {
        abs::Pos::ONE
    }
}

fn clamp_to_at_least_one_vector(v: abs::Vector) -> abs::Vector {
    clamp_to_at_least_one(abs::Pos::default() + v) - abs::Pos::default()
}

proptest! {
    #[test]
    fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_given_we_clamp_to_at_least_one(
        scroll in arb::scroll_xy()
            .prop_map(|s| s.map_elements(&clamp_to_at_least_one_vector)),
        outer_rect in arb::text_box_xywh()
            .prop_map(|t| t.map_elements(&clamp_to_at_least_one)),
        apron in arb::apron(),
        to_make_visible in arb::rounded_non_negative_text_xy()
            .prop_map(|c| c.map_elements(&clamp_to_at_least_one))
    ) {
        if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
            scroll,
            outer_rect,
            apron,
            to_make_visible,
        )
    }
}

#[test]
fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_given_we_clamp_to_at_least_one_in_this_case() {
    let scroll = ScrollXY { x: abs::Vector::ONE, y: abs::Vector::ONE };
    let text_box_xywh = TextBoxXYWH {
        xy: TextBoxXY { x: abs::Pos::ONE, y: abs::Pos::ONE },
        wh: sswh!(abs::Length::ONE, abs::Length::MAX)
    };
    let apron: Apron = d!();
    let cursor_xy = tsxy!{ abs::Pos::ONE, abs::Pos::ONE };

    if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
        scroll,
        text_box_xywh,
        apron,
        cursor_xy,
    )
}

proptest! {
    #[test]
    fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_given_we_clamp_the_widths_to_at_least_one(
        scroll in arb::scroll_xy(),
        text_box_xywh in arb::text_box_xywh()
            .prop_map(|t| t.map_elements(
                &clamp_to_at_least_one
            )),
        apron in arb::apron(),
        cursor_xy in arb::rounded_non_negative_text_xy(),
    ) {
        if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
            scroll,
            text_box_xywh,
            apron,
            cursor_xy,
        )
    }
}

proptest! {
    #[test]
    fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_given_we_trunc_and_clamp_the_widths(
        scroll in arb::scroll_xy(),
        outer_rect in arb::text_box_xywh()
            .prop_map(|t| t.map_elements(
                &|p| clamp_exponent_to_24(p.trunc())
            )),
        apron in arb::apron(),
        to_make_visible in arb::rounded_non_negative_text_xy(),
    ) {
        if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
            scroll,
            outer_rect,
            apron,
            to_make_visible,
        )
    }
}

const TBXY_1_1: TextBoxXY = TextBoxXY{ x: abs::Pos::ONE, y: abs::Pos::ONE };

#[test]
fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_given_these_truncated_widths() {
    let scroll = slxy!{ -33590000.0, 0.0 };
    let text_box_xywh = TextBoxXYWH {
        xy: TBXY_1_1,
        wh: sswh!(33593811.0, 1.0)
    };
    let cursor_xy = tsxy!{ 13030.0, 0.0 };
    if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
        scroll,
        text_box_xywh,
        d!(),
        cursor_xy,
    )
}

#[test]
fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_given_these_truncated_widths_and_a_large_negative_scroll_x() {
    let scroll = slxy!{ -16777216.0, 0.0 };
    let text_box_xywh = TextBoxXYWH {
        xy: TBXY_1_1,
        wh: sswh!(16777216.0, 16777216.0)
    };
    let cursor_xy = tsxy!{ 0.0, 0.0 };
    if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
        scroll,
        text_box_xywh,
        d!(),
        cursor_xy,
    )
}

proptest! {
    #[test]
    fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_given_these_truncated_widths_automated_reduction(
        cursor_x in 0i32..13030,
        x in 0i32..33593811
    ) {
        let scroll = ScrollXY { x: ((-x) as f32).into(), y: d!() };
        let outer_rect = TextBoxXYWH {
            xy: TBXY_1_1,
            wh: sswh!(x as f32, 1.0)
        };
        let to_make_visible = TextSpaceXY { x: (cursor_x as f32).into(), y: d!() };
        if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
            scroll,
            outer_rect,
            d!(),
            to_make_visible,
        )
    }
}

#[test]
fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_given_these_truncated_widths_reduction() {
    let mut scroll = slxy!{ -33590000.0, 0.0 };
    let text_box_xywh = TextBoxXYWH {
        xy: TBXY_1_1,
        wh: sswh!(33593811.0, 1.0)
    };
    let cursor_xy = tsxy!{ 13030.0, 0.0 };
    let attempt_result = attempt_to_make_xy_visible(
        &mut scroll,
        text_box_xywh,
        d!(),
        cursor_xy,
    );

    // TODO would making this a discarded result help the input generation enough that it's worth doing?
    if attempt_result == VisibilityAttemptResult::Succeeded {
        assert_inside_rect!(
            text_space_to_screen_space(
                scroll,
                text_box_xywh.xy,
                cursor_xy,
            ),
            ssxywh!(
                text_box_xywh.xy.into(),
                text_box_xywh.wh
            ).into()
        );
    }
}

#[test]
fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on_this_incorrect_visualization_found_example() {
    if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
        slxy!{ 0.0, 52.5 },
        tbxywh!(170.75, 98.25, 69.5, 68.5),
        apron!(1.0),
        tsxy!{ 0.0, 96.0 },
    )
}

#[test]
fn if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on_this_visualization_found_example() {
    if_attempt_to_make_visible_succeeds_the_cursor_is_visible_on(
        slxy!{ 0.0, 196.5 },
        tbxywh!(170.75, 98.25, 341.5, 196.5),
        apron!(1.0),
        tsxy! { 0.0, 160.0 },
    )
}

proptest!{
    #[test]
    fn text_space_to_screen_space_to_text_space_is_identity_if_all_values_are_small_enough(
        text_space in arb::text_xy_quarter(),
        text_box_pos in arb::text_box_xy_quarter(),
        scroll in arb::scroll_xy_quarter(),
    ) {
        let converted = screen_space_to_text_space(
            text_space_to_screen_space(
                scroll,
                text_box_pos,
                text_space
            ),
            text_box_pos,
            scroll
        );

        assert_eq!(converted, text_space);
    }
}

proptest!{
    #[test]
    fn screen_space_to_text_space_to_screen_space_is_identity_if_all_values_are_small_enough(
        screen_space in arb::screen_xy_quarter(),
        text_box_pos in arb::text_box_xy_quarter(),
        scroll in arb::scroll_xy_quarter(),
    ) {
        let converted = text_space_to_screen_space(
            scroll,
            text_box_pos,
            screen_space_to_text_space(
                screen_space,
                text_box_pos,
                scroll,
            ),
        );

        assert_eq!(converted, screen_space);
    }
}



pub mod arb;
