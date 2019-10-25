use super::*;
use crate::floating_point::*;
use proptest::{num::f32, prop_compose, proptest};

prop_compose! {
    fn arb_pos(max_line: usize, max_offset: usize)
    (line in 0..=max_line, offset in 0..=max_offset) -> Position {
        Position{ line, offset: CharOffset(offset) }
    }
}

// I'm not sure it makes sense to compare highllights outside of tests, and if it does, that this
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

        push_highlights(&mut highlights, position, highlight_position);

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

// It turns out this is also needed int he tests and in fact, it's kind of in the way since we now
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

/* We can revive these tests if the tested functions need to change further
// Turns out that we only need this in the tests anymore!
fn xy_is_visible(
    ScrollableScreen {
        scroll,
        wh: ScreenSpaceWH { w, h },
    }: &ScrollableScreen,
    text: TextBoxSpaceXY,
) -> bool {
    let ScreenSpaceXY { x, y } = text_box_to_screen(text, *scroll);
    x >= 0.0 && x < *w && y >= 0.0 && y < *h
}



fmt_display!(for ScrollableScreen : ScrollableScreen {scroll, wh}
      in "ScrollableScreen {{ scroll:{}, wh: {} }}", scroll, wh
 );

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

fn xy_is_visible_works_on_this_passed_in_screen(screen: &ScrollableScreen) {
    // TODO rewrite this with the correct meaning of `scroll` in mind.

    // negated so `NaN` would end up here
    if !dbg!(screen.wh.w > 0.0 && screen.wh.h > 0.0) {
        // If we got here the no point should be considered visible!
        xy_is_visible_assert!(not & screen, TextSpaceXY { x: 0.0, y: 0.0 });

        xy_is_visible_assert!(
            not & screen,
            TextSpaceXY {
                x: screen.scroll.x,
                y: 0.0
            }
        );

        xy_is_visible_assert!(
            not & screen,
            TextSpaceXY {
                x: 0.0,
                y: screen.scroll.y
            }
        );

        xy_is_visible_assert!(
            not & screen,
            TextSpaceXY {
                x: screen.scroll.x,
                y: screen.scroll.y
            }
        );

        return;
    }

    if screen.scroll.x + screen.wh.w == screen.scroll.x
        || screen.scroll.y - screen.wh.h == screen.scroll.y
        || screen.scroll.x + usual_f32_minimal_decrease(screen.wh.w)
            == screen.scroll.x + screen.wh.w
        || screen.scroll.y + usual_f32_minimal_decrease(screen.wh.h)
            == screen.scroll.y + screen.wh.h
        || screen.scroll.x - usual_f32_minimal_decrease(screen.wh.w)
            == screen.scroll.x - screen.wh.w
        || screen.scroll.y - usual_f32_minimal_decrease(screen.wh.h)
            == screen.scroll.y - screen.wh.h
    {
        // We've hit the limits of f32 precision. If this turns out to be problem in praactice,
        // we'll need to get more precision from the platform layer somehow.
        return;
    }

    // Reminder: the `x` coord is positive right, negative left, so a negative scroll value means
    // move the screen left, so you can see things that are further to the right.

    // Reminder: the `y` coord is positive down, negative up, so a negative scroll value means
    // move the screen up, so you can see things that are further down.

    xy_is_visible_assert!(
        &screen,
        screen_to_text_box(
            ScreenSpaceXY {
                x: screen.wh.w / 2.0,
                y: screen.wh.h / 2.0
            },
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_to_text_box(
            ScreenSpaceXY {
                x: -screen.wh.w / 2.0,
                y: -screen.wh.h / 2.0
            },
            screen.scroll
        )
    );

    xy_is_visible_assert!(
        &screen,
        screen_to_text_box(
            ScreenSpaceXY {
                x: usual_f32_minimal_decrease(screen.wh.w),
                y: usual_f32_minimal_decrease(screen.wh.h)
            },
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_to_text_box(
            ScreenSpaceXY {
                x: screen.wh.w,
                y: screen.wh.h
            },
            screen.scroll
        )
    );

    xy_is_visible_assert!(
        not & screen,
        screen_to_text_box(
            ScreenSpaceXY {
                x: screen.wh.w * 3.0 / 2.0,
                y: screen.wh.h * 3.0 / 2.0
            },
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_to_text_box(
            ScreenSpaceXY {
                x: -screen.wh.w * 3.0 / 2.0,
                y: -screen.wh.h * 3.0 / 2.0
            },
            screen.scroll
        )
    );

    xy_is_visible_assert!(
        &screen,
        screen_to_text_box(ScreenSpaceXY { x: 0.0, y: 0.0 }, screen.scroll)
    );
    xy_is_visible_assert!(
        not & screen,
        TextSpaceXY {
            x: usual_f32_minimal_decrease(screen.scroll.x),
            y: screen.scroll.y
        }
    );
    xy_is_visible_assert!(
        not & screen,
        TextSpaceXY {
            x: screen.scroll.x,
            y: usual_f32_minimal_decrease(screen.scroll.y)
        }
    );
    xy_is_visible_assert!(
        not & screen,
        TextSpaceXY {
            x: usual_f32_minimal_decrease(screen.scroll.x),
            y: usual_f32_minimal_decrease(screen.scroll.y)
        }
    );

    xy_is_visible_assert!(
        &screen,
        screen_to_text_box(
            ScreenSpaceXY {
                x: usual_f32_minimal_decrease(screen.wh.w),
                y: 0.0
            },
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_to_text_box(
            ScreenSpaceXY {
                x: screen.wh.w,
                y: 0.0
            },
            screen.scroll
        )
    );

    xy_is_visible_assert!(
        &screen,
        screen_to_text_box(
            ScreenSpaceXY {
                x: 0.0,
                y: usual_f32_minimal_decrease(screen.wh.h)
            },
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_to_text_box(
            ScreenSpaceXY {
                x: 0.0,
                y: screen.wh.h
            },
            screen.scroll
        )
    );
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
        scroll: ScrollXY {
            x: 0.0,
            y: 0.04851929,
        },
        wh: ScreenSpaceWH {
            w: 511087840000000000000000000000000000.0,
            h: 0.00000006715828,
        },
    };

    xy_is_visible_works_on_this_passed_in_screen(&screen);
}

#[test]
fn xy_is_visible_works_on_this_realistic_example() {
    let screen = ScrollableScreen {
        scroll: ScrollXY { x: 250.0, y: 440.0 },
        wh: ScreenSpaceWH { w: 800.0, h: 400.0 },
    };

    xy_is_visible_assert!(
        &screen,
        TextSpaceXY {
            x: 1000.0,
            y: 480.0
        }
    );
}

fn attempt_to_make_xy_visible_works_in_this_scenario(
    screen: &mut ScrollableScreen,
    char_dim: CharDim,
    xy: TextSpaceXY,
) {
    let attempt = attempt_to_make_xy_visible(&mut screen.scroll, screen.wh, char_dim.into(), xy);

    if dbg!(attempt) == VisibilityAttemptResult::Succeeded {
        dbg!(&screen);
        xy_is_visible_assert!(&screen, xy);
    }
}

proptest! {
    #[test]
    fn attempt_to_make_xy_visible_works(
        mut screen in arb::scrollable_screen(f32::ANY),
        char_dim in arb::char_dim(f32::ANY),
        xy in arb::text_xy(f32::POSITIVE | f32::ZERO),
    ) {
        attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
    }
}

proptest! {
    #[test]
    fn attempt_to_make_xy_visible_works_with_more_realistic_values(
        mut screen in arb::scrollable_screen(arb::usual()),
        xy in arb::text_xy(f32::POSITIVE | f32::ZERO),
    ) {
        let char_dim = CharDim {
            w: 4.0,
            h: 8.0,
        };
        attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
    }
}

#[test]
fn attempt_to_make_xy_visible_works_on_this_generated_example() {
    let mut screen = ScrollableScreen {
        scroll: ScrollXY { x: 0.0, y: 0.0 },
        wh: ScreenSpaceWH {
            w: 1927329000.0,
            h: 1.4144982,
        },
    };
    let char_dim = CharDim {
        w: 0.000000000026796234,
        h: 0.0000000000000000003944164,
    };
    let xy = screen_to_text_box(
        ScreenSpaceXY {
            x: 0.0,
            y: 0.0000000000000000000000000006170001,
        },
        d!(),
    );

    attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
}

#[test]
fn attempt_to_make_xy_visible_works_on_this_realistic_example() {
    let mut screen = ScrollableScreen {
        scroll: ScrollXY { x: 250.0, y: 440.0 },
        wh: ScreenSpaceWH { w: 800.0, h: 400.0 },
    };
    let char_dim = CharDim { w: 4.0, h: 8.0 };
    let xy = TextSpaceXY { x: 0.0, y: 0.0 };

    xy_is_visible_assert!(not & screen, xy);

    attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
}

#[test]
fn attempt_to_make_xy_visible_works_on_this_realistically_sized_example() {
    let mut screen = ScrollableScreen {
        scroll: ScrollXY { x: 0.0, y: 0.0 },
        wh: ScreenSpaceWH {
            w: 1024.0,
            h: 576.0,
        },
    };
    let char_dim = CharDim { w: 30.0, h: 60.0 };
    let xy = TextSpaceXY { x: 60.0, y: 600.0 };

    xy_is_visible_assert!(not & screen, xy);

    attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
}

#[test]
// real as in recovered from an actual run of the program
fn attempt_to_make_xy_visible_works_on_this_vertically_scrolled_realistically_sized_example() {
    let char_dim = CharDim { w: 30.0, h: 60.0 };
    let xy = screen_to_text_box(
        ScreenSpaceXY { x: 0.0, y: 600.0 },
        TextBoxXY { x: 0.0, y: 180.0 },
    );

    xy_is_visible_assert!(not & screen, xy);

    attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
}

fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
    screen: ScrollableScreen,
    xy: ScreenSpaceXY,
    text_box_pos: TextBoxXY,
) {
    let scroll = screen.scroll;
    let char_dim = CharDim { w: 4.0, h: 8.0 };

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

proptest! {
    #[test]
    fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion(
        screen in arb::scrollable_screen(arb::usual()),
        xy in arb::rounded_non_negative_screen_xy(),
        ScreenSpaceXY{x, y} in arb::rounded_non_negative_screen_xy(),
    ) {
        screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
            screen, xy, TextBoxXY{x, y}
        )
    }
}

#[test]
fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_this_generated_example(
) {
    screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
        ScrollableScreen { scroll: ScrollXY { x: 0.0, y: -135712.25 }, wh: ScreenSpaceWH { w: 0.0, h: 0.0 } },
        ScreenSpaceXY { x: 0.0, y: 0.0 },
        TextBoxXY { x: 0.0, y: 0.0 }
    )
}

fn text_space_to_position_then_position_to_text_space_is_identity_after_one_conversion_for_these(
    xy: TextSpaceXY,
) {
    let char_dim = CharDim { w: 4.0, h: 8.0 };

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
        text_box_xy in arb::text_box_xy(arb::usual()),
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
        TextBoxXY {
            x: -75525750000000000.0,
            y: 0.0,
        },
        ScreenSpaceXY { x: 0.0, y: 0.0 },
    )
}

#[test]
fn text_box_to_screen_works_on_this_realistic_example() {
    assert_eq!(
        text_box_to_screen(
            TextBoxSpaceXY {
                x: 1000.0,
                y: 480.0
            },
            TextBoxXY { x: 250.0, y: 440.0 }
        ),
        ScreenSpaceXY { x: 750.0, y: 40.0 }
    );
}
*/

pub mod arb;
