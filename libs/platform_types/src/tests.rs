use super::*;
use proptest::{num::f32, prop_compose, proptest};
use std::cmp::Ordering;

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

impl Ord for OrdHighlight {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0
            .min
            .cmp(&other.0.min)
            .then_with(|| self.0.max.cmp(&other.0.max))
    }
}

impl PartialOrd for OrdHighlight {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for OrdHighlight {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}
impl Eq for OrdHighlight {}

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

const SIGN_BIT: u32 = 0x8000_0000;
const ALL_BUT_SIGN_BIT: u32 = 0x7fff_ffff;

// Assumes x is one of the "usual" `f32`s, AKA not sub/denormal, Infinity or NaN.
// So normal numbers or 0. This does not imply that the output is a usual f32.
fn usual_f32_minimal_increase(x: f32) -> f32 {
    let non_sign_bits = x.to_bits() & ALL_BUT_SIGN_BIT;
    // if is 0 or -0
    if non_sign_bits == 0 {
        std::f32::MIN_POSITIVE
    } else {
        let sign_bit = x.to_bits() & SIGN_BIT;
        let sign = if sign_bit == 0 { 1 } else { -1 };
        f32::from_bits(sign_bit | (non_sign_bits as i32 + sign) as u32)
    }
}

// Assumes x is one of the "usual" `f32`s, AKA not sub/denormal, Infinity or NaN.
// So normal numbers or 0. This does not imply that the output is a usual f32.
fn usual_f32_minimal_decrease(x: f32) -> f32 {
    let non_sign_bits = x.to_bits() & ALL_BUT_SIGN_BIT;
    // if is 0 or -0
    if non_sign_bits == 0 {
        -std::f32::MIN_POSITIVE
    } else {
        let sign_bit = x.to_bits() & SIGN_BIT;
        let sign = if sign_bit == 0 { 1 } else { -1 };
        f32::from_bits(sign_bit | (non_sign_bits as i32 - sign) as u32)
    }
}

// meta
proptest! {
    #[test]
    fn usual_f32_minimal_increase_outputs_usual_f32s(
        x in arb::usual(),
    ) {
        use std::num::FpCategory::{Zero, Normal};
        let category = usual_f32_minimal_increase(x).classify();
        assert!(
            category == Zero || category == Normal,
            "category was {:?}, not Zero or Normal",
            category
        );
    }
}

proptest! {
    #[test]
    fn usual_f32_minimal_decrease_outputs_usual_f32s(
        x in arb::usual(),
    ) {
        use std::num::FpCategory::{Zero, Normal};
        let category = usual_f32_minimal_decrease(x).classify();
        assert!(
            category == Zero || category == Normal,
            "category was {:?}, not Zero or Normal",
            category
        );
    }
}

proptest! {
    #[test]
    fn usual_f32_minimal_increase_increases(
        old in arb::usual(),
    ) {
        let new = usual_f32_minimal_increase(old);
        assert!(
            new > old,
            "{:?} <= {:?}\n{:b} <= {:b}",
            new,
            old,
            new.to_bits(),
            old.to_bits()
        );
    }
}

proptest! {
    #[test]
    fn usual_f32_minimal_decrease_decreases(
        old in arb::usual(),
    ) {
        let new = usual_f32_minimal_decrease(old);
        assert!(
            new < old,
            "{:?} >= {:?}\n{:b} >= {:b}",
            new,
            old,
            new.to_bits(),
            old.to_bits()
        );
    }
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
        screen_to_text(
            ScreenSpaceXY {
                x: screen.wh.w / 2.0,
                y: screen.wh.h / 2.0
            },
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_to_text(
            ScreenSpaceXY {
                x: -screen.wh.w / 2.0,
                y: -screen.wh.h / 2.0
            },
            screen.scroll
        )
    );

    xy_is_visible_assert!(
        &screen,
        screen_to_text(
            ScreenSpaceXY {
                x: usual_f32_minimal_decrease(screen.wh.w),
                y: usual_f32_minimal_decrease(screen.wh.h)
            },
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_to_text(
            ScreenSpaceXY {
                x: screen.wh.w,
                y: screen.wh.h
            },
            screen.scroll
        )
    );

    xy_is_visible_assert!(
        not & screen,
        screen_to_text(
            ScreenSpaceXY {
                x: screen.wh.w * 3.0 / 2.0,
                y: screen.wh.h * 3.0 / 2.0
            },
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_to_text(
            ScreenSpaceXY {
                x: -screen.wh.w * 3.0 / 2.0,
                y: -screen.wh.h * 3.0 / 2.0
            },
            screen.scroll
        )
    );


    xy_is_visible_assert!(
        &screen,
        screen_to_text(ScreenSpaceXY { x: 0.0, y: 0.0 }, screen.scroll)
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
        screen_to_text(
            ScreenSpaceXY {
                x: usual_f32_minimal_decrease(screen.wh.w),
                y: 0.0
            },
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_to_text(
            ScreenSpaceXY {
                x: screen.wh.w,
                y: 0.0
            },
            screen.scroll
        )
    );

    xy_is_visible_assert!(
        &screen,

        screen_to_text(
            ScreenSpaceXY {
                x: 0.0,
                y: usual_f32_minimal_decrease(screen.wh.h)
            },
            screen.scroll
        )
    );
    xy_is_visible_assert!(
        not & screen,
        screen_to_text(
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
    let attempt = attempt_to_make_xy_visible(screen, char_dim.into(), xy);

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
    let xy = screen_to_text(
        ScreenSpaceXY {
            x: 0.0,
            y: 0.0000000000000000000000000006170001,
        },
        screen.scroll,
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
    let mut screen = ScrollableScreen {
        scroll: ScrollXY { x: 0.0, y: 180.0 },
        wh: ScreenSpaceWH {
            w: 1024.0,
            h: 576.0,
        },
    };
    let char_dim = CharDim { w: 30.0, h: 60.0 };
    let xy = screen_to_text(ScreenSpaceXY { x: 0.0, y: 600.0 }, screen.scroll);

    xy_is_visible_assert!(not & screen, xy);

    attempt_to_make_xy_visible_works_in_this_scenario(&mut screen, char_dim, xy);
}

fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
    screen: ScrollableScreen,
    xy: ScreenSpaceXY,
) {
    let scroll = screen.scroll;
    let char_dim = CharDim { w: 4.0, h: 8.0 };

    let pos = dbg!(screen_space_to_position(
        xy,
        char_dim,
        scroll,
        PositionRound::TowardsZero
    ));

    let mut xy = dbg!(position_to_screen_space(pos, char_dim, scroll));

    const COUNT: usize = 8;

    let mut v = Vec::with_capacity(COUNT);

    for _ in 0..COUNT {
        let new_xy = position_to_screen_space(
            dbg!(screen_space_to_position(
                xy,
                char_dim,
                scroll,
                PositionRound::TowardsZero
            )),
            char_dim,
            scroll,
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
    ) {
        screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
            screen, xy
        )
    }
}

#[test]
fn screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_this_generated_example(
) {
    screen_space_to_position_then_position_to_screen_space_is_identity_after_one_conversion_for_these(
        ScrollableScreen { scroll: ScrollXY { x: 0.0, y: -135712.25 }, wh: ScreenSpaceWH { w: 0.0, h: 0.0 } },
        ScreenSpaceXY { x: 0.0, y: 0.0 }
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

fn screen_to_text_then_text_to_screen_is_identity_after_one_conversion_for_these(
    screen: ScrollableScreen,
    xy: ScreenSpaceXY,
) {
    let scroll = screen.scroll;

    let text_xy = dbg!(screen_to_text(xy, scroll));

    let mut xy = dbg!(text_to_screen(text_xy, scroll));

    const COUNT: usize = 8;

    let mut v = Vec::with_capacity(COUNT);

    for _ in 0..COUNT {
        let new_xy = text_to_screen(dbg!(screen_to_text(xy, scroll)), scroll);

        v.push(new_xy);

        xy = new_xy;
    }

    assert_eq!(v, [xy; COUNT].to_vec());
}

proptest! {
    #[test]
    fn screen_to_text_then_text_to_screen_is_identity_after_one_conversion(
        screen in arb::scrollable_screen(arb::usual()),
        xy in arb::rounded_non_negative_screen_xy(),
    ) {
        screen_to_text_then_text_to_screen_is_identity_after_one_conversion_for_these(
            screen,
            xy
        )
    }
}

#[test]
fn screen_to_text_then_text_to_screen_is_identity_after_one_conversion_for_this_generated_example() {
    screen_to_text_then_text_to_screen_is_identity_after_one_conversion_for_these(
        ScrollableScreen { scroll: ScrollXY { x: -75525750000000000.0, y: 0.0 }, wh: ScreenSpaceWH { w: 0.0, h: 0.0 } },
        ScreenSpaceXY { x: 0.0, y: 0.0 }
    )
}


mod arb;
