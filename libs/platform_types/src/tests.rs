#![cfg_attr(feature = "pub_arb", allow(dead_code))]
#![cfg_attr(feature = "pub_arb", allow(unused_macros))]
#![cfg_attr(feature = "pub_arb", allow(unused_imports))]


use screen_space::*;
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
use proptest::{prop_compose, proptest, num::f32, strategy::Strategy};

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
            scroll: self.scroll.map_elements(mapper),
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

fn xy_is_visible_works_on_this_passed_in_screen(screen: &ScrollableScreen) {
    if screen.scroll.x + screen.wh.w.get() == screen.scroll.x
        || screen.scroll.y - screen.wh.h.get() == screen.scroll.y
        || screen.scroll.x + screen.wh.w.minimal_decrease()
            == screen.scroll.x + screen.wh.w.get()
        || screen.scroll.y + screen.wh.h.minimal_decrease()
            == screen.scroll.y + screen.wh.h.get()
        || screen.scroll.x - screen.wh.w.minimal_decrease()
            == screen.scroll.x - screen.wh.w.get()
        || screen.scroll.y - screen.wh.h.minimal_decrease()
            == screen.scroll.y - screen.wh.h.get()
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
            x: screen.scroll.x - abs::Pos::MIN_POSITIVE,
            y: screen.scroll.y
        }
    );
    xy_is_visible_assert!(
        not & screen,
        TextSpaceXY {
            x: screen.scroll.x,
            y: screen.scroll.y - abs::Pos::MIN_POSITIVE
        }
    );
    xy_is_visible_assert!(
        not & screen,
        TextSpaceXY {
            x: screen.scroll.x - abs::Pos::MIN_POSITIVE,
            y: screen.scroll.y - abs::Pos::MIN_POSITIVE
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
    if x < abs::Pos::from(65536.0) {
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

fn clamp_to_at_least_one(p: abs::Pos) -> abs::Pos {
    if p >= abs::Pos::ONE {
        p
    } else {
        abs::Pos::ONE
    }
}

const TBXY_1_1: TextBoxXY = TextBoxXY{ x: abs::Pos::ONE, y: abs::Pos::ONE };

pub mod arb;
