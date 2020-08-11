use super::*;
use proptest::prelude::{Strategy};
use pub_arb_abs::{abs_pos, abs_length};

pub fn scroll_xy() -> impl Strategy<Value = ScrollXY> {
    let spec = abs_pos();
    (spec, spec).prop_map(|(x, y)| slxy!{ x, y })
}

pub fn wh() -> impl Strategy<Value = ScreenSpaceWH> {
    let strat = abs_length();
    (strat, strat).prop_map(|(w, h)| ScreenSpaceWH { w, h })
}

pub fn scrollable_screen() -> impl Strategy<Value = ScrollableScreen> {
    (scroll_xy(), wh()).prop_map(|(scroll, wh)| ScrollableScreen {
        scroll,
        wh,
    })
}

