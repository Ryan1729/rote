#![deny(unused)]

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests{
use screen_space::*;
use proptest::prelude::{Strategy};
use pub_arb_abs::{abs_pos, abs_length};

#[derive(Default, Debug)]
pub struct ScrollableScreen {
    pub scroll: ScrollXY,
    pub wh: (),
}

pub fn scroll_xy() -> impl Strategy<Value = ScrollXY> {
    let spec = abs_pos();
    (spec, spec).prop_map(|(x, y)| slxy!{ x, y })
}

pub fn wh() -> impl Strategy<Value = ()> {
    let strat = abs_length();
    (strat, strat).prop_map(|(_w, _h)| ())
}

pub fn scrollable_screen() -> impl Strategy<Value = ScrollableScreen> {
    (scroll_xy(), wh()).prop_map(|(scroll, wh)| ScrollableScreen {
        scroll,
        wh,
    })
}
}
