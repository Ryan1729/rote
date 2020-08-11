

use screen_space::*;
use crate::screen_positioning::{
    MapElements,
    ScreenSpaceWH,
};

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
// It turns out this is also needed only in the tests and in fact, it's kind of in the way since we now
// want to store the pieces separately. I'll leave the original comments here for posterity.

#[derive(Default, Debug)]
pub struct ScrollableScreen {
    pub scroll: ScrollXY,
    pub wh: ScreenSpaceWH,
}

impl MapElements<abs::Pos> for ScrollableScreen {
    fn map_elements(&self, mapper: &impl Fn(abs::Pos) -> abs::Pos) -> Self {
        Self { 
            scroll: self.scroll.map_elements(mapper),
            wh: self.wh.map_elements(mapper),
        }
    }
}
