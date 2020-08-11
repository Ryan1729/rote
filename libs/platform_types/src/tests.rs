

use screen_space::*;
use crate::screen_positioning::{
    MapElements,
    ScreenSpaceWH,
};
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

pub mod arb;
