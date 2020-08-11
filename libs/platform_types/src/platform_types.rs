#![deny(unused)]

#[cfg(test)]
pub mod tests{
use proptest::prelude::{Strategy};
use pub_arb_abs::{abs_pos};

#[derive(Default, Debug)]
pub struct ScrollableScreen {
    pub scroll: ScrollXY,
}

pub fn scroll_xy() -> impl Strategy<Value = ScrollXY> {
    let spec = abs_pos();
    (spec, spec).prop_map(|(x, y)| slxy!{ x, y })
}

pub fn scrollable_screen() -> impl Strategy<Value = ScrollableScreen> {
    scroll_xy().prop_map(|scroll| ScrollableScreen {
        scroll
    })
}
}
