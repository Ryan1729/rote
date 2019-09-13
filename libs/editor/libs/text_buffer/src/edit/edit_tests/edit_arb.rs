use super::*;

prop_compose! {
    fn arb_absolute_char_offset_range(max_len: usize)
    (o1 in arb_absolute_char_offset(max_len), o2 in arb_absolute_char_offset(max_len)) -> AbsoluteCharOffsetRange {
        AbsoluteCharOffsetRange::new(o1, o2)
    }
}

prop_compose! {
    fn range_edit(max_len: usize)
    (chars in ".*", range in arb_absolute_char_offset_range(max_len)) -> RangeEdit {
        RangeEdit {
            chars,
            range
        }
    }
}

prop_compose! {
    fn range_edits(max_len: usize)
    (insert_range in option::of(range_edit(max_len)), delete_range in option::of(range_edit(max_len))) -> RangeEdits {
        RangeEdits {
            insert_range,
            delete_range,
        }
    }
}

prop_compose! {
    pub(crate) fn edit()
    (len in 1..SOME_AMOUNT)
    (range_edits in vec1(range_edits(len), len), cursors in arb_change!(arb::cursors(len))) -> Edit {
        Edit {
            range_edits,
            cursors,
        }
    }
}

// Because I couldn't figure out the types for this. And it looks like `proptest` ends up making
// custom structs for each instance of things like this.
#[macro_export]
macro_rules! arb_change {
    ($strat: expr) => {
        ($strat, $strat).prop_map(|(old, new)| Change { old, new })
    };
}
