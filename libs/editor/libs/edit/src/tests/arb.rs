use proptest::{
    option,
    prelude::*,
};

use rope_pos::AbsoluteCharOffsetRange;

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

pub fn edit<'rope, R: 'rope + Borrow<Rope>>(rope: R) -> impl Strategy<Value = Edit> + 'rope {
    let rope = rope.borrow();
    let rope = rope.clone();

    (1..SOME_AMOUNT).prop_flat_map(move |len| {
        let cursor_strat = arb::valid_cursors_for_rope(rope.clone(), len);
        let cursor_strat2 = arb::valid_cursors_for_rope(rope.clone(), len);

        (
            vec1(range_edits(len), len),
            (cursor_strat, cursor_strat2).prop_map(|(old, new)| Change { old, new }),
        )
            .prop_map(|(range_edits, cursors)| Edit {
                range_edits,
                cursors,
            })
    })
}

pub fn edit_with_cursors<'rope, R: 'rope + Borrow<Rope>>(rope: R, cursors: Cursors) -> impl Strategy<Value = Edit> + 'rope {
    edit(rope).prop_map(move |mut edit| {
        edit.cursors.old = cursors;
        edit
    })
}
