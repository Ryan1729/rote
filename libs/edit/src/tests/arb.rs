use crate::{Change, Edit, CursoredRope};
use proptest::{option, prop_compose, Strategy};
use cursors::Cursors;
use vec1::{Vec1};
use pub_arb_cursors::{valid_cursors_for_rope};
use pub_arb_vec1::{vec1};
use panic_safe_rope::{Rope, AbsoluteCharOffset};
use crate::{RangeEdit, RangeEdits};
use rope_pos::AbsoluteCharOffsetRange;
use core::borrow::Borrow;

const SOME_AMOUNT: usize = 16;

prop_compose! {
    pub fn absolute_char_offset(max_len: usize)(offset in 0..=max_len) -> AbsoluteCharOffset {
        AbsoluteCharOffset(offset)
    }
}

prop_compose! {
    fn absolute_char_offset_range(max_len: usize)
    (o1 in absolute_char_offset(max_len), o2 in absolute_char_offset(max_len)) -> AbsoluteCharOffsetRange {
        AbsoluteCharOffsetRange::new(o1, o2)
    }
}

prop_compose! {
    fn range_edit(max_len: usize)
    (chars in ".*", range in absolute_char_offset_range(max_len)) -> RangeEdit {
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
        let cursor_strat = valid_cursors_for_rope(rope.clone(), len);
        let cursor_strat2 = valid_cursors_for_rope(rope.clone(), len);

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

pub fn edit_with_cursors(cursored_rope: CursoredRope) -> impl Strategy<Value = Edit>{
    let (rope, cursors) = cursored_rope.split();
    edit(rope).prop_map(move |mut edit| {
        edit.cursors.old = cursors.clone();
        edit
    })
}

pub fn edit_from_pieces(
    range_edits: Vec1<RangeEdits>,
    cursors: Change<Cursors>,
) -> Edit {
    Edit {
        range_edits,
        cursors,
    }
}

pub fn edit_range_edits_mut(edit: &mut Edit) -> &mut Vec1<RangeEdits> {
    &mut edit.range_edits
}