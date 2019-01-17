use super::*;

fn this_panic_safe_rope_slices_properly_on_line_14(rope: &mut Rope) {
    assert_eq!(&String::from(&*rope), include_str!("./pre-deleted.txt"));

    let line = dbg!(rope.line(LineIndex(14)).unwrap());

    let after_spaces = CharOffset(4);
    let slice_end = dbg!(line.len_chars());
    
    assert_eq!(
        line.slice(after_spaces..slice_end - 1).unwrap(),
        "precious"
    );
}

#[test]
fn panic_safe_rope_slices_properly_in_pre_deleted_txt() {
    let mut rope = Rope::from_str(include_str!("./pre-deleted.txt"));

    this_panic_safe_rope_slices_properly_on_line_14(&mut rope);
}

#[test]
fn panic_safe_rope_slices_properly_in_buggy_txt() {
    let mut rope = Rope::from_str(include_str!("./buggy.txt"));

    rope.remove(AbsoluteCharOffset(971)..AbsoluteCharOffset(973));

    this_panic_safe_rope_slices_properly_on_line_14(&mut rope);
}

fn this_ropey_rope_slices_properly_on_line_14(rope: &mut ropey::Rope) {
    assert_eq!(&String::from(&*rope), include_str!("./pre-deleted.txt"));

    let line = rope.line(14);

    let after_spaces = 4;
    let slice_end = dbg!(line.len_chars());
    
    assert_eq!(
        line.slice(after_spaces..slice_end - 1),
        "precious"
    );
}

#[test]
fn ropey_rope_slices_properly_in_pre_deleted_txt() {
    let mut rope = ropey::Rope::from_str(include_str!("./pre-deleted.txt"));

    this_ropey_rope_slices_properly_on_line_14(&mut rope);
}

#[test]
fn ropey_rope_slices_properly_in_buggy_txt() {
    let mut rope = ropey::Rope::from_str(include_str!("./buggy.txt"));

    rope.remove(971..973);

    this_ropey_rope_slices_properly_on_line_14(&mut rope);
}

#[test]
fn both_ropey_rope_and_panic_safe_rope_slices_properly_in_this_case_when_we_call_ropey_remove() {
    let line_index = 14;
    let after_spaces = 4;
    let slice_end = 13;
    {
        let mut rope = ropey::Rope::from_str(include_str!("./buggy.txt"));
    
        rope.remove(971..973);    
    
        let line = rope.line(line_index);
    
        assert_eq!(dbg!(line.len_chars()), slice_end);
    
        assert_eq!(
            line.slice(after_spaces..slice_end - 1),
            "precious"
        );
    }

    let mut rope = ropey::Rope::from_str(include_str!("./buggy.txt"));

    rope.remove(971..973);

    let safe_rope = Rope { rope };

    let safe_line = safe_rope.line(LineIndex(line_index)).unwrap();

    assert_eq!(safe_line.len_chars().0, slice_end);
    
    assert_eq!(
        safe_line.slice(CharOffset(after_spaces)..CharOffset(slice_end - 1)).unwrap(),
        "precious"
    );

}

#[test]
fn both_ropey_rope_and_panic_safe_rope_slices_properly_in_this_case_when_we_call_panic_safe_remove() {
    let line_index = 14;
    let after_spaces = 4;
    let slice_end = 13;
    {
        let mut safe_rope = Rope::from_str(include_str!("./buggy.txt"));

        safe_rope.remove(AbsoluteCharOffset(971)..AbsoluteCharOffset(973));
        let mut rope = safe_rope.rope;
    
        rope.remove(971..973);    
    
        let line = rope.line(line_index);
    
        assert_eq!(dbg!(line.len_chars()), slice_end);
    
        assert_eq!(
            line.slice(after_spaces..slice_end - 1),
            "precious"
        );
    }

    let mut safe_rope = Rope::from_str(include_str!("./buggy.txt"));

    safe_rope.remove(AbsoluteCharOffset(971)..AbsoluteCharOffset(973));

    let safe_line = safe_rope.line(LineIndex(line_index)).unwrap();

    assert_eq!(safe_line.len_chars().0, slice_end);
    
    assert_eq!(
        safe_line.slice(CharOffset(after_spaces)..CharOffset(slice_end - 1)).unwrap(),
        "precious"
    );

}

#[test]
fn panic_safe_rope_slices_properly_in_buggy_txt_all_in_one_test() {
    let mut rope = Rope::from_str(include_str!("./buggy.txt"));

    rope.remove(AbsoluteCharOffset(971)..AbsoluteCharOffset(973));

    assert_eq!(&String::from(&rope), include_str!("./pre-deleted.txt"));

    let line = dbg!(rope.line(LineIndex(14)).unwrap());

    let after_spaces = CharOffset(4);
    let slice_end = dbg!(line.len_chars());
    
    assert_eq!(
        line.slice(after_spaces..slice_end - 1).unwrap(),
        "precious"
    );
}

#[test]
fn reduced_panic_safe_rope_slices_properly_in_buggy_txt_all_in_one_test() {
    let mut rope = Rope::from_str(include_str!("./buggy.txt"));

    rope.remove(AbsoluteCharOffset(971)..AbsoluteCharOffset(973));

    assert_eq!(&String::from(&rope), include_str!("./pre-deleted.txt"));

    let line = dbg!(RopeSlice{ rope_slice: rope.rope.line(14) });

    use std::collections::Bound;

    let slice = dbg!(RopeSlice {
            rope_slice: line.rope_slice.slice((Bound::Included(4), Bound::Excluded(12))),
        });
    
    assert_eq!(slice.as_str(), line.rope_slice.as_str());

    // these tests seems to have been based on a misunderstanding of what `as_str` is supposed to do. 

    assert_eq!(
        slice,
        "precious"
    );
}
