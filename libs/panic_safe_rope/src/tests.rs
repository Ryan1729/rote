use super::*;

use proptest::proptest;

proptest!{
    #[test]
    fn empty_slice_works(s in ".*") {
        let r: Rope = s.into();

        let slice = r.empty_slice();
        assert_eq!(slice.len_chars().0, 0);
    }

    #[test]
    fn full_slice_works(s in ".*") {
        let r: Rope = s.into();

        let slice = r.full_slice();
        assert_eq!(slice.len_chars().0, r.len_chars().0);
    }
}

#[test]
fn slice_methods_do_not_panic_on_an_empty_slice() {
    let r: Rope = "".into();

    let slice = r.empty_slice();

    let _ = slice.len_bytes();
    let _ = slice.len_chars();
    let _ = slice.len_lines();
    let _ = slice.byte_to_char(ByteIndex(0));
    let _ = slice.byte_to_line(ByteIndex(0));
    let _ = slice.char_to_byte(CharOffset(0));
    let _ = slice.char_to_line(CharOffset(0));
    let _ = slice.line_to_byte(LineIndex(0));
    let _ = slice.line_to_char(LineIndex(0));
    let _ = slice.byte(ByteIndex(0));
    let _ = slice.char(CharOffset(0));
    let _ = slice.chunk_at_byte(ByteIndex(0));
    let _ = slice.chunk_at_char(CharOffset(0));
    let _ = slice.chunk_at_line_break(0);
    let _ = slice.line(LineIndex(0));
    let _ = slice.slice(CharOffset(1)..CharOffset(2));
    let _ = slice.empty_slice();
    let _ = slice.full_slice();
    let _ = slice.bytes();
    let _ = slice.bytes_at(ByteIndex(0));
    let _ = slice.chars();
    let _ = slice.chars_at(CharOffset(0));
    let _ = slice.chars_at_end();
    let _ = slice.lines();
    let _ = slice.lines_at(0);
    let _ = slice.chunks();
    let _ = slice.as_str_if_no_allocation_needed();
    let _ = slice.as_cow_str();
}

mod included_files;