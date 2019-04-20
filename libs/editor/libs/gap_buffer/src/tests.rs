use super::*;

macro_rules! init {
    ($str:literal) => {
        GapBuffer::new($str.to_string())
    };
}

macro_rules! p {
    ($buffer:ident) => {
        println!("{:?}", $buffer);
        println!("{:?}", $buffer.chars().collect::<String>())
    };
}

#[test]
fn newline_bug_is_squished() {
    // "fix bug where if you add a newline in the middle of a line then move down and then to
    // the start of the line, then press delete twice, everything after the place where you
    // pressed enter is deleted"

    let mut buffer = init!("1234567890");

    p!(buffer);

    buffer.insert(
        '\n',
        Position {
            offset: CharOffset(5),
            ..d!()
        },
    );

    p!(buffer);

    // should delete inserted newline
    buffer.delete(Position {
        offset: CharOffset(0),
        line: 1,
    });

    p!(buffer);

    // should delete '0'
    buffer.delete(Position {
        offset: CharOffset(10),
        line: 0,
    });

    p!(buffer);

    assert_eq!(buffer.chars().collect::<String>(), "123456789");
}

#[test]
fn deleting_past_the_end_does_nothing() {
    let mut buffer = init!("1234567890");

    buffer.delete(Position {
        offset: CharOffset(0),
        line: 1,
    });

    assert_eq!(buffer.chars().collect::<String>(), "1234567890");
}

#[test]
fn backward_works_on_a_left_edge() {
    let buffer = init!("1234\n567\r\n890");

    assert_eq!(
        backward(
            &buffer,
            Position {
                offset: CharOffset(0),
                line: 1,
            },
        ),
        Position {
            offset: CharOffset(4),
            line: 0,
        }
    );

    assert_eq!(
        backward(
            &buffer,
            Position {
                offset: CharOffset(0),
                line: 2,
            },
        ),
        Position {
            offset: CharOffset(3),
            line: 1,
        }
    );
}

#[test]
fn backward_works_with_an_invalid_postion() {
    let mut buffer = init!("123467890");

    buffer.insert(
        '5',
        Position {
            offset: CharOffset(4),
            ..d!()
        },
    );

    assert_eq!(
        backward(
            &buffer,
            Position {
                offset: CharOffset(0),
                line: 1,
            },
        ),
        Position {
            offset: CharOffset(10),
            line: 0,
        }
    );
}

//
//    LINES
//

#[test]
fn lines_works_with_single_line_with_gap_at_start() {
    let mut buffer = init!("1234567890");
    buffer.move_gap(ByteIndex(0));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("1234567890")]
    );
}
#[test]
fn lines_works_with_single_line_with_gap_in_middle() {
    let mut buffer = init!("1234567890");
    buffer.move_gap(ByteIndex(5));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Gapped("12345", "67890")]
    );
}
#[test]
fn lines_works_with_single_line_with_gap_at_end() {
    let mut buffer = init!("1234567890");
    buffer.move_gap(ByteIndex(10));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("1234567890")]
    );
}

#[test]
fn lines_works_with_two_lines_with_gap_at_start_of_first() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(0));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
    );
}
#[test]
fn lines_works_with_two_lines_with_gap_in_midde_of_first() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(2));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Gapped("12", "345"), GapLine::Connected("67890")]
    );
}
#[test]
fn lines_works_with_two_lines_with_gap_at_end_of_first() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(5));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
    );
}

#[test]
fn lines_works_with_two_lines_with_gap_at_start_of_second() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(6));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
    );
}
#[test]
fn lines_works_with_two_lines_with_gap_in_midde_of_second() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(8));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("12345"), GapLine::Gapped("67", "890")]
    );
}
#[test]
fn lines_works_with_two_lines_with_gap_at_end_of_second() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(11));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
    );
}

#[test]
fn lines_works_with_three_lines_with_gap_at_start_of_first() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(0));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![
            GapLine::Connected("1234"),
            GapLine::Connected("567"),
            GapLine::Connected("890")
        ]
    );
}
#[test]
fn lines_works_with_three_lines_with_gap_in_midde_of_first() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(2));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![
            GapLine::Gapped("12", "34"),
            GapLine::Connected("567"),
            GapLine::Connected("890")
        ]
    );
}
#[test]
fn lines_works_with_three_lines_with_gap_at_end_of_first() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(4));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![
            GapLine::Connected("1234"),
            GapLine::Connected("567"),
            GapLine::Connected("890")
        ]
    );
}

#[test]
fn lines_works_with_three_lines_with_gap_at_start_of_second() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(5));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![
            GapLine::Connected("1234"),
            GapLine::Connected("567"),
            GapLine::Connected("890")
        ]
    );
}
#[test]
fn lines_works_with_three_lines_with_gap_in_midde_of_second() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(6));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![
            GapLine::Connected("1234"),
            GapLine::Gapped("5", "67"),
            GapLine::Connected("890")
        ]
    );
}
#[test]
fn lines_works_with_three_lines_with_gap_at_end_of_second() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(8));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![
            GapLine::Connected("1234"),
            GapLine::Connected("567"),
            GapLine::Connected("890")
        ]
    );
}

#[test]
fn lines_works_with_three_lines_with_gap_at_start_of_third() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(9));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![
            GapLine::Connected("1234"),
            GapLine::Connected("567"),
            GapLine::Connected("890")
        ]
    );
}
#[test]
fn lines_works_with_three_lines_with_gap_in_midde_of_third() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(10));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![
            GapLine::Connected("1234"),
            GapLine::Connected("567"),
            GapLine::Gapped("8", "90")
        ]
    );
}
#[test]
fn lines_works_with_three_lines_with_gap_at_end_of_third() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(12));
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![
            GapLine::Connected("1234"),
            GapLine::Connected("567"),
            GapLine::Connected("890")
        ]
    );
}

//
//    NTH LINE COUNT
//

#[test]
fn nth_line_count_works_with_single_line_with_gap_at_start() {
    let mut buffer = init!("1234567890");
    buffer.move_gap(ByteIndex(0));
    assert_eq!(buffer.nth_line_count(0), Some(10));
}
#[test]
fn nth_line_count_works_with_single_line_with_gap_in_middle() {
    let mut buffer = init!("1234567890");
    buffer.move_gap(ByteIndex(5));
    assert_eq!(buffer.nth_line_count(0), Some(10));
}
#[test]
fn nth_line_count_works_with_single_line_with_gap_at_end() {
    let mut buffer = init!("1234567890");
    buffer.move_gap(ByteIndex(10));
    assert_eq!(buffer.nth_line_count(0), Some(10));
}

#[test]
fn nth_line_count_works_with_two_lines_with_gap_at_start_of_first() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(0));
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}
#[test]
fn nth_line_count_works_with_two_lines_with_gap_in_midde_of_first() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(2));
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}
#[test]
fn nth_line_count_works_with_two_lines_with_gap_at_end_of_first() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(5));
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}

#[test]
fn nth_line_count_works_with_two_lines_with_gap_at_start_of_second() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(6));
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}
#[test]
fn nth_line_count_works_with_two_lines_with_gap_in_midde_of_second() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(8));
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}
#[test]
fn nth_line_count_works_with_two_lines_with_gap_at_end_of_second() {
    let mut buffer = init!("12345\n67890");
    buffer.move_gap(ByteIndex(11));
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}

#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_start_of_first() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(0));
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_in_midde_of_first() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(2));
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_end_of_first() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(4));
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}

#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_start_of_second() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(5));
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_in_midde_of_second() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(6));
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_end_of_second() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(8));
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}

#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_start_of_third() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(9));
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_in_midde_of_third() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(10));
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_end_of_third() {
    let mut buffer = init!("1234\n567\n890");
    buffer.move_gap(ByteIndex(12));
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}

enum Operation {
    Insert(String, Position),
    Delete(std::ops::RangeInclusive<Position>),
    MoveGap(Position),
}

use proptest::proptest;

// Regex is "Zero or more Non-control characters, plus '\r' and '\n', and minus space.
const TYPEABLE: &str = r"[\PC\r\n&&[^ ]]*";

fn first_char_utf8_len<S: AsRef<str>>(s: S) -> usize {
    s.as_ref()
        .chars()
        .next()
        .map(|c| c.len_utf8())
        .unwrap_or_default()
}

proptest! {
    #[test]
    #[should_panic(expected = "assertion failed")]
    fn strings_gen_nl(s in TYPEABLE) {
        assert!(!s.contains('\n'));
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn strings_gen_cr(s in TYPEABLE) {
        assert!(!s.contains('\r'));
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn strings_gen_one_byte_char(s in TYPEABLE) {
        assert!(!(first_char_utf8_len(s) == 1));
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn strings_gen_two_byte_chars(s in TYPEABLE) {
        assert!(!(first_char_utf8_len(s) == 2));
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn strings_gen_three_byte_chars(s in TYPEABLE) {
        assert!(!(first_char_utf8_len(s) == 3));
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn strings_gen_four_byte_chars(s in TYPEABLE) {
        assert!(!(first_char_utf8_len(s) == 4));
    }
}

proptest! {
    // #[test]

    // fn any_single_operation_preserves_offset_cache_correctness(operation in any_operation(), s in r"[\PC\r\n&&[^ ]]*") {
    //     let mut buffer = init!(s);
    //     match operation {
    //         Insert(s, p) => buffer.insert_str(&s, p),
    //         Delete(r) => buffer.delete_range(r),
    //         MoveGap(p) => buffer.move_gap(p),
    //     }
    //     assert_eq!(buffer.offset_cache, buffer.optimal_offset_cache());
    // }
}
