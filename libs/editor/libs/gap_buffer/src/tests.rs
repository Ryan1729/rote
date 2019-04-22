use super::*;

macro_rules! p {
    ($buffer:ident) => {
        println!("{:?}", $buffer);
        println!("{:?}", $buffer.chars().collect::<String>())
    };
}

macro_rules! pos {
    (l $line:literal o $offset:literal) => {
        Position {
            line: $line,
            offset: CharOffset($offset),
        }
    };
    () => {
        Position::default()
    };
}

macro_rules! cached_offset {
    (l $line:literal o $offset:literal i $index:literal) => {
        CachedOffset {
            position: Position {
                line: $line,
                offset: CharOffset($offset),
            },
            index: ByteIndex($index),
        }
    };
    (p: $position:expr, i $index:literal) => {
        CachedOffset {
            position: $position,
            index: ByteIndex($index),
        }
    };
    (i $index:literal) => {
        CachedOffset {
            position: pos! {},
            index: ByteIndex($index),
        }
    };
    () => {
        CachedOffset::default()
    };
}

macro_rules! init {
    ($str:literal) => {{
        let mut buffer = GapBuffer::new($str.to_string());

        // make sure the gap is not empty since things could work
        // accidentally in the tests otherwise
        let s = "␠␠␠␠";
        buffer.insert_str(s, pos! {});
        buffer.delete_range(
            pos! {}..=Position {
                offset: CharOffset(s.chars().count()),
                ..d!()
            },
        );
        // moving the gap to the end allows tests to continue to use byte indexes
        // without knowing the size of the gap
        buffer.move_gap(ByteIndex(buffer.data.len()));

        buffer
    }};
    ($str:literal gap $index:expr) => {{
        let mut buffer = init!($str);
        buffer.move_gap(ByteIndex($index));
        buffer
    }};
}

#[test]
fn init_makes_a_gap_without_affecting_the_string() {
    //let buffer = init!("the same literal\nas the other one" gap 6);
    let buffer = init!("the same literal\nas the other one");

    assert_eq!(
        buffer.chars().collect::<String>(),
        "the same literal\nas the other one".to_owned()
    );
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
    let buffer = init!("1234567890" gap 0);
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("1234567890")]
    );
}
#[test]
fn lines_works_with_single_line_with_gap_in_middle() {
    let buffer = init!("1234567890" gap 5);
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Gapped("12345", "67890")]
    );
}
#[test]
fn lines_works_with_single_line_with_gap_at_end() {
    let buffer = init!("1234567890" gap 10);
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("1234567890")]
    );
}

#[test]
fn lines_works_with_two_lines_with_gap_at_start_of_first() {
    let buffer = init!("12345\n67890" gap 0);
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
    );
}
#[test]
fn lines_works_with_two_lines_with_gap_in_midde_of_first() {
    let buffer = init!("12345\n67890" gap 2);
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Gapped("12", "345"), GapLine::Connected("67890")]
    );
}
#[test]
fn lines_works_with_two_lines_with_gap_at_end_of_first() {
    let buffer = init!("12345\n67890" gap 5);
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
    );
}

#[test]
fn lines_works_with_two_lines_with_gap_at_start_of_second() {
    let buffer = init!("12345\n67890" gap 6);
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
    );
}
#[test]
fn lines_works_with_two_lines_with_gap_in_midde_of_second() {
    let buffer = init!("12345\n67890" gap 8);
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("12345"), GapLine::Gapped("67", "890")]
    );
}
#[test]
fn lines_works_with_two_lines_with_gap_at_end_of_second() {
    let buffer = init!("12345\n67890" gap 11);
    assert_eq!(
        buffer.lines().collect::<Vec<_>>(),
        vec![GapLine::Connected("12345"), GapLine::Connected("67890")]
    );
}

#[test]
fn lines_works_with_three_lines_with_gap_at_start_of_first() {
    let buffer = init!("1234\n567\n890" gap 0);
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
    let buffer = init!("1234\n567\n890" gap 2);
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
    let buffer = init!("1234\n567\n890" gap 4);
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
    let buffer = init!("1234\n567\n890" gap 5);
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
    let buffer = init!("1234\n567\n890" gap 6);
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
    let buffer = init!("1234\n567\n890" gap 8);
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
    let buffer = init!("1234\n567\n890" gap 9);
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
    let buffer = init!("1234\n567\n890" gap 10);
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
    let buffer = init!("1234\n567\n890" gap 12);
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
    let buffer = init!("1234567890" gap 0);
    assert_eq!(buffer.nth_line_count(0), Some(10));
}
#[test]
fn nth_line_count_works_with_single_line_with_gap_in_middle() {
    let buffer = init!("1234567890" gap 5);
    assert_eq!(buffer.nth_line_count(0), Some(10));
}
#[test]
fn nth_line_count_works_with_single_line_with_gap_at_end() {
    let buffer = init!("1234567890" gap 10);
    assert_eq!(buffer.nth_line_count(0), Some(10));
}

#[test]
fn nth_line_count_works_with_two_lines_with_gap_at_start_of_first() {
    let buffer = init!("12345\n67890" gap 0);
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}
#[test]
fn nth_line_count_works_with_two_lines_with_gap_in_midde_of_first() {
    let buffer = init!("12345\n67890" gap 2);
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}
#[test]
fn nth_line_count_works_with_two_lines_with_gap_at_end_of_first() {
    let buffer = init!("12345\n67890" gap 5);
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}

#[test]
fn nth_line_count_works_with_two_lines_with_gap_at_start_of_second() {
    let buffer = init!("12345\n67890" gap 6);
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}
#[test]
fn nth_line_count_works_with_two_lines_with_gap_in_midde_of_second() {
    let buffer = init!("12345\n67890" gap 8);
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}
#[test]
fn nth_line_count_works_with_two_lines_with_gap_at_end_of_second() {
    let buffer = init!("12345\n67890" gap 11);
    assert_eq!(buffer.nth_line_count(0), Some(5));
    assert_eq!(buffer.nth_line_count(1), Some(5));
}

#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_start_of_first() {
    let buffer = init!("1234\n567\n890" gap 0);
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_in_midde_of_first() {
    let buffer = init!("1234\n567\n890" gap 2);
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_end_of_first() {
    let buffer = init!("1234\n567\n890" gap 4);
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}

#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_start_of_second() {
    let buffer = init!("1234\n567\n890" gap 5);
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_in_midde_of_second() {
    let buffer = init!("1234\n567\n890" gap 6);
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_end_of_second() {
    let buffer = init!("1234\n567\n890" gap 8);
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}

#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_start_of_third() {
    let buffer = init!("1234\n567\n890" gap 9);
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_in_midde_of_third() {
    let buffer = init!("1234\n567\n890" gap 10);
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}
#[test]
fn nth_line_count_works_with_three_lines_with_gap_at_end_of_third() {
    let buffer = init!("1234\n567\n890" gap 12);
    assert_eq!(buffer.nth_line_count(0), Some(4));
    assert_eq!(buffer.nth_line_count(1), Some(3));
    assert_eq!(buffer.nth_line_count(2), Some(3));
}

use proptest::strategy::Strategy;
use proptest::{prop_oneof, proptest};

// Regex is "Zero or more Non-control characters, plus '\r' and '\n', and minus space.
const TYPEABLE: &str = r"[\PC\r\n&&[^ ]]*";

/*
// re run these if the TYPEABLE regex ever changes.

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
*/

fn any_position() -> impl proptest::strategy::Strategy<Value = Position> {
    use proptest::arbitrary::any;
    (any::<usize>(), any::<usize>()).prop_map(|(line, o)| Position {
        line,
        offset: CharOffset(o),
    })
}

#[derive(Debug)]
enum Operation {
    Insert(String, Position),
    Delete(std::ops::RangeInclusive<Position>),
    MoveGap(Position),
}

fn any_operation() -> impl Strategy<Value = Operation> {
    prop_oneof![
        any_position().prop_map(Operation::MoveGap),
        (any_position(), any_position()).prop_map(|(p1, p2)| {
            if p1 <= p2 {
                Operation::Delete(p1..=p2)
            } else {
                Operation::Delete(p2..=p1)
            }
        }),
        (TYPEABLE, any_position()).prop_map(|(s, p)| Operation::Insert(s, p)),
    ]
}

proptest! {
    #[test]
    fn buffers_start_with_correct_caches(s in TYPEABLE) {
        let buffer = GapBuffer::new(s);
        assert_eq!(buffer.offset_cache, buffer.optimal_offset_cache());
    }

    #[test]
    fn any_single_operation_preserves_offset_cache_correctness(operation in any_operation(), s in TYPEABLE) {
        let mut buffer = GapBuffer::new(s);
        match operation {
            Operation::Insert(s, p) => { buffer.insert_str(&s, p);} ,
            Operation::Delete(r) => buffer.delete_range(r),
            Operation::MoveGap(p) => {
                if let Some(i) = buffer.find_index(p) { buffer.move_gap(i); }
            },
        }
        assert_eq!(buffer.offset_cache, buffer.optimal_offset_cache());
    }
}

mod find_index_unbounded_to_unbounded {
    use super::*;
    #[test]
    fn at_start() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {}, ..),
            Some(ByteIndex(0))
        );
    }
    #[test]
    fn before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 0 o 2}, ..),
            Some(ByteIndex(2))
        );
    }
    #[test]
    fn at_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 1 o 1}, ..),
            Some(ByteIndex(6))
        );
    }
    #[test]
    fn after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, ..),
            Some(ByteIndex(10 + buffer.gap_length.0))
        );
    }
    #[test]
    fn at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, ..),
            Some(ByteIndex(12 + buffer.gap_length.0))
        );
    }
}

mod find_index_included_to_unbounded {
    use super::*;
    #[test]
    fn at_start() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {}, cached_offset! {i 0}..),
            Some(ByteIndex(0))
        );
    }
    #[test]
    fn before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 0 o 2};
        assert_eq!(
            buffer.find_index_within_range(p, cached_offset! {p: p, i 2}..),
            Some(ByteIndex(2))
        );
    }
    #[test]
    fn not_found_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 0 o 2};
        assert_eq!(
            buffer.find_index_within_range(p, cached_offset! {p: p, i 2}..),
            None
        );
    }
    #[test]
    fn at_gap_left_edge_before() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(p, cached_offset! {l 0 o 4 i 4}..),
            Some(ByteIndex(6))
        );
    }
    #[test]
    fn at_gap_left_edge_at() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(p, cached_offset! {p: p, i 6}..),
            Some(ByteIndex(6))
        );
    }
    #[test]
    fn not_found_at_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 1 o 1}, cached_offset! {l 1 o 2 i 7}..),
            None
        );
    }
    #[test]
    fn after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, cached_offset! {l 1 o 2 i 7}..),
            Some(ByteIndex(10 + buffer.gap_length.0))
        );
    }
    #[test]
    fn not_found_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, cached_offset! {l 1 o 2 i 7}..),
            None
        );
    }
    #[test]
    fn at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, cached_offset! {l 1 o 2 i 7}..),
            Some(ByteIndex(12 + buffer.gap_length.0))
        );
    }
    #[test]
    fn not_found_at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 2 o 3};
        assert_eq!(
            buffer.find_index_within_range(p, cached_offset! {p: p, i 13}..),
            None
        );
    }
}

// Excluded at the beginning currently requires sonmeone to implement the RangeBounds trait
// on a new data structure, eithout using the `..`syntax. This seems unlikely enough that we
// don't bother testing it.
