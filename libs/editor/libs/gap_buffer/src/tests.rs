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

macro_rules! gap_informed {
    ($index:literal, $buffer:ident) => {
        inform_of_gap(GapObliviousByteIndex($index), &$buffer)
    };
}

macro_rules! cached_offset {
    (l $line:literal o $offset:literal i $index:literal) => {
        CachedOffset {
            position: Position {
                line: $line,
                offset: CharOffset($offset),
            },
            index: GapObliviousByteIndex($index),
        }
    };
    (p: $position:expr, i $index:literal) => {
        CachedOffset {
            position: $position,
            index: GapObliviousByteIndex($index),
        }
    };
    () => {
        CachedOffset::default()
    };
}

const TEST_BLOCK_SIZE: NonZeroUsize = unsafe { NonZeroUsize::new_unchecked(8) };

macro_rules! init {
    ($str:literal) => {{
        //make the blocksize smaller to make edge cases more frequent
        let mut buffer = GapBuffer::new_with_block_size($str.to_string(), TEST_BLOCK_SIZE);

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

// This has failed before due to an incorrect offset cache.
// If the offset cache tests are failing try fixing them first
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
        (any_position(), any_position()).prop_map(|(p1, p2)| if p1 <= p2 {
            Operation::Delete(p1..=p2)
        } else {
            Operation::Delete(p2..=p1)
        }),
        (TYPEABLE, any_position()).prop_map(|(s, p)| Operation::Insert(s, p)),
    ]
}

proptest! {
    #[test]
    fn buffers_start_with_correct_caches(s in TYPEABLE) {
        let buffer = GapBuffer::new_with_block_size(s, TEST_BLOCK_SIZE);
        assert_eq!(buffer.offset_cache, buffer.optimal_offset_cache());
    }

    #[test]
    fn any_single_operation_preserves_offset_cache_correctness(operation in any_operation(), s in TYPEABLE) {
        let mut buffer = GapBuffer::new_with_block_size(s, TEST_BLOCK_SIZE);
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

#[test]
fn insert_with_newline_preserves_offset_cache_correctness() {
    let mut buffer = GapBuffer::new_with_block_size("".to_owned(), TEST_BLOCK_SIZE);

    buffer.insert_str("a\n", Position::default());

    assert_eq!(buffer.offset_cache, buffer.optimal_offset_cache());
}

#[test]
fn insert_with_newline_on_non_empty_string_preserves_offset_cache_correctness() {
    let mut buffer = GapBuffer::new_with_block_size("0".to_owned(), TEST_BLOCK_SIZE);

    buffer.insert_str("a\n", Position::default());

    assert_eq!(buffer.offset_cache, buffer.optimal_offset_cache());
}

mod find_index_unbounded_to_unbounded {
    use super::*;
    #[test]
    fn at_start() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {}, ..),
            Some(gap_informed!(0, buffer))
        );
    }
    #[test]
    fn before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 0 o 2}, ..),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn at_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 1 o 1}, ..),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, ..),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, ..),
            Some(gap_informed!(12, buffer))
        );
    }
}

mod find_index_included_to_unbounded {
    use super::*;
    #[test]
    fn at_start() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {}, cached_offset! {}..),
            Some(gap_informed!(0, buffer))
        );
    }
    #[test]
    fn not_found_at_start() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {}, cached_offset! {l 0 o 1 i 1}..),
            None
        );
    }
    #[test]
    fn before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 0 o 2};
        assert_eq!(
            buffer.find_index_within_range(p, cached_offset! {p: p, i 2}..),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn not_found_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 0 o 2};
        assert_eq!(
            buffer.find_index_within_range(p, cached_offset! {l 1 o 2 i 7}..),
            None
        );
    }
    #[test]
    fn at_gap_left_edge_before() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(p, cached_offset! {l 0 o 4 i 4}..),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn at_gap_left_edge_at() {
        let buffer = dbg!(init!("1234\n567\n890" gap 6));

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(p, cached_offset! {p: p, i 6}..),
            Some(gap_informed!(6, buffer))
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
    fn immeadiately_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, cached_offset! {l 1 o 2 i 7}..),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn not_found_immeadiately_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 1 o 2}, cached_offset! {l 2 o 2 i 11}..),
            None
        );
    }
    #[test]
    fn after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, cached_offset! {l 2 o 0 i 9}..),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn not_found_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, cached_offset! {l 2 o 2 i 11}..),
            None
        );
    }
    #[test]
    fn at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, cached_offset! {l 1 o 2 i 7}..),
            Some(gap_informed!(12, buffer))
        );
    }
    #[test]
    fn not_found_at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 2 o 3};
        assert_eq!(
            buffer.find_index_within_range(p, cached_offset! {l 4 o 0 i 13}..),
            None
        );
    }
}

// Excluded at the beginning currently requires sonmeone to implement the RangeBounds trait
// on a new data structure, eithout using the `..`syntax. This seems unlikely enough that we
// don't bother testing it.

mod find_index_unbounded_to_included {
    use super::*;
    #[test]
    fn at_start_to_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {}, ..=cached_offset! {l 0 o 4 i 4}),
            Some(gap_informed!(0, buffer))
        );
    }
    #[test]
    fn at_start_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {}, ..=cached_offset! {l 1 o 2 i 7}),
            Some(gap_informed!(0, buffer))
        );
    }
    #[test]
    fn before_gap_to_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 0 o 2}, ..=cached_offset! {l 0 o 4 i 4}),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn before_gap_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 0 o 2}, ..=cached_offset! {l 1 o 2 i 7}),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn not_found_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 0 o 2}, ..=cached_offset! {}),
            None
        );
    }
    #[test]
    fn at_gap_to_at_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(p, ..=cached_offset! {p: p, i 6}),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn at_gap_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 1 o 1}, ..=cached_offset! {l 1 o 2 i 7}),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn not_found_at_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 1 o 1}, ..=cached_offset! {l 0 o 1 i 1}),
            None
        );
    }
    #[test]
    fn after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, ..=cached_offset! {l 2 o 2 i 11}),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn not_found_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, ..=cached_offset! {l 1 o 2 i 7}),
            None
        );
    }
    #[test]
    fn at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, ..=cached_offset! {l 4 o 0 i 13}),
            Some(gap_informed!(12, buffer))
        );
    }
    #[test]
    fn not_found_at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, ..=cached_offset! {l 1 o 2 i 7}),
            None
        );
    }
    #[test]
    fn at_end_when_gap_is_at_end() {
        let buffer = init!("1234\n567\n890" gap 12);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, ..=cached_offset! {l 4 o 0 i 13}),
            Some(gap_informed!(12, buffer))
        );
    }
    #[test]
    fn not_found_at_end_when_gap_is_at_end() {
        let buffer = init!("1234\n567\n890" gap 12);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, ..=cached_offset! {l 2 o 2 i 11}),
            None
        );
    }
}

mod find_index_included_to_included {
    use super::*;
    #[test]
    fn not_found_at_start() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 0 o 2}, cached_offset! {}..=cached_offset! {}),
            None
        );
    }
    #[test]
    fn at_start_to_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer
                .find_index_within_range(pos! {}, cached_offset! {}..=cached_offset! {l 0 o 4 i 4}),
            Some(gap_informed!(0, buffer))
        );
    }
    #[test]
    fn not_found_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {},
                cached_offset! {l 0 o 1 i 1}..=cached_offset! {l 1 o 4 i 4}
            ),
            None
        );
    }
    #[test]
    fn at_start_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer
                .find_index_within_range(pos! {}, cached_offset! {}..=cached_offset! {l 1 o 2 i 7}),
            Some(gap_informed!(0, buffer))
        );
    }
    #[test]
    fn not_found_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {},
                cached_offset! {l 0 o 1 i 1}..=cached_offset! {l 1 o 2 i 7}
            ),
            None
        );
    }
    #[test]
    fn before_gap_to_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 0 o 2},
                cached_offset! {l 0 o 1 i 1}..=cached_offset! {l 0 o 4 i 4}
            ),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn before_gap_to_immeadiately_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 0 o 2},
                cached_offset! {l 0 o 1 i 1}..=cached_offset! {l 1 o 2 i 7}
            ),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn before_gap_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 0 o 2},
                cached_offset! {l 0 o 1 i 1}..=cached_offset! {l 2 o 0 i 9}
            ),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn at_gap_to_at_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(
                p,
                cached_offset! {p: p, i 6}..=cached_offset! {p: p, i 6}
            ),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn at_gap_to_immeadiately_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(
                p,
                cached_offset! {p: p, i 6}..=cached_offset! {l 1 o 2 i 7}
            ),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn at_gap_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(
                p,
                cached_offset! {p: p, i 6}..=cached_offset! {l 2 o 0 i 9}
            ),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn not_found_at_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 1 o 1},
                cached_offset! {}..=cached_offset! {l 0 o 1 i 1}
            ),
            None
        );
    }
    #[test]
    fn after_gap_from_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 1},
                cached_offset! {l 0 o 4 i 4}..=cached_offset! {l 2 o 2 i 11}
            ),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn after_gap_from_at_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 1},
                cached_offset! {l 1 o 1 i 6}..=cached_offset! {l 2 o 2 i 11}
            ),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn after_gap_from_immeadiately_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 1},
                cached_offset! {l 1 o 2 i 7}..=cached_offset! {l 2 o 2 i 11}
            ),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn not_found_at_gap_to_immeadiately_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 1},
                cached_offset! {l 1 o 2 i 6}..=cached_offset! {l 1 o 2 i 7}
            ),
            None
        );
    }
    #[test]
    fn not_found_at_gap_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 1},
                cached_offset! {l 1 o 2 i 6}..=cached_offset! {l 2 o 0 i 9}
            ),
            None
        );
    }
    #[test]
    fn after_gap_from_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 1},
                cached_offset! {l 2 o 0 i 9}..=cached_offset! {l 2 o 2 i 11}
            ),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn not_found_after_gap_from_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 0},
                cached_offset! {l 2 o 1 i 10}..=cached_offset! {l 2 o 2 i 11}
            ),
            None
        );
    }
    #[test]
    fn at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 3},
                cached_offset! {}..=cached_offset! {l 4 o 0 i 13}
            ),
            Some(gap_informed!(12, buffer))
        );
    }
    #[test]
    fn not_found_at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 3},
                cached_offset! {}..=cached_offset! {l 1 o 2 i 7}
            ),
            None
        );
    }
    #[test]
    fn at_end_when_gap_is_at_end() {
        let buffer = init!("1234\n567\n890" gap 12);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 3},
                cached_offset! {}..=cached_offset! {l 4 o 0 i 13}
            ),
            Some(gap_informed!(12, buffer))
        );
    }
    #[test]
    fn not_found_at_end_when_gap_is_at_end() {
        let buffer = init!("1234\n567\n890" gap 12);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 3},
                cached_offset! {}..=cached_offset! {l 2 o 2 i 11}
            ),
            None
        );
    }
}

mod find_index_unbounded_to_excluded {
    use super::*;
    #[test]
    fn at_start_to_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {}, ..cached_offset! {l 0 o 4 i 4}),
            Some(gap_informed!(0, buffer))
        );
    }
    #[test]
    fn at_start_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {}, ..cached_offset! {l 1 o 3 i 8}),
            Some(gap_informed!(0, buffer))
        );
    }
    #[test]
    fn before_gap_to_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 0 o 2}, ..cached_offset! {l 0 o 4 i 4}),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn before_gap_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 0 o 2}, ..cached_offset! {l 1 o 3 i 8}),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn not_found_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 0 o 2}, ..cached_offset! {l 0 o 2 i 2}),
            None
        );
    }
    #[test]
    fn at_gap_to_immeadiately_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(p, ..cached_offset! {l 1 o 2 i 7}),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn at_gap_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(p, ..cached_offset! {l 2 o 0 i 9}),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn not_found_at_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 1 o 1}, ..cached_offset! {l 1 o 1 i 6}),
            None
        );
    }
    #[test]
    fn after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, ..cached_offset! {l 2 o 2 i 11}),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn not_found_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 1}, ..cached_offset! {l 2 o 1 i 10}),
            None
        );
    }
    #[test]
    fn at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, ..cached_offset! {l 4 o 0 i 13}),
            Some(gap_informed!(12, buffer))
        );
    }
    #[test]
    fn not_found_at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, ..cached_offset! {l 2 o 3 i 12}),
            None
        );
    }
    #[test]
    fn at_end_when_gap_is_at_end() {
        let buffer = init!("1234\n567\n890" gap 12);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, ..cached_offset! {l 4 o 0 i 13}),
            Some(gap_informed!(12, buffer))
        );
    }
    #[test]
    fn not_found_at_end_when_gap_is_at_end() {
        let buffer = init!("1234\n567\n890" gap 12);

        assert_eq!(
            buffer.find_index_within_range(pos! {l 2 o 3}, ..cached_offset! {l 2 o 3 i 12}),
            None
        );
    }
}

mod find_index_included_to_excluded {
    use super::*;
    #[test]
    fn at_start_to_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer
                .find_index_within_range(pos! {}, cached_offset! {}..cached_offset! {l 0 o 4 i 4}),
            Some(gap_informed!(0, buffer))
        );
    }
    #[test]
    fn not_found_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {},
                cached_offset! {l 0 o 1 i 1}..cached_offset! {l 1 o 4 i 4}
            ),
            None
        );
    }
    #[test]
    fn at_start_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer
                .find_index_within_range(pos! {}, cached_offset! {}..cached_offset! {l 1 o 3 i 8}),
            Some(gap_informed!(0, buffer))
        );
    }
    #[test]
    fn not_found_immeadiately_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {},
                cached_offset! {l 0 o 1 i 1}..cached_offset! {l 1 o 2 i 7}
            ),
            None
        );
    }
    #[test]
    fn not_found_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {},
                cached_offset! {l 0 o 1 i 1}..cached_offset! {l 2 o 0 i 9}
            ),
            None
        );
    }
    #[test]
    fn before_gap_to_before_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 0 o 2};
        assert_eq!(
            buffer.find_index_within_range(
                p,
                cached_offset! {p: p, i 2}..cached_offset! {l 0 o 4 i 4}
            ),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn before_gap_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 0 o 2};
        assert_eq!(
            buffer.find_index_within_range(
                p,
                cached_offset! {p: p, i 2}..cached_offset! {l 1 o 3 i 8}
            ),
            Some(gap_informed!(2, buffer))
        );
    }
    #[test]
    fn not_found_by_excluding() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 0 o 2},
                cached_offset! {}..cached_offset! {l 0 o 2 i 2}
            ),
            None
        );
    }
    #[test]
    fn at_gap_to_immeadiately_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(p, ..cached_offset! {l 1 o 2 i 7}),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn at_gap_to_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        let p = pos! {l 1 o 1};
        assert_eq!(
            buffer.find_index_within_range(p, ..cached_offset! {l 2 o 0 i 9}),
            Some(gap_informed!(6, buffer))
        );
    }
    #[test]
    fn not_found_at_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 1 o 1},
                cached_offset! {}..cached_offset! {l 1 o 1 i 6}
            ),
            None
        );
    }
    #[test]
    fn after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 1},
                cached_offset! {}..cached_offset! {l 2 o 2 i 11}
            ),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn not_found_by_excluding_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 1},
                cached_offset! {}..cached_offset! {l 2 o 1 i 10}
            ),
            None
        );
    }
    #[test]
    fn after_gap_from_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 1},
                cached_offset! {l 2 o 0 i 9}..cached_offset! {l 2 o 2 i 11}
            ),
            Some(gap_informed!(10, buffer))
        );
    }
    #[test]
    fn not_found_after_gap_from_after_gap() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 0},
                cached_offset! {l 2 o 1 i 10}..cached_offset! {l 2 o 2 i 11}
            ),
            None
        );
    }
    #[test]
    fn at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 3},
                cached_offset! {}..cached_offset! {l 4 o 0 i 13}
            ),
            Some(gap_informed!(12, buffer))
        );
    }
    #[test]
    fn not_found_at_end() {
        let buffer = init!("1234\n567\n890" gap 6);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 3},
                cached_offset! {}..cached_offset! {l 2 o 3 i 12}
            ),
            None
        );
    }
    #[test]
    fn at_end_when_gap_is_at_end() {
        let buffer = init!("1234\n567\n890" gap 12);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 3},
                cached_offset! {}..cached_offset! {l 4 o 0 i 13}
            ),
            Some(gap_informed!(12, buffer))
        );
    }
    #[test]
    fn not_found_at_end_when_gap_is_at_end() {
        let buffer = init!("1234\n567\n890" gap 12);

        assert_eq!(
            buffer.find_index_within_range(
                pos! {l 2 o 3},
                cached_offset! {}..cached_offset! {l 2 o 3 i 12}
            ),
            None
        );
    }
}

use std::ops::{Bound, RangeBounds};
#[test]
fn get_index_bounds_works_on_empty_cache() {
    let output = get_index_bounds(OffsetCache::default(), pos! {});

    assert_eq!(output.start_bound(), Bound::Unbounded);
    assert_eq!(output.end_bound(), Bound::Unbounded);
}

#[test]
fn get_index_bounds_length_1_cache_at_start() {
    let output = get_index_bounds(vec![cached_offset! {l 1 o 2 i 7}], pos! {});

    assert_eq!(output.start_bound(), Bound::Unbounded);
    assert_eq!(
        output.end_bound(),
        Bound::Excluded(&cached_offset! {l 1 o 2 i 7})
    );
}

#[test]
fn get_index_bounds_length_1_cache_after_start() {
    let output = get_index_bounds(vec![cached_offset! {l 1 o 2 i 7}], pos! {l 1 o 0});

    assert_eq!(output.start_bound(), Bound::Unbounded);
    assert_eq!(
        output.end_bound(),
        Bound::Excluded(&cached_offset! {l 1 o 2 i 7})
    );
}

#[test]
fn get_index_bounds_length_1_cache_on_node_0() {
    let output = get_index_bounds(vec![cached_offset! {l 1 o 2 i 7}], pos! {l 1 o 2});

    assert_eq!(
        output.start_bound(),
        Bound::Included(&cached_offset! {l 1 o 2 i 7})
    );
    assert_eq!(
        output.end_bound(),
        Bound::Included(&cached_offset! {l 1 o 2 i 7})
    );
}

#[test]
fn get_index_bounds_length_1_cache_after_middle() {
    let output = get_index_bounds(vec![cached_offset! {l 1 o 2 i 7}], pos! {l 1 o 3});

    assert_eq!(
        output.start_bound(),
        Bound::Excluded(&cached_offset! {l 1 o 2 i 7})
    );
    assert_eq!(output.end_bound(), Bound::Unbounded);
}

#[test]
fn get_index_bounds_length_1_cache_far_after_middle() {
    let output = get_index_bounds(vec![cached_offset! {l 1 o 2 i 7}], pos! {l 2 o 3});

    assert_eq!(
        output.start_bound(),
        Bound::Excluded(&cached_offset! {l 1 o 2 i 7})
    );
    assert_eq!(output.end_bound(), Bound::Unbounded);
}

#[test]
fn get_index_bounds_length_2_cache_at_start() {
    let output = get_index_bounds(
        vec![cached_offset! {l 1 o 2 i 7}, cached_offset! {l 0 o 2 i 2}],
        pos! {},
    );

    assert_eq!(output.start_bound(), Bound::Unbounded);
    assert_eq!(
        output.end_bound(),
        Bound::Excluded(&cached_offset! {l 0 o 2 i 2})
    );
}

#[test]
fn get_index_bounds_length_2_cache_after_start() {
    let output = get_index_bounds(
        vec![cached_offset! {l 1 o 2 i 7}, cached_offset! {l 0 o 2 i 2}],
        pos! {l 0 o 1},
    );

    assert_eq!(output.start_bound(), Bound::Unbounded);
    assert_eq!(
        output.end_bound(),
        Bound::Excluded(&cached_offset! {l 0 o 2 i 2})
    );
}

#[test]
fn get_index_bounds_length_2_cache_on_node_1() {
    let output = get_index_bounds(
        vec![cached_offset! {l 1 o 2 i 7}, cached_offset! {l 0 o 2 i 2}],
        pos! {l 0 o 2},
    );

    assert_eq!(
        output.start_bound(),
        Bound::Included(&cached_offset! {l 0 o 2 i 2})
    );
    assert_eq!(
        output.end_bound(),
        Bound::Included(&cached_offset! {l 0 o 2 i 2})
    );
}

#[test]
fn get_index_bounds_length_2_cache_after_node_1() {
    let output = get_index_bounds(
        vec![cached_offset! {l 1 o 2 i 7}, cached_offset! {l 0 o 2 i 2}],
        pos! {l 0 o 3},
    );

    assert_eq!(
        output.start_bound(),
        Bound::Excluded(&cached_offset! {l 0 o 2 i 2})
    );
    assert_eq!(
        output.end_bound(),
        Bound::Excluded(&cached_offset! {l 1 o 2 i 7})
    );
}

#[test]
fn get_index_bounds_length_2_cache_on_node_0() {
    let output = get_index_bounds(
        vec![cached_offset! {l 1 o 2 i 7}, cached_offset! {l 0 o 2 i 2}],
        pos! {l 1 o 2},
    );

    assert_eq!(
        output.start_bound(),
        Bound::Included(&cached_offset! {l 1 o 2 i 7})
    );
    assert_eq!(
        output.end_bound(),
        Bound::Included(&cached_offset! {l 1 o 2 i 7})
    );
}

#[test]
fn get_index_bounds_length_2_cache_after_node_0() {
    let output = get_index_bounds(
        vec![cached_offset! {l 1 o 2 i 7}, cached_offset! {l 0 o 2 i 2}],
        pos! {l 2 o 2},
    );

    assert_eq!(
        output.start_bound(),
        Bound::Excluded(&cached_offset! {l 1 o 2 i 7})
    );
    assert_eq!(output.end_bound(), Bound::Unbounded);
}

#[test]
fn get_index_bounds_length_3_cache_at_start() {
    let output = get_index_bounds(
        vec![
            cached_offset! {l 1 o 2 i 7},
            cached_offset! {l 0 o 2 i 2},
            cached_offset! {l 2 o 2 i 11},
        ],
        pos! {},
    );

    assert_eq!(output.start_bound(), Bound::Unbounded);
    assert_eq!(
        output.end_bound(),
        Bound::Excluded(&cached_offset! {l 0 o 2 i 2})
    );
}

#[test]
fn get_index_bounds_length_3_cache_after_start() {
    let output = get_index_bounds(
        vec![
            cached_offset! {l 1 o 2 i 7},
            cached_offset! {l 0 o 2 i 2},
            cached_offset! {l 2 o 2 i 11},
        ],
        pos! {l 0 o 1},
    );

    assert_eq!(output.start_bound(), Bound::Unbounded);
    assert_eq!(
        output.end_bound(),
        Bound::Excluded(&cached_offset! {l 0 o 2 i 2})
    );
}

#[test]
fn get_index_bounds_length_3_cache_on_node_1() {
    let output = get_index_bounds(
        vec![
            cached_offset! {l 1 o 2 i 7},
            cached_offset! {l 0 o 2 i 2},
            cached_offset! {l 2 o 2 i 11},
        ],
        pos! {l 0 o 2},
    );

    assert_eq!(
        output.start_bound(),
        Bound::Included(&cached_offset! {l 0 o 2 i 2})
    );
    assert_eq!(
        output.end_bound(),
        Bound::Included(&cached_offset! {l 0 o 2 i 2})
    );
}

#[test]
fn get_index_bounds_length_3_cache_after_node_1() {
    let output = get_index_bounds(
        vec![
            cached_offset! {l 1 o 2 i 7},
            cached_offset! {l 0 o 2 i 2},
            cached_offset! {l 2 o 2 i 11},
        ],
        pos! {l 0 o 3},
    );

    assert_eq!(
        output.start_bound(),
        Bound::Excluded(&cached_offset! {l 0 o 2 i 2})
    );
    assert_eq!(
        output.end_bound(),
        Bound::Excluded(&cached_offset! {l 1 o 2 i 7})
    );
}

#[test]
fn get_index_bounds_length_3_cache_on_node_0() {
    let output = get_index_bounds(
        vec![
            cached_offset! {l 1 o 2 i 7},
            cached_offset! {l 0 o 2 i 2},
            cached_offset! {l 2 o 2 i 11},
        ],
        pos! {l 1 o 2},
    );

    assert_eq!(
        output.start_bound(),
        Bound::Included(&cached_offset! {l 1 o 2 i 7})
    );
    assert_eq!(
        output.end_bound(),
        Bound::Included(&cached_offset! {l 1 o 2 i 7})
    );
}

#[test]
fn get_index_bounds_length_3_cache_after_node_0_before_node_2() {
    let output = get_index_bounds(
        vec![
            cached_offset! {l 1 o 2 i 7},
            cached_offset! {l 0 o 2 i 2},
            cached_offset! {l 2 o 2 i 11},
        ],
        pos! {l 2 o 1},
    );

    assert_eq!(
        output.start_bound(),
        Bound::Excluded(&cached_offset! {l 1 o 2 i 7})
    );
    assert_eq!(
        output.end_bound(),
        Bound::Excluded(&cached_offset! {l 2 o 2 i 11})
    );
}

#[test]
fn get_index_bounds_length_3_cache_at_node_2() {
    let output = get_index_bounds(
        vec![
            cached_offset! {l 1 o 2 i 7},
            cached_offset! {l 0 o 2 i 2},
            cached_offset! {l 2 o 2 i 11},
        ],
        pos! {l 2 o 2},
    );

    assert_eq!(
        output.start_bound(),
        Bound::Included(&cached_offset! {l 2 o 2 i 11})
    );
    assert_eq!(
        output.end_bound(),
        Bound::Included(&cached_offset! {l 2 o 2 i 11})
    );
}

#[test]
fn get_index_bounds_length_3_cache_after_node_2() {
    let output = get_index_bounds(
        vec![
            cached_offset! {l 1 o 2 i 7},
            cached_offset! {l 0 o 2 i 2},
            cached_offset! {l 2 o 2 i 11},
        ],
        pos! {l 2 o 4},
    );

    assert_eq!(
        output.start_bound(),
        Bound::Excluded(&cached_offset! {l 2 o 2 i 11})
    );
    assert_eq!(output.end_bound(), Bound::Unbounded);
}

fn assert_is_reasonable_all_cached_offsets(offsets: Vec<CachedOffset>, buffer: GapBuffer) {
    use std::collections::HashMap;
    let len = offsets.len();

    // It should included the offst before the first character and after the last character.
    assert_eq!(len, buffer.graphemes().count() + 1);

    let mut pos_counts: HashMap<_, usize> = HashMap::with_capacity(len);
    let mut index_counts: HashMap<_, usize> = HashMap::with_capacity(len);
    let mut offset_counts: HashMap<_, usize> = HashMap::with_capacity(len);

    for offset in offsets.iter() {
        let pos_count = pos_counts.entry(offset.position).or_insert(0);
        *pos_count += 1;

        let index_count = index_counts.entry(offset.index).or_insert(0);
        *index_count += 1;

        let offset_count = offset_counts.entry(offset).or_insert(0);
        *offset_count += 1;
    }

    for (k, count) in pos_counts {
        assert_eq!(
            count, 1,
            "count for {:?} is {}.\noffsets: {:#?}",
            k, count, offsets
        );
    }

    for (k, count) in index_counts {
        assert_eq!(
            count, 1,
            "count for {:?} is {}.\noffsets: {:#?}",
            k, count, offsets
        );
    }

    for (k, count) in offset_counts {
        assert_eq!(
            count, 1,
            "count for {:?} is {}.\noffsets: {:#?}",
            k, count, offsets
        );
    }
}

#[test]
fn get_all_cached_offsets_works_on_single_char() {
    let buffer = GapBuffer::new("a".to_owned());
    let offsets = buffer.get_all_cached_offsets();

    assert_is_reasonable_all_cached_offsets(offsets, buffer);
}

proptest! {
    #[test]
    fn get_all_cached_offsets_works(s in TYPEABLE) {
        let buffer = GapBuffer::new(s);
        let offsets = buffer.get_all_cached_offsets();

        assert_is_reasonable_all_cached_offsets(offsets, buffer);
    }
}

mod optimal_offset_cache_from_all_cached_offsets_tests {
    use super::*;
    const F: fn(Vec<CachedOffset>, NonZeroUsize) -> OffsetCache =
        optimal_offset_cache_from_all_cached_offsets;

    #[test]
    fn works_on_empty_vector() {
        let output = F(vec![], TEST_BLOCK_SIZE);

        assert_eq!(output, vec![]);
    }

    #[test]
    fn does_not_crash_given_duplicates() {
        F(
            vec![
                cached_offset! {l 1 o 2 i 7},
                cached_offset! {l 1 o 2 i 7},
                cached_offset! {l 1 o 2 i 7},
            ],
            TEST_BLOCK_SIZE,
        );

        assert!(true);
    }

    fn is_sorted<P, I>(mut iterator: I) -> bool
    where
        P: PartialOrd,
        I: Iterator<Item = P>,
    {
        let mut previous: P = match iterator.next() {
            Some(e) => e,
            None => return true,
        };

        while let Some(current) = iterator.next() {
            if previous
                .partial_cmp(&current)
                .map(|o| o == std::cmp::Ordering::Greater)
                .unwrap_or(true)
            {
                return false;
            }
            previous = current;
        }

        true
    }

    macro_rules! assert_reasonable_output {
        ($output: ident, $block_size: ident, $offsets: ident) => {
            let len = $offsets.len();

            let minimum_len = if len <= 1 {
                len
            } else {
                std::cmp::max(len / $block_size.get(), 1)
            };
            assert!(
                $output.len() >= minimum_len,
                "{} not <= to {}.\n output: {:#?}",
                minimum_len,
                $output.len(),
                $output
            );
            let maximun_len = minimum_len * 2 + 1;
            assert!(
                $output.len() < maximun_len,
                "{} not < {}.\n output: {:#?}",
                $output.len(),
                maximun_len,
                $output
            );
            for c_o in $output.iter() {
                assert!(
                    $offsets.contains(c_o),
                    "{:?} does not contain {:?}",
                    $offsets,
                    c_o
                );
            }

            assert!(is_sorted($offsets.iter()));

            let deduped = {
                let mut d = $offsets.clone();
                d.dedup();
                d
            };
            assert!(
                deduped.len() == $offsets.len(),
                "{:?} contains duplicates",
                $offsets,
            );

            for chunk in $offsets.chunks($block_size.get()) {
                assert!(
                    $output.iter().any(|o| chunk.contains(o)),
                    "{:#?} does not contain any element of {:#?}",
                    $output,
                    chunk
                );
            }
        };
    }

    #[test]
    fn works_on_single_offset() {
        let offsets = vec![d!()];

        let output = F(offsets.clone(), TEST_BLOCK_SIZE);

        assert_reasonable_output!(output, TEST_BLOCK_SIZE, offsets);
    }

    #[test]
    fn works_on_less_than_one_block() {
        let offsets = vec![
            cached_offset! {},
            cached_offset! {l 0 o 1 i 1},
            cached_offset! {l 0 o 2 i 2},
            cached_offset! {l 0 o 3 i 3},
            cached_offset! {l 0 o 4 i 4},
            cached_offset! {l 1 o 0 i 5},
            cached_offset! {l 1 o 1 i 6},
            // cached_offset! {l 1 o 2 i 7},
            // cached_offset! {l 1 o 3 i 8},
            // cached_offset! {l 2 o 0 i 9},
            // cached_offset! {l 2 o 1 i 10},
            // cached_offset! {l 2 o 2 i 11},
            // cached_offset! {l 2 o 3 i 12},
        ];

        let output = F(offsets.clone(), TEST_BLOCK_SIZE);

        assert_reasonable_output!(output, TEST_BLOCK_SIZE, offsets);
    }

    #[test]
    fn works_on_string_with_newline_at_index_5() {
        let buffer = GapBuffer::new("12345\n78".to_owned());
        let offsets = dbg!(buffer.get_all_cached_offsets());

        let output = F(offsets.clone(), TEST_BLOCK_SIZE);

        assert_reasonable_output!(output, TEST_BLOCK_SIZE, offsets);
    }

    proptest! {
        #[test]
        fn works_on_multiple_blocks(s in TYPEABLE) {
            let buffer = GapBuffer::new(s);
            let offsets = buffer.get_all_cached_offsets();

            let output = F(offsets.clone(), TEST_BLOCK_SIZE);

            assert_reasonable_output!(output, TEST_BLOCK_SIZE, offsets);
        }
    }

}

proptest! {
    #[test]
    fn inform_of_gap_and_remove_gap_knowledge_composed_is_identity(s in TYPEABLE, i in 0usize..) {
        let buffer = GapBuffer::new(s);
        assert_eq!(
            remove_gap_knowledge(inform_of_gap(GapObliviousByteIndex(i), &buffer), &buffer).0,
            i
        );
        assert_eq!(
            inform_of_gap(remove_gap_knowledge(ByteIndex(i), &buffer), &buffer).0,
            i
        );
    }
}
