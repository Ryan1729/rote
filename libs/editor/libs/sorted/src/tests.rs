use super::*;

use macros::d;
use proptest::proptest;

#[test]
fn spans_looks_like_it_works() {
    let sorted: Sorted<_> = vec![7, 2, 11].into();

    assert_eq!(spans(sorted), vec![(2, 7), (7, 11)])
}

#[test]
fn spans_works_on_a_single_offset() {
    let sorted: Sorted<_> = vec![7].into();

    assert_eq!(spans(sorted), vec![(7, 7)])
}

mod get_tree_bounds_tests {
    use super::*;
    use std::ops::{Bound, RangeBounds};

    type TestType = u8;

    //This way is nicer to use with parameters that are literals.
    fn f(s: Sorted<TestType>, target: TestType) -> (Bound<TestType>, Bound<TestType>) {
        let bounds = get_tree_bounds(s, target);
        (
            cloned_bound(bounds.start_bound()),
            cloned_bound(bounds.end_bound()),
        )
    }

    #[test]
    fn works_on_empty_vec() {
        let output = f(d!(), d!());

        assert_eq!(output.start_bound(), Bound::Unbounded);
        assert_eq!(output.end_bound(), Bound::Unbounded);
    }

    #[test]
    fn length_1_vec_at_start() {
        let output = f(vec![7].into(), 0);

        assert_eq!(output.start_bound(), Bound::Unbounded);
        assert_eq!(output.end_bound(), Bound::Excluded(&7));
    }

    #[test]
    fn length_1_vec_after_start() {
        let output = f(vec![7].into(), 3);

        assert_eq!(output.start_bound(), Bound::Unbounded);
        assert_eq!(output.end_bound(), Bound::Excluded(&7));
    }

    #[test]
    fn length_1_vec_on_node_0() {
        let output = f(vec![7].into(), 7);

        assert_eq!(output.start_bound(), Bound::Included(&7));
        assert_eq!(output.end_bound(), Bound::Included(&7));
    }

    #[test]
    fn length_1_vec_after_middle() {
        let output = f(vec![7].into(), 9);

        assert_eq!(output.start_bound(), Bound::Excluded(&7));
        assert_eq!(output.end_bound(), Bound::Unbounded);
    }

    #[test]
    fn length_1_vec_far_after_middle() {
        let output = f(vec![7].into(), 100);

        assert_eq!(output.start_bound(), Bound::Excluded(&7));
        assert_eq!(output.end_bound(), Bound::Unbounded);
    }

    #[test]
    fn length_2_vec_at_start() {
        let output = f(vec![7, 2].into(), 0);

        assert_eq!(output.start_bound(), Bound::Unbounded);
        assert_eq!(output.end_bound(), Bound::Excluded(&2));
    }

    #[test]
    fn length_2_vec_after_start() {
        let output = f(vec![7, 2].into(), 1);

        assert_eq!(output.start_bound(), Bound::Unbounded);
        assert_eq!(output.end_bound(), Bound::Excluded(&2));
    }

    #[test]
    fn length_2_vec_on_node_1() {
        let output = f(vec![7, 2].into(), 2);

        assert_eq!(output.start_bound(), Bound::Included(&2));
        assert_eq!(output.end_bound(), Bound::Included(&2));
    }

    #[test]
    fn length_2_vec_after_node_1() {
        let output = f(vec![7, 2].into(), 3);

        assert_eq!(output.start_bound(), Bound::Excluded(&2));
        assert_eq!(output.end_bound(), Bound::Excluded(&7));
    }

    #[test]
    fn length_2_vec_on_node_0() {
        let output = f(vec![7, 2].into(), 7);

        assert_eq!(output.start_bound(), Bound::Included(&7));
        assert_eq!(output.end_bound(), Bound::Included(&7));
    }

    #[test]
    fn length_2_vec_after_node_0() {
        let output = f(vec![7, 2].into(), 9);

        assert_eq!(output.start_bound(), Bound::Excluded(&7));
        assert_eq!(output.end_bound(), Bound::Unbounded);
    }

    #[test]
    fn length_3_vec_at_start() {
        let output = f(vec![7, 2, 11].into(), 0);

        assert_eq!(output.start_bound(), Bound::Unbounded);
        assert_eq!(output.end_bound(), Bound::Excluded(&2));
    }

    #[test]
    fn length_3_vec_after_start() {
        let output = f(vec![7, 2, 11].into(), 1);

        assert_eq!(output.start_bound(), Bound::Unbounded);
        assert_eq!(output.end_bound(), Bound::Excluded(&2));
    }

    #[test]
    fn length_3_vec_on_node_1() {
        let output = f(vec![7, 2, 11].into(), 2);

        assert_eq!(output.start_bound(), Bound::Included(&2));
        assert_eq!(output.end_bound(), Bound::Included(&2));
    }

    #[test]
    fn length_3_vec_after_node_1() {
        let output = f(vec![7, 2, 11].into(), 3);

        assert_eq!(output.start_bound(), Bound::Excluded(&2));
        assert_eq!(output.end_bound(), Bound::Excluded(&7));
    }

    #[test]
    fn length_3_vec_on_node_0() {
        let output = f(vec![7, 2, 11].into(), 7);

        assert_eq!(output.start_bound(), Bound::Included(&7));
        assert_eq!(output.end_bound(), Bound::Included(&7));
    }

    #[test]
    fn length_3_vec_after_node_0_before_node_2() {
        let output = f(vec![7, 2, 11].into(), 9);

        assert_eq!(output.start_bound(), Bound::Excluded(&7));
        assert_eq!(output.end_bound(), Bound::Excluded(&11));
    }

    #[test]
    fn length_3_vec_at_node_2() {
        let output = f(vec![7, 2, 11].into(), 11);

        assert_eq!(output.start_bound(), Bound::Included(&11));
        assert_eq!(output.end_bound(), Bound::Included(&11));
    }

    #[test]
    fn length_3_vec_after_node_2() {
        let output = f(vec![7, 2, 11].into(), 13);

        assert_eq!(output.start_bound(), Bound::Excluded(&11));
        assert_eq!(output.end_bound(), Bound::Unbounded);
    }
}
