use super::*;

use proptest::proptest;

#[test]
fn in_order_works_on_a_single_thing() {
    let in_order = in_order(&vec![7].into());
    assert_eq!(in_order, vec![7])
}

#[test]
fn in_order_works_on_multiple_single_things() {
    let in_order = in_order(&vec![7, 3, 11].into());
    assert_eq!(in_order, vec![3, 7, 11])
}

#[test]
fn spans_looks_like_it_works() {
    let search_tree: SearchTree<_> = vec![
        7,
        2,
        11,
    ].into();

    assert_eq!(
        spans(search_tree),
        vec![
            (2, 7),
            (7, 11)
        ]
    )
}

#[test]
fn spans_works_on_a_single_offset() {
    let search_tree: SearchTree<_> = vec![7].into();

    assert_eq!(
        spans(search_tree),
        vec![(7, 7)]
    )
}

// TODO reduce duplication with other tests modules
macro_rules! assert_in_tree_order {
    ($offset_cache: expr) => (
        assert!(
            is_sorted(in_order(&$offset_cache).iter()),
            "\nIn order traversal of `{}` does not produce ordered array: {:?}",
            stringify!($offset_cache),
            in_order(&$offset_cache)
        );
    );
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

proptest!{
    #[test]
    fn SearchTree_new_works_on_full_trees(n in 0..=16) {
        let count = (1 << n) - 1;
        let v: Vec<_> = (0..count).collect();

        let s = SearchTree::new(v);

        assert_in_tree_order!(s)

    }
}
