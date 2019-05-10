use std::borrow::Borrow;
use std::cmp::Ordering;
use std::ops::{Bound, RangeBounds};

#[derive(Debug, Default)]
pub struct Sorted<O: Ord>(Vec<O>);

impl<O: Ord> From<Vec<O>> for Sorted<O> {
    fn from(vec: Vec<O>) -> Sorted<O> {
        Sorted::new(vec)
    }
}

impl<O: Ord + PartialEq> PartialEq for Sorted<O> {
    fn eq(&self, other: &Sorted<O>) -> bool {
        self.0 == other.0
    }
}

impl<O: Ord> PartialEq<Vec<O>> for Sorted<O> {
    fn eq(&self, other: &Vec<O>) -> bool {
        &self.0 == other
    }
}

impl<O: Ord> std::iter::FromIterator<O> for Sorted<O> {
    fn from_iter<I: IntoIterator<Item = O>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let (l, u) = iter.size_hint();
        let mut v = Vec::with_capacity(u.unwrap_or(l));

        for e in iter {
            v.push(e);
        }

        v.into()
    }
}

impl<O: Ord> Sorted<O> {
    pub fn new(mut vec: Vec<O>) -> Self {
        vec.sort_unstable();

        Sorted(vec)
    }

    pub fn new_unchecked(vec: Vec<O>) -> Self {
        Sorted(vec)
    }

    pub fn into_vec(self) -> Vec<O> {
        self.0
    }

    pub fn get<I>(&self, index: I) -> Option<&I::Output>
    where
        I: std::slice::SliceIndex<[O]>,
    {
        self.0.get(index)
    }

    pub fn iter(&self) -> std::slice::Iter<O> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn insert(&mut self, element: O) {
        match self.0.binary_search(&element) {
            Ok(i) => self.0[i] = element,
            Err(i) => self.0.insert(i, element),
        }
    }
}

impl<O: Ord + Clone> Sorted<O> {
    pub fn spans(&self) -> Vec<(O, O)> {
        spans(self)
    }
}

pub fn spans<S, O>(sorted: S) -> Vec<(O, O)>
where
    S: Borrow<Sorted<O>>,
    O: Ord + Clone,
{
    let sorted = sorted.borrow();

    let vec = &sorted.0;

    let mut output = Vec::with_capacity(vec.len());
    if vec.len() == 1 {
        output.push((vec[0].clone(), vec[0].clone()));
    } else {
        for slice in vec.windows(2) {
            output.push((slice[0].clone(), slice[1].clone()));
        }
    }

    output
}

pub fn extrema<O: Ord + Clone>(spans: &Vec<(O, O)>) -> Option<(O, O)> {
    match (spans.first(), spans.last()) {
        (None, None) | (None, Some(_)) | (Some(_), None) => None,
        (Some((first, _)), Some((_, last))) => Some((first.clone(), last.clone())),
    }
}

/// Interpret the `Sorted<O>` as a binary tree and get bounds (containing nodes of the tree)
/// which contain the target element.
pub fn get_tree_bounds<O, S>(sorted: S, target: O) -> impl RangeBounds<O> + Clone
where
    O: Ord + Clone,
    S: Borrow<Sorted<O>>,
{
    get_tree_bounds_by(sorted, move |o: &O| o.cmp(&target))
}

pub fn get_tree_bounds_by<O, S, F>(sorted: S, compare: F) -> impl RangeBounds<O> + Clone
where
    O: Ord + Clone,
    S: Borrow<Sorted<O>>,
    F: Fn(&O) -> Ordering,
{
    use Bound::{Excluded, Included, Unbounded};
    use Ordering::{Equal, Greater, Less};
    let sorted = sorted.borrow();

    let mut lower = Unbounded;
    let mut upper = Unbounded;
    let mut len = sorted.len();
    let mut left_edge = 0;

    loop {
        let half = dbg!(len / 2);
        let middle = dbg!(left_edge + half);
        let current = if let Some(current) = sorted.get(middle) {
            current
        } else {
            return (lower, upper);
        };
        match dbg!(compare(current)) {
            Greater => {
                upper = Excluded((*current).clone());
            }
            Equal => {
                return (Included((*current).clone()), Included((*current).clone()));
            }
            Less => {
                lower = Excluded((*current).clone());
                left_edge = middle;
            }
        }
        len -= half;
        if half < 1 {
            return (lower, upper);
        }
    }
}

pub fn to_bound_pair<O: Ord + Clone, R: RangeBounds<O> + Clone>(bounds: R) -> (Bound<O>, Bound<O>) {
    (
        cloned_bound(bounds.start_bound()),
        cloned_bound(bounds.end_bound()),
    )
}

pub fn cloned_bound<T: Clone>(bound: Bound<&T>) -> Bound<T> {
    match bound {
        Bound::Unbounded => Bound::Unbounded,
        Bound::Included(b) => Bound::Included(b.clone()),
        Bound::Excluded(b) => Bound::Excluded(b.clone()),
    }
}

#[cfg(test)]
mod tests;
