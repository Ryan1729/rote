use std::borrow::Borrow;

#[derive(Debug, Default)]
pub struct SearchTree<O: Ord>(Vec<O>);

impl <O: Ord> From<Vec<O>> for SearchTree<O> {
    fn from(vec: Vec<O>) -> SearchTree<O> {
        //TODO ensure the vector is in tree order.
        SearchTree(vec)
    }
}

impl <O: Ord> SearchTree<O> {
    pub fn into_vec(self) -> Vec<O> {
        let SearchTree(vec) = self;
        vec
    }

    pub fn get<I>(&self, index: I) -> Option<&I::Output>
        where I: std::slice::SliceIndex<[O]>
    {
        self.0.get(index)
    }

    pub fn iter(&self) -> std::slice::Iter<O> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn insert(&mut self, index: usize, element: O) {
        //TODO write insert that rebalances tree if necessary
        self.0.insert(index, element)
    }
}

impl <O: Ord + Clone> SearchTree<O> {
    pub fn in_order(&self) -> Vec<O> {
        in_order(self)
    }
    pub fn spans(&self) -> Vec<(O, O)> {
        spans(self)
    }
}

pub fn in_order<O : Ord + Clone>(cache: &SearchTree<O>) -> Vec<O> {
    let mut output = Vec::with_capacity(cache.len());
    in_order_helper(cache, 0, &mut output);
    output
}

fn in_order_helper<O : Ord + Clone>(cache: &SearchTree<O>, index: usize, output: &mut Vec<O>) {
    if let Some(offset) = cache.get(index) {
        in_order_helper(cache, index * 2 + 1, output);
        output.push(offset.clone());
        in_order_helper(cache, index * 2 + 2, output);
    }
}

pub fn spans<S, O>(cache: S) -> Vec<(O, O)>
where S : Borrow<SearchTree<O>>,
    O : Ord + Clone {
    let cache = cache.borrow();

    let in_order = in_order(cache);

    let mut output = Vec::with_capacity(in_order.len());
    if in_order.len() == 1 {
        output.push((in_order[0].clone(), in_order[0].clone()));
    } else {
        for slice in in_order.windows(2) {
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

impl <O: Ord + PartialEq> PartialEq for SearchTree<O> {
    fn eq(&self, other: &SearchTree<O>) -> bool {
        self.0 == other.0
    }
}

impl <O: Ord> PartialEq<Vec<O>> for SearchTree<O> {
    fn eq(&self, other: &Vec<O>) -> bool {
        &self.0 == other
    }
}

#[cfg(test)]
mod tests {
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
        assert_eq!(
            spans(vec![
                cached_offset! {l 1 o 2 i 7},
                cached_offset! {l 0 o 2 i 2},
                cached_offset! {l 2 o 2 i 11},
            ].into()),
            vec![
                (cached_offset! {l 0 o 2 i 2}, cached_offset! {l 1 o 2 i 7}),
                (cached_offset! {l 1 o 2 i 7}, cached_offset! {l 2 o 2 i 11})
            ]
        )
    }

    #[test]
    fn spans_works_on_a_single_offset() {
        assert_eq!(
            spans(vec![cached_offset! {l 1 o 2 i 7}].into()),
            vec![(cached_offset! {l 1 o 2 i 7}, cached_offset! {l 1 o 2 i 7}),]
        )
    }

}
