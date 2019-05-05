use std::borrow::Borrow;

#[derive(Debug, Default)]
pub struct SearchTree<O: Ord>(Vec<O>);

impl <O: Ord> From<Vec<O>> for SearchTree<O> {
    fn from(vec: Vec<O>) -> SearchTree<O> {
        SearchTree::new(vec)
    }
}

impl <O: Ord> std::iter::FromIterator<O> for SearchTree<O> {
    fn from_iter<I: IntoIterator<Item=O>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let (l, u) = iter.size_hint();
        let mut v = Vec::with_capacity(u.unwrap_or(l));

        for e in iter {
            v.push(e);
        }

        v.into()
    }
}

impl <O: Ord> SearchTree<O> {
    pub fn new(mut vec: Vec<O>) -> Self {
        put_in_tree_order(&mut vec);

        SearchTree(vec)
    }

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

pub fn in_order<O : Ord + Clone>(tree: &SearchTree<O>) -> Vec<O> {
    let mut output = Vec::with_capacity(tree.len());
    in_order_helper(tree, 0, &mut output);
    output
}

fn in_order_helper<O : Ord + Clone>(tree: &SearchTree<O>, index: usize, output: &mut Vec<O>) {
    if let Some(offset) = tree.get(index) {
        in_order_helper(tree, index * 2 + 1, output);
        output.push(offset.clone());
        in_order_helper(tree, index * 2 + 2, output);
    }
}

pub fn spans<S, O>(tree: S) -> Vec<(O, O)>
where S : Borrow<SearchTree<O>>,
    O : Ord + Clone {
    let tree = tree.borrow();

    let in_order = in_order(tree);

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

pub fn put_in_tree_order<O: Ord>(vec: &mut Vec<O>) {
    vec.sort_unstable();

    put_in_tree_order_helper(vec, 0);
}

fn put_in_tree_order_helper<O : Ord>(output: &mut Vec<O>, index: usize) {
    // if let Some(offset) = cache.get(index) {
    //     in_order_helper(cache, index * 2 + 1, output);
    //     output.push(offset.clone());
    //     in_order_helper(cache, index * 2 + 2, output);
    // }
}

#[cfg(test)]
mod tests;
