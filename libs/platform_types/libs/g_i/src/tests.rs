// We seem to be getting false positives here.
#![cfg_attr(feature = "pub_arb", allow(dead_code))]
#![cfg_attr(feature = "pub_arb", allow(unused_imports))]
#![cfg_attr(feature = "pub_arb", allow(unused_macros))]
use super::*;
use proptest::{
    collection::vec,
    prelude::{any, proptest, Just}
};
use arb_macros::{arb_enum};
use macros::{u};
use vec1::{vec1};

macro_rules! g_i_s {
    (g $generation: literal mi $source: literal $target: literal) => {
        State { 
            current: $generation,
            invalidation: Invalidation::MovedTo(source, $target)
        }
    };
    (g $generation: literal ri $index: literal) => {
        State::new_removed_at($generation, IndexPart::or_max($index))
    };
    () => {
        g_i_s!(g 0 ri 0)
    };
}

impl State {
    pub fn new_removed_at(current: Generation, index_part: IndexPart) -> Self {
        State {
            current,
            invalidation: Invalidation::RemovedAt(index_part),
        }
    }

    pub fn get_generation(&self) -> Generation {
        self.current
    }
}

macro_rules! g_i_i {
    (g $generation: literal i $index: literal) => {
        Index::new_from_parts($generation, IndexPart::or_max($index))
    };
    () => {
        g_i_i!(g 0 i 0)
    };
}

impl Index {
    pub fn new_from_parts(generation: Generation, index: IndexPart) -> Self {
        Index { generation, index }
    }

    pub fn get_generation(&self) -> Generation {
        self.generation
    }
}

macro_rules! g_i_as {
    ($($tokens: tt)*) => {{
        u!{SelectionAdjustment}
        u!{SelectionMove}

        vec![$($tokens)*]
    }}
}

macro_rules! g_i_mtk {
    (g $generation: literal i $index_part: literal ri $invalidation_index: literal [$($g_i_as_tokens: tt)*] ($(i)? $i1: literal, $(i)? $i2: literal)) => {
        (
            g_i_i!(g $generation i $index_part),
            g_i_s!(g $generation ri $invalidation_index),
            g_i_as![$($g_i_as_tokens)*],
            (g_i_i!(g $generation i $i1), g_i_i!(g $generation i $i2))
        )
    }
}

macro_rules! g_i_map {
    ($($index_part: literal : $element: expr),*) => {{
        let mut map = Map::with_capacity(Length::or_max(16));
        let state: State = d!();
        $(
            map.insert(state, state.new_index_or_max($index_part), $element);
        )*

        map
    }}
}

#[derive(Clone, Debug)]
pub enum MutMethodSpec<A> {
    GetCurrentElementMut,
    GetCurrentElementOrNoneMut,
    GetMut(Index),
    SetCurrentIndex(Index),
    RemoveIfPresent(Index),
    MoveOrIgnore(Index, Index),
    SwapOrIgnore(Index, Index),
    CloseElement(Index),
    AdjustSelection(SelectionAdjustment),
    PushAndSelectNew(A),
    ReplaceWithMapped(Vec1<A>),
}
d!(<A> for MutMethodSpec<A>: MutMethodSpec::GetCurrentElementMut);

impl <A: Clone> MutMethodSpec<A> {
    fn apply(self, svec1: &mut SelectableVec1<A>) {
        use MutMethodSpec::*;
        match self {
            GetCurrentElementMut => {
                let _ = svec1.get_current_element_mut();
            },
            GetCurrentElementOrNoneMut => {
                let _ = svec1.get_current_element_or_none_mut();
            },
            GetMut(index) => {
                let _ = svec1.get_mut(index);
            },
            SetCurrentIndex(index) => {
                svec1.set_current_index(index);
            },
            PushAndSelectNew(a) => {
                svec1.push_and_select_new(a);
            },
            AdjustSelection(adjustment) => {
                svec1.adjust_selection(adjustment);
            },
            CloseElement(index) => {
                svec1.close_element(index);
            },
            SwapOrIgnore(index1, index2) => {
                svec1.swap_or_ignore(index1, index2);
            },
            MoveOrIgnore(index1, index2) => {
                svec1.move_or_ignore(index1, index2);
            },
            RemoveIfPresent(index) => {
                svec1.remove_if_present(index);
            }
            ReplaceWithMapped(vec1) => {
                svec1.replace_with_mapped(
                    &SelectableVec1::new_from_vec1(
                        vec1
                    ),
                    |a| a.clone()
                );
            }
        }
    }

    fn method_names() -> Vec<&'static str> {
        vec![
            "get_current_element_mut", 
            "get_current_element_or_none_mut",
            "get_mut",
            "set_current_index",
            "push_and_select_new",
            "adjust_selection",
            "close_element",
            "swap_or_ignore",
            "move_or_ignore",
            "remove_if_present",
            "replace_with_mapped",
        ]
    }
}

pub mod arb {
    use super::{*};
    use proptest::prelude::{any, Strategy, prop_compose};
    use proptest::collection::vec;
    use std::convert::TryInto;

    pub fn index_from_parts(generation: Generation, index: IndexPart) -> Index {
        Index::new_from_parts(generation, index)
    }

    pub fn selectable_vec1<'strat, A: std::fmt::Debug + Clone, S: Strategy<Value = A>>(
        strat: S, max_len: LengthSize,
    ) -> impl Strategy<Value = SelectableVec1<A>> {
        (
            vec(strat, 1..(max_len as usize)),
            state_with_index(max_len)
        )
        .prop_map(|(v, (current_index, i_s))| {
            let elements = Vec1::try_from_vec(v).unwrap();
            SelectableVec1::from_parts(
                elements,
                i_s,
                current_index,
            )
        })
    }

    pub fn index_part(max_len: LengthSize) -> impl Strategy<Value = IndexPart> {
        (0..=max_len).prop_map(|i| IndexPart::or_max(i as _))
    }

    arb_enum!{
        fn invalidation(max_index: LengthSize) -> Invalidation {
            RemovedAt(_) => { index_part(max_index).prop_map(RemovedAt) },
            SwappedAt(_, _) => { 
                (0..=max_index, 0..=max_index).prop_map(|(a, b)| {
                    Invalidation::SwappedAt(
                        IndexPart::or_max(a as usize),
                        IndexPart::or_max(b as usize)
                    )
                })
            },
            MovedTo(_, _) => { 
                (0..=max_index, 0..=max_index).prop_map(|(a, b)| {
                    Invalidation::MovedTo(
                        IndexPart::or_max(a as usize),
                        IndexPart::or_max(b as usize)
                    )
                })
            }
       }
    }

    pub fn state(max_index: LengthSize) -> impl Strategy<Value = State> {
        (any::<Generation>(), invalidation(max_index))
            .prop_map(|(current, invalidation)| State {
                current,
                // invalidations other than the default are impossible
                // for generation 0 with the current API. Having 
                // impossible states get generated makes reducing down
                // tests inconvenient.
                invalidation: if current == 0 { d!() } else { invalidation },
            })
    }

    pub fn state_with_default_invalidation() -> impl Strategy<Value = State> {
        any::<Generation>()
            .prop_map(move |current| State {
                current,
                invalidation: d!(),
            })
    }

    prop_compose!{
        pub fn state_with_index(max_index: LengthSize)
           (s in {state(max_index)}, i_p in index_part(max_index))
           -> (Index, State) {
           (Index::new_from_parts(s.current, i_p), s) 
        }
    }

    prop_compose!{
        pub fn index(max_index: LengthSize)
           ((i, _) in state_with_index(max_index))
           -> Index {
           i
        }
    }

    prop_compose!{
        pub fn vector_with_valid_index(max_len: LengthSize)
            (vector in proptest::collection::vec(any::<i32>(), 1usize..(max_len as _)))
            (
                (i, s) in state_with_index({ 
                    let len: LengthSize = vector.len().try_into().unwrap();
                    len - 1u32
                }),
                v in Just(vector)
            )
            -> (Index, State, Vec<i32>) {
                (i, s, v)
        }
    }

    pub type MoveTestKit = (Index, State, Vec<i32>, (Index, Index));

    prop_compose!{
        pub fn move_test_kit(max_len: LengthSize)
                            ((index, state, vector) in vector_with_valid_index(max_len))
                            ((s_i1, s_i2) in (0..vector.len(), 0..vector.len()), (i, s, v) in Just((index, state, vector)))
         -> MoveTestKit {
            (i, s, v, (s.new_index_or_max(s_i1), s.new_index_or_max(s_i2)))
        }
    }

    proptest!{    
        #[test]
        fn move_test_kit_always_returns_valid_indexes(
            (previous_index, state, vector, (swap_i1, swap_i2)) in move_test_kit(16)
        ) {
            vector.get(previous_index.get(state).expect("previous_index.get")).expect("vector.get with previous_index");
            vector.get(swap_i1.get(state).expect("swap_i1.get")).expect("vector.get with swap_i1");
            vector.get(swap_i2.get(state).expect("swap_i2.get")).expect("vector.get with swap_i2");
        }
    }

    prop_compose!{
        pub fn selectable_vec1_of_i32(max_len: LengthSize)
                            ((index, state, vector) in vector_with_valid_index(max_len))
                            ((current_index, index_state) in Just((index, state)), elements in Just(vec1::Vec1::try_from_vec(vector).unwrap()))
         -> SelectableVec1<i32> {
            SelectableVec1::from_parts(
                elements,
                index_state,
                current_index,
            )
        }
    }

    arb_enum!{
        pub fn selection_move() -> SelectionMove {
            Left => Just(Left),
            Right => Just(Right),
            ToStart => Just(ToStart),
            ToEnd => Just(ToEnd),
        }
    }

    prop_compose!{
        pub fn selection_moves(max_len: LengthSize)
                (vector in proptest::collection::vec(selection_move(), 0usize..(max_len as _)))
         -> Vec<SelectionMove> {
            vector
        }
    }

    arb_enum!{
        pub fn selection_adjustment() -> SelectionAdjustment {
            Next => Just(Next),
            Previous => Just(Previous),
            Move(_) => selection_move().prop_map(Move),
        }
    }

    prop_compose!{
        pub fn selection_adjustments(max_len: LengthSize)
                (vector in proptest::collection::vec(selection_adjustment(), 0usize..(max_len as _)))
         -> Vec<SelectionAdjustment> {
            vector
        }
    }

    arb_enum!{
        pub fn mut_method_spec(max_index: LengthSize) -> MutMethodSpec<i32> as MutMethodSpec {
            GetCurrentElementMut => Just(GetCurrentElementMut),
            GetCurrentElementOrNoneMut => Just(GetCurrentElementOrNoneMut),
            GetMut(_) => index(max_index).prop_map(GetMut),
            SetCurrentIndex(_) => index(max_index).prop_map(SetCurrentIndex),
            RemoveIfPresent(_) => index(max_index).prop_map(RemoveIfPresent),
            MoveOrIgnore(_, _) => (index(max_index), index(max_index)).prop_map(|(i1, i2)| MoveOrIgnore(i1, i2)),
            SwapOrIgnore(_, _) => (index(max_index), index(max_index)).prop_map(|(i1, i2)| SwapOrIgnore(i1, i2)),
            CloseElement(_) => index(max_index).prop_map(CloseElement),
            AdjustSelection(_) => selection_adjustment().prop_map(AdjustSelection),
            PushAndSelectNew(_) => any::<i32>().prop_map(PushAndSelectNew),
            ReplaceWithMapped(_) => replace_with_mapped(max_index),
        }
    }

    pub fn replace_with_mapped(max_index: LengthSize) -> impl Strategy<Value = MutMethodSpec<i32>> {
        any::<bool>().prop_flat_map(move |is_full| {
            let m_i = max_index as usize;

            if is_full {
                proptest::collection::vec(
                    any::<i32>(),
                    m_i..=m_i,
                )
            } else {
                proptest::collection::vec(
                    any::<i32>(),
                    1..=m_i,
                )
            }
        })
        .prop_map(|v| {
            Vec1::try_from_vec(v).expect("should contain at least one element")
        })
        .prop_map(MutMethodSpec::ReplaceWithMapped)
    }

    prop_compose!{
        pub fn mut_method_specs(max_len: LengthSize)
                (vector in proptest::collection::vec(mut_method_spec(max_len), 0usize..(max_len as _)))
         -> Vec<MutMethodSpec<i32>> {
            vector
        }
    }

    pub fn map_of<S: Strategy>(strat: S, max_len: LengthSize)
     -> impl Strategy<Value = Map<S::Value>> {
        proptest::collection::vec(strat, 0usize..(max_len as _))
            .prop_map(|v| {
                let mut map = Map::with_capacity(Length::or_max(v.len()));
                
                let state: State = d!();
                let mut i = 0;
                // TODO generate non-contigious maps sometimes?
                for e in v {
                    map.insert(state, state.new_index_or_max(i), e);
                    i += 1;
                }
                
                map
            })
    }
}

#[test]
fn the_10_to_14_example_mentioned_in_a_comment_is_accuracte() {
    let initial_vec = vec![10, 11, 12, 13, 14];
    let mut current_vec = initial_vec.clone();

    let mut state: State = d!();

    let index_for_11 = state.new_index_or_max(1);
    // precondition
    assert_eq!(current_vec[index_for_11.get(state).unwrap()], 11);
    
    let index_for_12 = state.new_index_or_max(2);
    // precondition
    assert_eq!(current_vec[index_for_12.get(state).unwrap()], 12);

    let index_for_13 = state.new_index_or_max(3);
    // precondition
    assert_eq!(current_vec[index_for_13.get(state).unwrap()], 13);

    current_vec.remove(2);
    // precondition
    assert_eq!(current_vec, vec![10, 11, 13, 14]);
    state.removed_at(index_for_12);

    assert_eq!(current_vec[index_for_11.get(state).unwrap()], 11);
    assert_eq!(index_for_12.get(state), None);
    assert_eq!(current_vec[index_for_13.get(state).unwrap()], 13);
}

fn every_previous_generation_index_works_after_a_swap_on(
    (previous_index, mut state, mut vector, (swap_i1, swap_i2)): arb::MoveTestKit,
) {
    let previous_value = vector[previous_index.get(state).unwrap()];

    vector.swap(swap_i1.get(state).unwrap(), swap_i2.get(state).unwrap());
    state.swapped_at_or_ignore(swap_i1, swap_i2);

    assert_eq!(
        vector[previous_index.get(state).unwrap()],
        previous_value,
        "{:?} mapped to {:?} which corresponds to {} not {} in {:?}",
        previous_index,
        previous_index.get(state).unwrap(),
        vector[previous_index.get(state).unwrap()],
        previous_value,
        vector,
    );
}

proptest!{
    #[test]
    fn every_previous_generation_index_works_after_a_swap(
        kit in arb::move_test_kit(16),
    ) {
        every_previous_generation_index_works_after_a_swap_on(kit);
    }
}

#[test]
fn every_previous_generation_index_works_after_a_swap_in_this_generated_case() {
    every_previous_generation_index_works_after_a_swap_on((
        d!(),
        d!(),
        vec![506961, 1698395105],
        (Index::new_from_parts(0, IndexPart::or_max(1)), d!())
    ));
}

fn every_previous_generation_index_works_after_a_move_to_on(
    (previous_index, mut state, mut vector, (source, target)): arb::MoveTestKit,
) {
    let previous_value = vector[previous_index.get(state).unwrap()];
    
    {
        let moved_val = vector.remove(source.get(state).unwrap());
        vector.insert(target.get(state).unwrap(), moved_val);
    }
    state.moved_to_or_ignore(source, target);

    let result_index = previous_index.get(state).unwrap();

    assert_eq!(
        vector[result_index],
        previous_value,
        "{:?} mapped to {:?} which corresponds to {} not {} in {:?}",
        previous_index,
        result_index,
        vector[result_index],
        previous_value,
        vector,
    );
}

proptest!{
    #[test]
    fn every_previous_generation_index_works_after_a_move_to(
        kit in arb::move_test_kit(16),
    ) {
        every_previous_generation_index_works_after_a_move_to_on(kit);
    }
}

#[test]
fn every_previous_generation_index_works_after_a_move_to_in_this_reduced_case() {
    every_previous_generation_index_works_after_a_move_to_on(g_i_mtk!(
        g 0 i 1 ri 1 [10, 11] (i 0, i 1)
    ));
}

#[test]
fn every_previous_generation_index_works_after_a_move_to_in_this_generated_case() {
    every_previous_generation_index_works_after_a_move_to_on(g_i_mtk!(
        g 59910 i 8 ri 1 [116927047, -350084188, 1481126173, -1185533403, 1753439307, 1329181082, -2060683224, -1418353247, -890176339, 927573044, -740644675, -1681590353]
        (i 9, i 10)
    ));
}

#[test]
fn every_previous_generation_index_works_after_a_move_to_in_this_differently_reduced_case() {
    every_previous_generation_index_works_after_a_move_to_on(g_i_mtk!(
        g 0 i 2 ri 1 [30, 31, 32, 33, 34]
        (i 3, i 4)
    ));
}

fn no_selection_adjustment_causes_getting_the_current_element_to_return_none_on<A: std::fmt::Debug>(
    mut s_vec1: SelectableVec1<A>,
    adjustments: Vec<SelectionAdjustment>,
) {
    // precondition
    assert!(s_vec1.get_current_element_or_none().is_some(), "precondition failed on {:?}", s_vec1);

    for (i, adjustment) in adjustments.into_iter().enumerate() {
        s_vec1.adjust_selection(adjustment);

        assert!(
            s_vec1.get_current_element_or_none().is_some(),
            "{:?} (index {}) caused get_current_element to return None",
            adjustment,
            i
        );
    }
}

proptest!{
    #[test]
    fn no_selection_adjustment_causes_getting_the_current_element_to_return_none(
        s_vec1 in arb::selectable_vec1_of_i32(16),
        adjustments in arb::selection_adjustments(16),
    ) {
        no_selection_adjustment_causes_getting_the_current_element_to_return_none_on(
            s_vec1,
            adjustments,
        )
    }
}

fn no_selection_adjustment_causes_getting_the_current_element_mut_to_panic_on<A: std::fmt::Debug>(
    mut s_vec1: SelectableVec1<A>,
    adjustments: Vec<SelectionAdjustment>,
) {
    // precondition: this doesn't panic
    let _ = s_vec1.get_current_element_mut();

    for adjustment in adjustments {
        s_vec1.adjust_selection(adjustment);

        // if this doesn't panic, the test passes
        let _ = s_vec1.get_current_element_mut();
    }
}

proptest!{
    #[test]
    fn no_selection_adjustment_causes_getting_the_current_element_mut_to_panic(
        s_vec1 in arb::selectable_vec1_of_i32(16),
        adjustments in arb::selection_adjustments(16),
    ) {
        no_selection_adjustment_causes_getting_the_current_element_mut_to_panic_on(
            s_vec1,
            adjustments,
        )
    }
}

#[test]
fn no_selection_adjustment_causes_getting_the_current_element_to_return_none_in_this_generated_case() {
    let s_vec1 = SelectableVec1::from_parts(
        vec1![12654029],
        g_i_s!(g 39631998 ri 0),
        g_i_i!(g 39631998 i 0),
    );

    let adjustments = g_i_as![Next, Move(ToStart), Move(Right), Next, Move(Right), Next, Next, Move(ToEnd), Next, Previous, Next, Next, Next, Previous];
    
    no_selection_adjustment_causes_getting_the_current_element_to_return_none_on(
        s_vec1,
        adjustments,
    )
}

#[test]
fn no_selection_adjustment_causes_getting_the_current_element_to_return_none_in_this_reduced_case() {
    no_selection_adjustment_causes_getting_the_current_element_to_return_none_on(
        SelectableVec1::new(0),
        g_i_as![Next, Move(ToStart), Move(Right)],
    )
}

#[test]
fn no_selection_adjustment_causes_getting_the_current_element_to_return_none_in_this_two_element_generated_case() {
    let s_vec1 = SelectableVec1::from_parts(
        vec1![919639994, -804550252],
        g_i_s!(g 0 ri 0),
        g_i_i!(g 0 i 1),
    );

    let adjustments = g_i_as![Next, Move(Left), Move(Right), Next, Previous, Move(ToEnd), Move(Right), Previous];
    
    no_selection_adjustment_causes_getting_the_current_element_to_return_none_on(
        s_vec1,
        adjustments,
    )
}

fn no_selection_move_causes_getting_the_current_element_mut_to_return_a_different_element_on(
    mut s_vec1: SelectableVec1<i32>,
    moves: Vec<SelectionMove>,
) {
    let previous_element = *s_vec1.get_current_element_mut();

    for (i, r#move) in moves.into_iter().enumerate() {
        s_vec1.adjust_selection(SelectionAdjustment::Move(r#move));

        assert_eq!(
            *s_vec1.get_current_element_mut(),
            previous_element,
            "{:?} (index {}) caused the current element to change",
            r#move,
            i
        );
    }
}

proptest!{
    #[test]
    fn no_selection_move_causes_getting_the_current_element_mut_to_return_a_different_element(
        s_vec1 in arb::selectable_vec1_of_i32(16),
        moves in arb::selection_moves(16),
    ) {
        no_selection_move_causes_getting_the_current_element_to_return_a_different_element_on(
            s_vec1,
            moves,
        )
    }
}

fn no_selection_move_causes_getting_the_current_element_to_return_a_different_element_on(
    mut s_vec1: SelectableVec1<i32>,
    moves: Vec<SelectionMove>,
) {
    let previous_element = *s_vec1.get_current_element();

    for (i, r#move) in moves.into_iter().enumerate() {
        s_vec1.adjust_selection(SelectionAdjustment::Move(r#move));

        assert_eq!(
            *s_vec1.get_current_element(),
            previous_element,
            "{:?} (index {}) caused the current element to change",
            r#move,
            i
        );
    }
}

proptest!{
    #[test]
    fn no_selection_move_causes_getting_the_current_element_to_return_a_different_element(
        s_vec1 in arb::selectable_vec1_of_i32(16),
        moves in arb::selection_moves(16),
    ) {
        no_selection_move_causes_getting_the_current_element_to_return_a_different_element_on(
            s_vec1,
            moves,
        )
    }
}

#[test]
fn no_selection_move_causes_getting_the_current_element_to_return_a_different_element_in_this_single_right_case() {
    let s_vec1 = SelectableVec1::from_parts(
        vec1![919639994, -804550252],
        g_i_s!(),
        g_i_i!(),
    );

    let moves = g_i_as![Right];

    no_selection_move_causes_getting_the_current_element_to_return_a_different_element_on(
        s_vec1,
        moves,
    )
}

#[test]
fn no_selection_move_causes_getting_the_current_element_to_return_a_different_element_in_this_larger_single_right_case() {
    let s_vec1 = SelectableVec1::from_parts(
        vec1![20, 21, 22, 23, 24],
        g_i_s!(),
        g_i_i!(g 0 i 4),
    );

    let moves = g_i_as![Right];

    no_selection_move_causes_getting_the_current_element_to_return_a_different_element_on(
        s_vec1,
        moves,
    )
}

fn no_mut_method_spec_causes_getting_the_current_element_mut_to_panic_on<A: std::fmt::Debug + Clone>(
    mut s_vec1: SelectableVec1<A>,
    specs: Vec<MutMethodSpec<A>>,
) {
    // precondition: this doesn't panic
    let _ = s_vec1.get_current_element_mut();

    for spec in specs.into_iter() {
        spec.apply(&mut s_vec1);

        // if this doesn't panic, the test passes
        let _ = s_vec1.get_current_element_mut();
    }
}

proptest!{
    #[test]
    fn no_mut_method_spec_causes_getting_the_current_element_mut_to_panic(
        s_vec1 in arb::selectable_vec1_of_i32(16),
        specs in arb::mut_method_specs(16),
    ) {
        no_mut_method_spec_causes_getting_the_current_element_mut_to_panic_on(
            s_vec1,
            specs,
        )
    }
}

#[test]
fn all_the_mut_methods_are_at_least_claimed_to_be_tested() {
    assert_eq!(
        MutMethodSpec::<i32>::method_names(),
        super::selectable_vec1::MUT_METHODS.to_vec()
    );
}

proptest!{
    #[test]
    fn calling_replace_with_mapped_after_each_mut_method_call_allows_indexes_from_the_first_s_vec1_to_be_used_on_the_second(
        mut target in arb::selectable_vec1_of_i32(16),
        specs in arb::mut_method_specs(16),
    ) {
        let mut source = SelectableVec1::new(0);
        source.push_and_select_new(1);
        source.push_and_select_new(2);
        
        target.replace_with_mapped(&source, |&x| x);

        {
            let mut index = source.first_index();
            
            assert_eq!(target.get(index), Some(&0), "precondition failure");

            index = source.next_index_from(index);
            assert_eq!(target.get(index), Some(&1), "precondition failure");

            index = source.next_index_from(index);
            assert_eq!(target.get(index), Some(&2), "precondition failure");
        }

        for spec in specs.into_iter() {
            spec.apply(&mut source);

            target.replace_with_mapped(&source, |&x| x);
        }

        {
            let mut index = source.first_index();
            
            assert_eq!(target.get(index), source.get(index));

            index = source.next_index_from(index);
            assert_eq!(target.get(index), source.get(index));

            index = source.next_index_from(index);
            assert_eq!(target.get(index), source.get(index));
        }
    }
}

fn no_mut_method_spec_causes_an_invalid_current_index_on<A: std::fmt::Debug + Clone>(
    mut s_vec1: SelectableVec1<A>,
    specs: Vec<MutMethodSpec<A>>,
) {
    assert!(
        s_vec1.current_index() < s_vec1.len(),
        "Precondition failure"
    );

    for spec in specs.into_iter() {
        spec.apply(&mut s_vec1);

        assert!(
            s_vec1.current_index() < s_vec1.len(),
            "Precondition failure"
        );
    }
}

proptest!{
    #[test]
    fn no_mut_method_spec_causes_an_invalid_current_index(
        s_vec1 in arb::selectable_vec1_of_i32(16),
        specs in arb::mut_method_specs(16),
    ) {
        no_mut_method_spec_causes_an_invalid_current_index_on(
            s_vec1,
            specs,
        )
    }
}

proptest!{
    #[test]
    fn no_replace_with_mapped_spec_causes_an_invalid_current_index(
        s_vec1 in arb::selectable_vec1_of_i32(16),
        specs in vec(arb::replace_with_mapped(32), 0..16),
    ) {
        no_mut_method_spec_causes_an_invalid_current_index_on(
            s_vec1,
            specs,
        )
    }
}

// This did in fact fail at one point because we left `d!()` there instead of `first_index()`.
proptest!{
    #[test]
    fn iter_with_indexes_returns_indexes_that_match_the_s_vec1s_generation(
        s_vec1 in arb::selectable_vec1_of_i32(16),
    ) {
        let generation = s_vec1.current_index().generation;

        for (index, _) in s_vec1.iter_with_indexes() {
            assert_eq!(index.generation, generation);
        }
    }
}

#[test]
fn map_get_returns_the_expected_values_in_this_found_case() {
    let mut index_state = d!();
    let mut map = Map::with_capacity(Length::or_max(2));
    map.insert(index_state, index_state.new_index_or_max(1), false);

    index_state.moved_to_or_ignore_index_part(
        IndexPart::or_max(1),
        IndexPart::or_max(0)
    );

    map.insert(index_state, index_state.new_index_or_max(0), true);

    assert_eq!(map.get(index_state, index_state.new_index_or_max(0)), Some(&true));
    assert_eq!(map.get(index_state, index_state.new_index_or_max(1)), None);
}

#[test]
fn map_get_returns_the_expected_values_in_this_found_case_reduction() {
    let mut index_state = d!();
    let mut map = Map::with_capacity(Length::or_max(2));
    map.insert(index_state, index_state.new_index_or_max(1), false);

    index_state.moved_to_or_ignore_index_part(
        IndexPart::or_max(1),
        IndexPart::or_max(0)
    );

    assert_eq!(map.get(index_state, index_state.new_index_or_max(0)), Some(&false));
}

fn moving_indexes_never_causes_a_map_to_lose_elements_on(
    mut map: Map<u8>,
    index_pairs: Vec<(Index, Index)>,
) {
    let mut state: State = d!();
    let initial_count = map.len();

    for (from, to) in index_pairs {
        state.moved_to_or_ignore(from, to);
        map.migrate_all(state);

        assert_eq!(
            map.len(),
            initial_count,
            "
            ({:?}, {:?})
            state: {:?},
            map: {:?}",
            from,
            to,
            state,
            map
        );
    }
}

proptest!{
    #[test]
    fn moving_indexes_never_causes_a_map_to_lose_elements(
        map in arb::map_of(any::<u8>(), 16),
        index_pairs in vec((arb::index(16), arb::index(16)), 0..16),
    ) {
        moving_indexes_never_causes_a_map_to_lose_elements_on(
            map,
            index_pairs
        )
    }
}

#[test]
fn moving_indexes_never_causes_a_map_to_lose_elements_in_this_generated_three_element_case() {
    moving_indexes_never_causes_a_map_to_lose_elements_on(
        g_i_map!{ 0: 10, 1: 11, 2: 12 },
        vec![(g_i_i!(g 0 i 0), g_i_i!(g 0 i 1))]
    )
}