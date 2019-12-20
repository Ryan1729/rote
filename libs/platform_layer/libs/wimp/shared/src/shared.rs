use macros::d;
use platform_types::g_i;
use std::collections::HashMap;

pub type Res<T> = Result<T, Box<dyn std::error::Error>>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BufferStatus {
    /// The copy on disk at the buffer's path is the same as in memory. Note that we don't
    /// distinguish whether the temp file matches or not. This is becaue the difference would only
    /// let us skip some writes to the temp files, and we currently just write those out every time.
    Unedited,
    /// The copy on disk at the buffer's path is different than in memory but the temp file is the same as in memory
    EditedAndSaved,
    /// The copy on disk at the buffer's path and the temp file are both different than what is in memory
    EditedAndUnSaved,
}
d!(for BufferStatus: BufferStatus::Unedited);

/// A representaion of how a `BufferStatus` can change. Having this be a separate data type allows
/// us to keep all of the reading of a `BufferStatusMap` on a single thread.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BufferStatusTransition {
    /// The copy in memory has changed, so we know that it may not match the what is on disk any longer.
    Edit,
    /// The temp file on disk has been updated to match what is in memory.
    SaveTemp,
    /// The copy on disk at the buffer's path has been updated to match what is in memory.
    Save,
}

pub fn transform_status(status: BufferStatus, transition: BufferStatusTransition) -> BufferStatus {
    use BufferStatus::*;
    use BufferStatusTransition::*;
    match (status, transition) {
        (_, Edit) => EditedAndUnSaved,
        (_, Save) | (Unedited, SaveTemp) => Unedited,
        (EditedAndUnSaved, SaveTemp) | (EditedAndSaved, SaveTemp) => EditedAndSaved,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BufferStatusMap {
    map: HashMap<usize, BufferStatus>,
    last_state: Option<g_i::State>,
}

impl BufferStatusMap {
    pub fn with_capacity(capacity: usize) -> Self {
        BufferStatusMap {
            map: HashMap::with_capacity(capacity),
            last_state: d!(),
        }
    }

    pub fn get(&self, state: g_i::State, index: g_i::Index) -> Option<BufferStatus> {
        index.get(state).and_then(|i| {
            dbg!(i, &self.map);
            self.map.get(&i).cloned()
        })
        // dbg!(state, index);
        // if self.last_state == Some(state) {
        //     dbg!();
        //     index.get(state).and_then(|i| {
        //         dbg!(i, &self.map);
        //         self.map.get(&i).cloned()
        //     })
        // } else if let Some(i) = self.last_state.and_then(|s| {
        //     dbg!();
        //     index
        //         .get(state)
        //         .and_then(|i| s.migrate(state.new_index(g_i::IndexPart::or_max(i))))
        // }) {
        //     dbg!();
        //     i.get(state).and_then(|i| self.map.get(&i).cloned())
        // } else {
        //     dbg!();
        //     None
        // }
    }

    pub fn insert(&mut self, state: g_i::State, current_index: g_i::Index, status: BufferStatus) {
        if let Some(current_index) = current_index.get(state) {
            let last_state = self.last_state;
            if Some(state) != last_state {
                let mut keys: Vec<_> = self.map.keys().cloned().collect();
                //currently all the state fixups work if we use thie reverse order.
                keys.sort();
                keys.reverse();
                for key in keys {
                    if let Some(i) = last_state.and_then(|s| {
                        state
                            .migrate(s.new_index(g_i::IndexPart::or_max(key)))
                            .and_then(|i| i.get(state))
                    }) {
                        let status = self.map.remove(&i).unwrap_or_default();
                        self.map.insert(i, status);
                    } else {
                        self.map.remove(&key);
                    }
                }

                self.last_state = Some(state);
            }

            self.map.insert(current_index, status);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn state_at_generation(generation: g_i::Generation) -> g_i::State {
        let mut s: g_i::State = d!();
        for _ in 0..generation {
            s.removed_at_index_part(g_i::IndexPart::or_max(usize::max_value()));
        }
        s
    }

    #[test]
    fn buffer_status_map_get_works_on_this_example_with_multiple_generations() {
        let state_0: g_i::State = d!();
        let state_1 = {
            let mut s = state_0.clone();
            s.removed_at_index_part(g_i::IndexPart::or_max(2));
            s
        };
        let state_2 = {
            let mut s = state_1.clone();
            s.removed_at_index_part(g_i::IndexPart::or_max(usize::max_value()));
            s
        };
        let len = 5;
        // In the map below, we simulate a map that was like this at generation 0:
        // [status_pre_2, status_pre_2, status_at_2, status_at_2, status_post_2, status_post_2]
        // and in generation 1 it became this:
        // [status_pre_2, status_pre_2, status_at_2, status_post_2, status_post_2]
        let status_pre_2 = BufferStatus::Unedited;
        let status_at_2 = BufferStatus::EditedAndSaved;
        let status_post_2 = BufferStatus::EditedAndUnSaved;

        let mut map = BufferStatusMap::with_capacity(len);
        // this test assumes that the state passed "matches" the maps state, so in this case,
        // always `state_1`. Note the index state is varied. We use state_1 so we can check what
        // happens above, at, and after the current state. Similarly we care about index 2 so we
        // have examples before and after it.
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(0)),
            status_pre_2,
        );
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(1)),
            status_pre_2,
        );
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(2)),
            status_at_2,
        );
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(3)),
            status_post_2,
        );
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(4)),
            status_post_2,
        );

        macro_rules! _0_to_5_get_assert {
            ($index_state: expr =>
                $ex0: expr,
                $ex1: expr,
                $ex2: expr,
                $ex3: expr,
                $ex4: expr,
             ) => {
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(0))),
                    $ex0,
                    "$ex0"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(1))),
                    $ex1,
                    "$ex1"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(2))),
                    $ex2,
                    "$ex2"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(3))),
                    $ex3,
                    "$ex3"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(4))),
                    $ex4,
                    "$ex4"
                );
            };
        }

        _0_to_5_get_assert! {
            state_0
            =>
            Some(status_pre_2),
            Some(status_pre_2),
            None,
            Some(status_at_2),
            Some(status_post_2),
        };

        _0_to_5_get_assert! {
            state_1
            =>
            Some(status_pre_2),
            Some(status_pre_2),
            Some(status_at_2),
            Some(status_post_2),
            Some(status_post_2),
        }

        _0_to_5_get_assert! {
            state_2
            =>
            None,
            None,
            None,
            None,
            None,
        }
    }

    #[test]
    fn buffer_status_map_insert_clears_away_old_generations() {
        let old_state = d!();
        let new_state = state_at_generation(3);
        let status = BufferStatus::EditedAndUnSaved;
        let len = 16;

        let mut map = BufferStatusMap::with_capacity(len);

        for i in 0..len {
            map.insert(
                old_state,
                old_state.new_index(g_i::IndexPart::or_max(i)),
                status,
            );
        }

        // precondition
        assert_eq!(map.map.len(), len);

        map.insert(
            new_state,
            new_state.new_index(g_i::IndexPart::or_max(0)),
            status,
        );

        assert_eq!(map.map.len(), 1);
    }
}
