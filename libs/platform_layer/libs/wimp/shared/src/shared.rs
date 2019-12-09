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

#[derive(Debug)]
pub struct BufferStatusMap {
    map: HashMap<usize, BufferStatus>,
}

impl BufferStatusMap {
    pub fn with_capacity(capacity: usize) -> Self {
        BufferStatusMap {
            map: HashMap::with_capacity(capacity),
        }
    }

    pub fn get(&self, state: g_i::State, index: g_i::Index) -> Option<BufferStatus> {
        index.get(state).and_then(|i| self.map.get(&i).cloned())
    }

    pub fn insert(&mut self, state: g_i::State, index: g_i::Index, status: BufferStatus) {
        if let Some(i) = index.get(state) {
            self.map.insert(i, status);
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
            state_1.new_index(g_i::IndexPart::or_max(0)),
            status_post_2,
        );
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(0)),
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
                    $ex0
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(1))),
                    $ex1
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(2))),
                    $ex2
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(3))),
                    $ex3
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(4))),
                    $ex4
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

    // TODO see if this can be merged with the similar macro below
    macro_rules! does_not_block_assert {
        ($buffer_statuses: ident) => {
            use std::time::{Duration, Instant};
            let start_time = Instant::now();

            const BLOCK_TIME: Duration = Duration::from_millis(10);
            // must be less than one third of BLOCK_TIME
            const NON_BLOCK_PROCESSING_TIME_UNIT: Duration = Duration::from_millis(1);

            let handle = {
                let buffer_statuses_ref = $buffer_statuses.clone();
                std::thread::Builder::new()
                    .name("get_mut_possibly_blocking".to_string())
                    .spawn(move || {
                        let map = buffer_statuses_ref.get_mut_possibly_blocking();
                        std::thread::sleep(BLOCK_TIME);

                        map.insert(d!(), d!(), d!());
                        dbg!(map);
                    })
                    .expect("Could not start get_mut_possibly_blocking thread!")
            };

            let buffer_statuses_ref = $buffer_statuses.clone();
            {
                let map = buffer_statuses_ref.get_mut_without_blocking_for_one_consumer();
                std::thread::sleep(NON_BLOCK_PROCESSING_TIME_UNIT);
                map.insert(d!(), d!(), d!());
                dbg!(map);
            }

            let map = buffer_statuses_ref.get_non_blocking();
            std::thread::sleep(NON_BLOCK_PROCESSING_TIME_UNIT);
            dbg!(map);

            let end_time = Instant::now();

            // assert that the thread did not stop the non-blocking operations from proceeding
            assert!(
                end_time.duration_since(start_time)
                    < BLOCK_TIME
                        .checked_sub(NON_BLOCK_PROCESSING_TIME_UNIT)
                        .unwrap()
            );

            handle.join().unwrap();
        };
    }

    #[test]
    fn does_not_block_in_this_expected_scenario() {
        let buffer_statuses = std::sync::Arc::new(BufferStatuses::new(16));

        does_not_block_assert!(buffer_statuses);
    }

    // meta
    // this test validates that `buffer_statuses_does_not_block_in_this_expected_scenario`
    // actually demonstrates what it purports to.
    #[test]
    fn implemented_with_a_mutex_blocks_inappropriately() {
        #[derive(Debug)]
        pub struct BufferStatuses {
            pub map_mutex: std::sync::Mutex<BufferStatusMap>,
        }

        impl BufferStatuses {
            pub fn new(capacity: usize) -> Self {
                BufferStatuses {
                    map_mutex: std::sync::Mutex::new(BufferStatusMap::with_capacity(capacity)),
                }
            }

            pub fn get_mut_possibly_blocking(&mut self) -> &mut BufferStatusMap {
                self.map_mutex.get_mut().unwrap()
            }

            pub fn get_mut_without_blocking_for_one_consumer(&mut self) -> &mut BufferStatusMap {
                self.map_mutex.get_mut().unwrap()
            }

            pub fn get_non_blocking(&mut self) -> &BufferStatusMap {
                self.map_mutex.get_mut().unwrap()
            }
        }

        let buffer_statuses = std::sync::Arc::new(BufferStatuses::new(16));
        does_not_block_assert!(buffer_statuses);
    }
}

#[derive(Debug)]
pub struct BufferStatuses {
    map: BufferStatusMap,
}

impl BufferStatuses {
    pub fn new(capacity: usize) -> Self {
        BufferStatuses {
            map: BufferStatusMap::with_capacity(capacity),
        }
    }

    pub fn get_mut_possibly_blocking(&self) -> &mut BufferStatusMap {
        unimplemented!()
    }

    pub fn get_mut_without_blocking_for_one_consumer(&self) -> &mut BufferStatusMap {
        unimplemented!()
    }

    pub fn get_non_blocking(&self) -> &BufferStatusMap {
        unimplemented!()
    }
}
