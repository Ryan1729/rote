use macros::d;
use platform_types::g_i;
use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

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

#[derive(Clone, Debug, PartialEq)]
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

    #[cfg(test)]
    fn len(&self) -> usize {
        self.map.len()
    }
}

/// `capacity` indicates capacity of each of the three maps, so the total capacity memory used
/// will be at least three times that number.
pub fn new_buffer_status_handles(
    capacity: usize,
) -> (BufferStatusReadHandle, BufferStatusWriteHandle) {
    let maps = Arc::new(BufferStatusMaps {
        maps: [
            Mutex::new(BufferStatusMap::with_capacity(capacity)),
            Mutex::new(BufferStatusMap::with_capacity(capacity)),
        ],
    });

    (
        BufferStatusReadHandle {
            maps: maps.clone(),
            index: 0,
        },
        BufferStatusWriteHandle { maps },
    )
}

#[derive(Debug)]
struct BufferStatusMaps {
    maps: [Mutex<BufferStatusMap>; 2],
}

/// There should be at most one `BufferStatusWriteHandle` for a given backing store of maps.
/// Therefore this does not an should not implement `Clone`.
#[derive(Debug)]
pub struct BufferStatusWriteHandle {
    maps: Arc<BufferStatusMaps>,
}

impl BufferStatusWriteHandle {
    fn perform_write<F>(&mut self, action: F)
    where
        F: Fn(&mut BufferStatusMap) -> (),
    {
        // Just in case we somehow would loop forever otherwise
        let mut count = 0;
        let mut wrote_to_1 = false;
        let mut wrote_to_2 = false;
        while count < 16 && !(wrote_to_1 && wrote_to_2) {
            if !wrote_to_1 {
                if let Ok(ref mut map) = self.maps.maps[0].try_lock() {
                    action(map);
                    wrote_to_1 = true;
                }
            }
            if !wrote_to_2 {
                if let Ok(ref mut map) = self.maps.maps[1].try_lock() {
                    action(map);
                    wrote_to_2 = true;
                }
            }

            count += 1;
        }
        debug_assert!(count < 16);
    }
}

#[derive(Debug)]
pub struct BufferStatusReadHandle {
    maps: Arc<BufferStatusMaps>,
    index: u8,
}

impl BufferStatusReadHandle {
    fn get(&mut self) -> BufferStatusMap {
        // TODO change the api to take a fn that takes a &BufferStatusMap to avoid the clone?
        if let Ok(read_ref) = self.maps.maps[self.index as usize].try_lock() {
            return (*read_ref).clone();
        }

        self.index += 1;
        self.index &= 1;

        if let Ok(read_ref) = self.maps.maps[self.index as usize].try_lock() {
            return read_ref.clone();
        }

        BufferStatusMap::with_capacity(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::{any, prop_oneof, proptest, Just, Strategy};

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

    #[derive(Default, Debug)]
    struct InsertCommand {
        state: g_i::State,
        index: g_i::Index,
        status: BufferStatus,
    }

    fn arb_buffer_status() -> impl Strategy<Value = BufferStatus> {
        use BufferStatus::*;
        prop_oneof![Just(Unedited), Just(EditedAndSaved), Just(EditedAndUnSaved),]
    }

    // The `State` is guarentted to match
    fn arb_cmd() -> impl Strategy<Value = InsertCommand> {
        (
            platform_types::g_i::arb::state(),
            platform_types::g_i::arb::index_part(),
            arb_buffer_status(),
        )
            .prop_map(|(state, index_part, status)| InsertCommand {
                state,
                index: state.new_index(index_part),
                status,
            })
    }

    macro_rules! does_not_block_assert {
        ($buffer_statuses_r: ident, $buffer_statuses_w: ident) => {
            use std::time::{Duration, Instant};
            let start_time = Instant::now();

            const BLOCK_TIME: Duration = Duration::from_millis(10);
            // must be less than one third of BLOCK_TIME
            const NON_BLOCK_PROCESSING_TIME_UNIT: Duration = Duration::from_millis(1);

            use std::sync::mpsc::channel;
            let (sink, source) = channel();

            let handle = {
                let w_ref = $buffer_statuses_w;
                std::thread::Builder::new()
                    .name("perform_write".to_string())
                    .spawn(move || {
                        let mut w_ref = w_ref;
                        w_ref.perform_write(|map| {
                            std::thread::sleep(BLOCK_TIME);
                            map.insert(d!(), d!(), d!());
                            dbg!(map);
                        });

                        if let Ok(InsertCommand {
                            state,
                            index,
                            status,
                        }) = source.recv()
                        {
                            w_ref.perform_write(|map| {
                                map.insert(state, index, status);
                            });
                        }
                    })
                    .expect("Could not start perform_write thread!")
            };

            {
                std::thread::sleep(NON_BLOCK_PROCESSING_TIME_UNIT);
                let mut cmd: InsertCommand = d!();
                cmd.index = cmd.state.new_index(g_i::IndexPart::or_max(1));
                sink.send(cmd);
            }

            let mut r_ref = $buffer_statuses_r;

            {
                let map = r_ref.get();
                std::thread::sleep(NON_BLOCK_PROCESSING_TIME_UNIT);
                dbg!(map);
            }

            let end_time = Instant::now();

            // assert that the thread did not stop the non-blocking operations from proceeding
            assert!(
                end_time.duration_since(start_time)
                    < BLOCK_TIME
                        .checked_sub(NON_BLOCK_PROCESSING_TIME_UNIT)
                        .unwrap()
            );

            handle.join().unwrap();

            let map = r_ref.get();

            // assert that all the writes eventually succeeded
            assert!(map.len() == 2);
        };
    }

    #[test]
    fn buffer_status_handles_do_not_block_in_this_expected_scenario() {
        let (r, w) = new_buffer_status_handles(16);
        does_not_block_assert!(r, w);
    }

    #[derive(Debug)]
    enum EditAction {
        UI(EditSpec),
        Background(EditSpec),
        Read(u8),
    }

    #[derive(Debug)]
    struct EditSpec {
        cmd: InsertCommand,
        wait_ns: u8,
    }

    fn arb_edit_spec() -> impl Strategy<Value = EditSpec> {
        (arb_cmd(), any::<u8>()).prop_map(|(cmd, wait_ns)| EditSpec { cmd, wait_ns })
    }

    fn arb_edit() -> impl Strategy<Value = EditAction> {
        prop_oneof![
            any::<u8>().prop_map(EditAction::Read),
            arb_edit_spec().prop_map(EditAction::UI),
            arb_edit_spec().prop_map(EditAction::Background),
        ]
    }

    fn arb_edits(len: usize) -> impl Strategy<Value = Vec<EditAction>> {
        proptest::collection::vec(arb_edit(), len)
    }

    macro_rules! eventually_consistent_assert {
        ($edits: expr) => {{
            let edits = $edits;
            use std::thread::sleep;
            use std::time::Duration;
            let mut expected: BufferStatusMap = BufferStatusMap::with_capacity(edits.len());
            for edit in edits.iter() {
                use EditAction::*;
                match edit {
                    Read(_) => {}
                    Background(EditSpec {
                        cmd:
                            InsertCommand {
                                state,
                                index,
                                status,
                            },
                        ..
                    })
                    | UI(EditSpec {
                        cmd:
                            InsertCommand {
                                state,
                                index,
                                status,
                            },
                        ..
                    }) => {
                        expected.insert(*state, *index, *status);
                    }
                };
            }

            use std::sync::mpsc::channel;
            let (background_sink, background_source) = channel();
            let (ui_sink, ui_source) = channel();
            let (ui_to_background_sink, ui_to_background_source) = channel();

            let (mut r_ref, w_ref) = new_buffer_status_handles(edits.len());

            let ui_handle = std::thread::Builder::new()
                .spawn(move || {
                    while let Ok(cmd) = ui_source.recv() {
                        if let Some(EditSpec { cmd, wait_ns }) = cmd {
                            sleep(Duration::from_nanos(wait_ns as _));
                            ui_to_background_sink
                                .send(EditSpec { cmd, wait_ns: 0 })
                                .unwrap();
                        } else {
                            break;
                        }
                    }
                })
                .expect("Could not start thread!");

            let background_handle = std::thread::Builder::new()
                .spawn(move || {
                    let mut w_ref = w_ref;
                    loop {
                        if let Ok(EditSpec {
                            cmd:
                                InsertCommand {
                                    state,
                                    index,
                                    status,
                                },
                            wait_ns,
                        }) = ui_to_background_source.try_recv()
                        {
                            w_ref.perform_write(|map| {
                                sleep(Duration::from_nanos(wait_ns as _));
                                map.insert(state, index, status);
                            });
                        }

                        if let Ok(cmd) = background_source.try_recv() {
                            if let Some(EditSpec {
                                cmd:
                                    InsertCommand {
                                        state,
                                        index,
                                        status,
                                    },
                                wait_ns,
                            }) = cmd
                            {
                                w_ref.perform_write(|map| {
                                    sleep(Duration::from_nanos(wait_ns as _));
                                    map.insert(state, index, status);
                                });
                            } else {
                                break;
                            }
                        }
                    }
                })
                .expect("Could not start thread!");

            for edit in edits {
                use EditAction::*;
                match edit {
                    Read(wait_ns) => {
                        let map = r_ref.get();
                        sleep(Duration::from_nanos(wait_ns as _));
                        dbg!(map);
                    }
                    UI(spec) => ui_sink.send(Some(spec)).unwrap(),
                    Background(spec) => background_sink.send(Some(spec)).unwrap(),
                };
            }

            sleep(Duration::from_nanos(1000 as _));

            ui_sink.send(None).unwrap();
            ui_handle.join().unwrap();

            background_sink.send(None).unwrap();
            background_handle.join().unwrap();

            // assert that all the writes eventually succeeded
            let mut map = r_ref.get();
            for _ in 0..8 {
                if map == expected {
                    break;
                }
                map = r_ref.get();
            }
            assert_eq!(map, expected);
        }};
    }

    proptest! {
        #[test]
        fn writes_to_buffer_status_handles_are_eventually_consistent(
            edits in arb_edits(16)
        ) {
            eventually_consistent_assert!(edits)
        }
    }

    #[test]
    fn writes_to_buffer_status_handles_are_eventually_consistent_in_this_reduced_case() {
        use BufferStatus::*;
        use EditAction::*;
        // Repeat to deal with this working accidentally sometimes
        for _ in 0..10 {
            eventually_consistent_assert!(vec![
                UI(EditSpec {
                    cmd: InsertCommand {
                        state: g_i::State::new_removed_at(10, g_i::IndexPart::or_max(5)),
                        index: g_i::Index::new_from_parts(10, g_i::IndexPart::or_max(0)),
                        status: Unedited,
                    },
                    wait_ns: 0,
                }),
                UI(EditSpec {
                    cmd: InsertCommand {
                        state: g_i::State::new_removed_at(10, g_i::IndexPart::or_max(5)),
                        index: g_i::Index::new_from_parts(10, g_i::IndexPart::or_max(1)),
                        status: EditedAndSaved,
                    },
                    wait_ns: 0,
                }),
                Background(EditSpec {
                    cmd: InsertCommand {
                        state: g_i::State::new_removed_at(10, g_i::IndexPart::or_max(5)),
                        index: g_i::Index::new_from_parts(10, g_i::IndexPart::or_max(2)),
                        status: EditedAndUnSaved,
                    },
                    wait_ns: 0,
                }),
            ])
        }
    }

    #[test]
    fn writes_to_buffer_status_handles_are_eventually_consistent_in_this_case() {
        use BufferStatus::*;
        use EditAction::*;
        // Repeat to deal with this working accidentally sometimes
        for _ in 0..10 {
            eventually_consistent_assert!(vec![
                Read(0),
                Read(0),
                Read(128),
                Read(10),
                UI(EditSpec {
                    cmd: InsertCommand {
                        state: g_i::State::new_removed_at(
                            1592203,
                            g_i::IndexPart::or_max(11682499)
                        ),
                        index: g_i::Index::new_from_parts(1592203, g_i::IndexPart::or_max(86)),
                        status: Unedited,
                    },
                    wait_ns: 19,
                }),
                Read(19),
                Read(7),
                UI(EditSpec {
                    cmd: InsertCommand {
                        state: g_i::State::new_removed_at(2709657, g_i::IndexPart::or_max(4)),
                        index: g_i::Index::new_from_parts(
                            2709657,
                            g_i::IndexPart::or_max(2388572503)
                        ),
                        status: Unedited,
                    },
                    wait_ns: 0,
                }),
                Read(0),
                UI(EditSpec {
                    cmd: InsertCommand {
                        state: g_i::State::new_removed_at(1174, g_i::IndexPart::or_max(3509)),
                        index: g_i::Index::new_from_parts(1174, g_i::IndexPart::or_max(1604)),
                        status: Unedited,
                    },
                    wait_ns: 0,
                }),
                UI(EditSpec {
                    cmd: InsertCommand {
                        state: g_i::State::new_removed_at(
                            31018491,
                            g_i::IndexPart::or_max(35019205)
                        ),
                        index: g_i::Index::new_from_parts(
                            31018491,
                            g_i::IndexPart::or_max(24714586)
                        ),
                        status: EditedAndSaved,
                    },
                    wait_ns: 9,
                }),
                Background(EditSpec {
                    cmd: InsertCommand {
                        state: g_i::State::new_removed_at(
                            77350305,
                            g_i::IndexPart::or_max(1593500352)
                        ),
                        index: g_i::Index::new_from_parts(
                            77350305,
                            g_i::IndexPart::or_max(3429944534)
                        ),
                        status: EditedAndUnSaved,
                    },
                    wait_ns: 37,
                }),
                UI(EditSpec {
                    cmd: InsertCommand {
                        state: g_i::State::new_removed_at(
                            4109276931,
                            g_i::IndexPart::or_max(271399484),
                        ),
                        index: g_i::Index::new_from_parts(
                            4109276931,
                            g_i::IndexPart::or_max(383778964),
                        ),
                        status: EditedAndSaved,
                    },
                    wait_ns: 196,
                }),
                Read(225),
                Read(137),
                Read(37),
            ])
        }
    }

    // this was reduced on an implementation we tried before
    #[test]
    fn writes_to_buffer_status_handles_are_eventually_consistent_in_this_previously_reduced_case() {
        use BufferStatus::*;
        use EditAction::*;

        eventually_consistent_assert!(vec![
            UI(EditSpec {
                cmd: InsertCommand {
                    state: g_i::State::new_removed_at(0, g_i::IndexPart::or_max(0)),
                    index: g_i::Index::new_from_parts(0, g_i::IndexPart::or_max(1)),
                    status: Unedited,
                },
                wait_ns: 0,
            }),
            Read(1),
            Read(0),
        ]);
    }
}

/// This will completely change later
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
