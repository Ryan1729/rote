use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};

use editor::{update_and_render, State};
use platform_types::{Input, Move};

fn slipsum_buffer() -> State {
    include_str!("../../../text/slipsum.txt").into()
}

fn cursor_movement_and_selection_benchmark(c: &mut Criterion) {
    c.bench_function("repeated movement and selection", |b| {
        b.iter_batched(
            || slipsum_buffer(),
            |mut state| {
                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::ToBufferEnd)),
                );

                update_and_render(&mut state, black_box(Input::MoveAllCursors(Move::Left)));
                update_and_render(&mut state, black_box(Input::MoveAllCursors(Move::Left)));
                update_and_render(&mut state, black_box(Input::MoveAllCursors(Move::Left)));
                update_and_render(&mut state, black_box(Input::MoveAllCursors(Move::Left)));

                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Right)),
                );
                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Right)),
                );
                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Right)),
                );
                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Right)),
                );

                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Left)),
                );
                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Left)),
                );
                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Left)),
                );
                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Left)),
                );

                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Right)),
                );
                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Right)),
                );
                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Right)),
                );
                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::Right)),
                );

                update_and_render(
                    &mut state,
                    black_box(Input::ExtendSelectionForAllCursors(Move::ToBufferStart)),
                );
            },
            BatchSize::LargeInput,
        )
    });
}

criterion_group!(
    cursor_movement_and_selection,
    cursor_movement_and_selection_benchmark
);
criterion_main!(cursor_movement_and_selection);
