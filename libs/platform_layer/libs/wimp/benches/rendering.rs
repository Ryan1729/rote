use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use glutin::{Api, GlProfile, GlRequest};
use glyph_brush::GlyphBrush;

use editor::{update_and_render, State};
use gl_layer::{RenderExtras, Vertex};
use opengl::{get_glyph_brush, render_buffer_view, FontInfo};
use platform_types::{Input, Move};

fn slipsum_buffer() -> State {
    include_str!("../../../../../text/slipsum.txt").into()
}

fn render_buffer_view_benchmark(c: &mut Criterion) {
    c.bench_function("render_buffer full highlight", |b| {
        b.iter_batched(
            || {
                let (full_highlight_view, _) = update_and_render(
                    &mut slipsum_buffer(),
                    Input::ExtendSelectionForAllCursors(Move::ToBufferEnd),
                );;
                let font_info = FontInfo::new(2.0).unwrap();
                let glyph_brush: GlyphBrush<Vertex> = get_glyph_brush(&font_info);
                (glyph_brush, full_highlight_view, font_info)
            },
            |(glyph_brush, view, font_info)| {
                render_buffer_view(&mut black_box(glyph_brush), &view, &font_info)
            },
            BatchSize::LargeInput,
        )
    });
}

const SCREEN_SIZE: u32 = 2048;

type Window = glutin::WindowedContext<glutin::PossiblyCurrent>;

fn render_frame(
    (gl_state, glyph_brush, window, extras): (
        &mut gl_layer::State,
        &mut GlyphBrush<Vertex>,
        &Window,
        RenderExtras,
    ),
) {
    gl_layer::render(
        black_box(gl_state),
        black_box(glyph_brush),
        SCREEN_SIZE as _,
        SCREEN_SIZE as _,
        extras.clone(),
    )
    .unwrap();
    window.swap_buffers().unwrap();
}

fn get_windowed_context() -> Window {
    let glutin_context = glutin::ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 2)))
        .with_srgb(true)
        .build_windowed(
            glutin::WindowBuilder::new()
                .with_dimensions((SCREEN_SIZE, SCREEN_SIZE).into())
                .with_title(SCREEN_SIZE.to_string()),
            &glutin::EventsLoop::new(),
        )
        .unwrap();

    unsafe { glutin_context.make_current() }.unwrap()
}

fn gl_layer_benchmark(c: &mut Criterion) {
    c.bench_function("gl_layer no highlight", |b| {
        b.iter_batched(
            || {
                let font_info = FontInfo::new(2.0).unwrap();

                let window = get_windowed_context();

                let mut glyph_brush: GlyphBrush<Vertex> = get_glyph_brush(&font_info);

                let mut gl_state =
                    gl_layer::init(&glyph_brush, |symbol| window.get_proc_address(symbol) as _)
                        .unwrap();

                let mut buffer = slipsum_buffer();

                {
                    let (full_highlight_view, _) = update_and_render(
                        &mut buffer,
                        Input::ExtendSelectionForAllCursors(Move::ToBufferEnd),
                    );

                    let extras =
                        render_buffer_view(&mut glyph_brush, &full_highlight_view, &font_info);

                    // fill the glyph cache
                    render_frame((&mut gl_state, &mut glyph_brush, &window, extras))
                }

                let (no_highlight_view, _) =
                    update_and_render(&mut buffer, Input::MoveAllCursors(Move::ToBufferStart));
                let extras = render_buffer_view(&mut glyph_brush, &no_highlight_view, &font_info);

                (gl_state, glyph_brush, window, extras)
            },
            |(mut gl_state, mut glyph_brush, window, extras)| {
                render_frame((&mut gl_state, &mut glyph_brush, &window, extras))
            },
            BatchSize::LargeInput,
        )
    });

    c.bench_function("gl_layer full highlight", |b| {
        b.iter_batched(
            || {
                let font_info = FontInfo::new(2.0).unwrap();

                let window = get_windowed_context();

                let mut glyph_brush: GlyphBrush<Vertex> = get_glyph_brush(&font_info);

                let mut gl_state =
                    gl_layer::init(&glyph_brush, |symbol| window.get_proc_address(symbol) as _)
                        .unwrap();

                let mut buffer = slipsum_buffer();
                let (full_highlight_view, _) = update_and_render(
                    &mut buffer,
                    Input::ExtendSelectionForAllCursors(Move::ToBufferEnd),
                );

                {
                    let extras =
                        render_buffer_view(&mut glyph_brush, &full_highlight_view, &font_info);

                    // fill the glyph cache
                    render_frame((&mut gl_state, &mut glyph_brush, &window, extras))
                }

                {
                    let (no_highlight_view, _) =
                        update_and_render(&mut buffer, Input::MoveAllCursors(Move::ToBufferStart));
                    let extras =
                        render_buffer_view(&mut glyph_brush, &no_highlight_view, &font_info);

                    // remove highlight frame from cache
                    render_frame((&mut gl_state, &mut glyph_brush, &window, extras));
                }

                let extras = render_buffer_view(&mut glyph_brush, &full_highlight_view, &font_info);
                (gl_state, glyph_brush, window, extras)
            },
            |(mut gl_state, mut glyph_brush, window, extras)| {
                render_frame((&mut gl_state, &mut glyph_brush, &window, extras))
            },
            BatchSize::LargeInput,
        )
    });
}

criterion_group!(render_buffer_view_group, render_buffer_view_benchmark);
criterion_group!(gl_layer_group, gl_layer_benchmark);
criterion_main!(render_buffer_view_group, gl_layer_group);
