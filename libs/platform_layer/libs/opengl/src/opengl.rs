// This was originally based on example code for https://github.com/alexheretic/glyph-brush
// the code is licensed under the Apache 2.0 license, as described in the license file in this folder.
// To the extent that the code remains as it was (at commit 90e7c7c331e9f991e11de6404b2ca073c0a09e61)

use glutin::{Api, ContextTrait, GlProfile, GlRequest};
use glyph_brush::{rusttype::Font, *};

use platform_types::{BufferView, Input, Sizes, UpdateAndRender};

pub fn run(update_and_render: UpdateAndRender) -> gl_layer::Res<()> {
    if cfg!(target_os = "linux") {
        use std::env;
        // winit wayland is currently still wip
        if env::var("WINIT_UNIX_BACKEND").is_err() {
            env::set_var("WINIT_UNIX_BACKEND", "x11");
        }
        // disables vsync sometimes on x11
        if env::var("vblank_mode").is_err() {
            env::set_var("vblank_mode", "0");
        }
    }

    let mut events = glutin::EventsLoop::new();
    let title = "rote";

    let window = glutin::WindowedContext::new_windowed(
        glutin::WindowBuilder::new()
            .with_dimensions((1024, 576).into())
            .with_title(title),
        glutin::ContextBuilder::new()
            .with_gl_profile(GlProfile::Core)
            .with_gl(GlRequest::Specific(Api::OpenGl, (3, 2)))
            .with_srgb(true),
        &events,
    )?;
    unsafe { window.make_current()? };

    let font_bytes: &[u8] = include_bytes!("./fonts/FantasqueSansMono-Regular.ttf");
    let font: Font<'static> = Font::from_bytes(font_bytes)?;
    let font_size: f32 = 11.0;
    let scroll_multiplier: f32 = 16.0;

    let scale = rusttype::Scale::uniform((font_size * window.get_hidpi_factor() as f32).round());

    let mut glyph_brush = GlyphBrushBuilder::using_font(font.clone()).build();

    let mut gl_state = gl_layer::init(&glyph_brush, |symbol| window.get_proc_address(symbol) as _)?;

    let mut loop_helper = spin_sleep::LoopHelper::builder().build_with_target_rate(250.0);
    let mut running = true;
    let mut dimensions = window
        .get_inner_size()
        .ok_or("get_inner_size = None")?
        .to_physical(window.get_hidpi_factor());

    let (mut view, mut cmd) = update_and_render(Input::SetSizes(Sizes! {
        screen_w: dimensions.width as f32,
        screen_h: dimensions.height as f32,
        char_w: {
            // We currently assume the font is monospaced.
            let em_space_char = '\u{2003}';
            let h_metrics = font.glyph(em_space_char).scaled(scale).h_metrics();

            h_metrics.advance_width
        },
        line_h: {
            let v_metrics = font.v_metrics(scale);

            v_metrics.ascent + -v_metrics.descent + v_metrics.line_gap
        }
    }));

    let block_width = {
        let full_block_char = '█';
        let h_metrics = font.glyph(full_block_char).scaled(scale).h_metrics();

        h_metrics.advance_width
    };

    while running {
        loop_helper.loop_start();

        events.poll_events(|event| {
            use glutin::*;
            if let Event::WindowEvent { event, .. } = event {
                macro_rules! call_u_and_r {
                    ($input:expr) => {
                        let (v, c) = update_and_render($input);
                        view = v;
                        cmd = c;
                    };
                }

                use platform_types::Move;
                match event {
                    WindowEvent::CloseRequested => running = false,
                    WindowEvent::Resized(size) => {
                        let dpi = window.get_hidpi_factor();
                        window.resize(size.to_physical(dpi));
                        if let Some(ls) = window.get_inner_size() {
                            dimensions = ls.to_physical(dpi);
                            call_u_and_r!(Input::SetSizes(Sizes! {
                                screen_w: dimensions.width as f32,
                                screen_h: dimensions.height as f32,
                                char_w: None,
                                line_h: None,
                            }));
                            gl_layer::set_dimensions(dimensions.width as _, dimensions.height as _);
                        }
                    }
                    WindowEvent::KeyboardInput {
                        input:
                            KeyboardInput {
                                state: ElementState::Pressed,
                                virtual_keycode: Some(keypress),
                                modifiers: ModifiersState { ctrl: true, .. },
                                ..
                            },
                        ..
                    } => match keypress {
                        VirtualKeyCode::Key0 => {
                            call_u_and_r!(Input::ResetScroll);
                        }
                        VirtualKeyCode::Home => {
                            call_u_and_r!(Input::MoveAllCursors(Move::ToBufferStart));
                        }
                        VirtualKeyCode::End => {
                            call_u_and_r!(Input::MoveAllCursors(Move::ToBufferEnd));
                        }
                        _ => (),
                    },
                    WindowEvent::KeyboardInput {
                        input:
                            KeyboardInput {
                                state: ElementState::Pressed,
                                virtual_keycode: Some(keypress),
                                modifiers: ModifiersState { ctrl: false, .. },
                                ..
                            },
                        ..
                    } => match keypress {
                        VirtualKeyCode::Escape => running = false,
                        VirtualKeyCode::Back => {
                            call_u_and_r!(Input::Delete);
                        }
                        VirtualKeyCode::Up => {
                            call_u_and_r!(Input::MoveAllCursors(Move::Up));
                        }
                        VirtualKeyCode::Down => {
                            call_u_and_r!(Input::MoveAllCursors(Move::Down));
                        }
                        VirtualKeyCode::Left => {
                            call_u_and_r!(Input::MoveAllCursors(Move::Left));
                        }
                        VirtualKeyCode::Right => {
                            call_u_and_r!(Input::MoveAllCursors(Move::Right));
                        }
                        VirtualKeyCode::Home => {
                            call_u_and_r!(Input::MoveAllCursors(Move::ToLineStart));
                        }
                        VirtualKeyCode::End => {
                            call_u_and_r!(Input::MoveAllCursors(Move::ToLineEnd));
                        }
                        _ => (),
                    },
                    WindowEvent::ReceivedCharacter(c) => {
                        if c != '\u{7f}' && c != '\u{8}' {
                            call_u_and_r!(Input::Insert(c));
                        }
                    }
                    WindowEvent::MouseWheel {
                        delta: MouseScrollDelta::LineDelta(_, y),
                        modifiers: ModifiersState { shift: false, .. },
                        ..
                    } => {
                        call_u_and_r!(Input::ScrollVertically(y * scroll_multiplier));
                    }
                    WindowEvent::MouseWheel {
                        delta: MouseScrollDelta::LineDelta(_, y),
                        modifiers: ModifiersState { shift: true, .. },
                        ..
                    } => {
                        call_u_and_r!(Input::ScrollHorizontally(y * scroll_multiplier));
                    }
                    _ => {}
                }
            }
        });

        for &BufferView {
            kind,
            bounds,
            color,
            ref chars,
            screen_position,
        } in &view.buffers
        {
            use platform_types::BufferViewKind;

            // Without a background the edit buffer(s) show through the status line(s)
            if let BufferViewKind::StatusLine = kind {
                let width = bounds.0;
                let count = (width / block_width.floor()) + 1.0;

                let x = screen_position.0;
                for i in 0..count as u64 {
                    glyph_brush.queue(Section {
                        text: "█",
                        scale,
                        screen_position: (x + (i as f32 * block_width.floor()), screen_position.1),
                        bounds,
                        color: [7.0 / 256.0, 7.0 / 256.0, 7.0 / 256.0, 1.0],
                        layout: Layout::default_single_line(),
                        ..Section::default()
                    });
                }
            }

            glyph_brush.queue(Section {
                text: chars,
                scale,
                screen_position,
                bounds,
                color,
                layout: match kind {
                    BufferViewKind::Edit => Layout::default_wrap(),
                    BufferViewKind::StatusLine | BufferViewKind::Cursor => {
                        Layout::default_single_line()
                    }
                },
                ..Section::default()
            });
        }

        let width = dimensions.width as u32;
        let height = dimensions.height as f32;

        gl_layer::render(&mut gl_state, &mut glyph_brush, width as _, height as _)?;

        window.swap_buffers()?;

        if let Some(rate) = loop_helper.report_rate() {
            window.set_title(&format!("{} {:.0} FPS", title, rate));
        }
        loop_helper.loop_sleep();
    }

    gl_layer::cleanup(gl_state)
}
