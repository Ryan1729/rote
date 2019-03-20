// This was originally based on example code for https://github.com/alexheretic/glyph-brush
// To the extent that the code remains as it was (at commit 90e7c7c331e9f991e11de6404b2ca073c0a09e61)
// the code is licensed under the Apache 2.0 license, as described in the license file in this folder.

use glutin::{Api, ContextTrait, GlProfile, GlRequest};
use glyph_brush::{rusttype::Font, *};

pub fn display(update_and_render: platform_types::UpdateAndRender) -> gl::Res<()> {
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
    let mut glyph_brush = GlyphBrushBuilder::using_font(font.clone()).build();

    let mut gl_state = gl::init(&glyph_brush, |symbol| window.get_proc_address(symbol) as _)?;

    let mut text: String = include_str!("text/lipsum.txt").into();
    let font_size: f32 = 11.0;
    let scroll_multiplier: f32 = 16.0;

    let mut loop_helper = spin_sleep::LoopHelper::builder().build_with_target_rate(250.0);
    let mut running = true;
    let mut dimensions = window
        .get_inner_size()
        .ok_or("get_inner_size = None")?
        .to_physical(window.get_hidpi_factor());

    let (mut screen_x, mut screen_y) = (0.0, 0.0);

    while running {
        loop_helper.loop_start();

        events.poll_events(|event| {
            use glutin::*;
            if let Event::WindowEvent { event, .. } = event {
                use platform_types::Input;
                update_and_render(Input::NoInput);
                match event {
                    WindowEvent::CloseRequested => running = false,
                    WindowEvent::Resized(size) => {
                        let dpi = window.get_hidpi_factor();
                        window.resize(size.to_physical(dpi));
                        if let Some(ls) = window.get_inner_size() {
                            dimensions = ls.to_physical(dpi);
                            gl::set_dimensions(dimensions.width as _, dimensions.height as _);
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
                            screen_x = 0.0;
                            screen_y = 0.0;
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
                            text.pop();
                        }
                        _ => (),
                    },
                    WindowEvent::ReceivedCharacter(c) => {
                        if c != '\u{7f}' && c != '\u{8}' {
                            text.push(c);
                        }
                    }
                    WindowEvent::MouseWheel {
                        delta: MouseScrollDelta::LineDelta(_, y),
                        modifiers: ModifiersState { shift: false, .. },
                        ..
                    } => {
                        screen_y -= y * scroll_multiplier;
                    }
                    WindowEvent::MouseWheel {
                        delta: MouseScrollDelta::LineDelta(_, y),
                        modifiers: ModifiersState { shift: true, .. },
                        ..
                    } => {
                        screen_x += y * scroll_multiplier;
                    }
                    _ => {}
                }
            }
        });

        let width = dimensions.width as f32;
        let height = dimensions.height as f32;
        let scale =
            rusttype::Scale::uniform((font_size * window.get_hidpi_factor() as f32).round());
        let line_height = {
            let v_metrics = font.v_metrics(scale);

            v_metrics.ascent + -v_metrics.descent + v_metrics.line_gap
        };

        let status_line_y = height - line_height;

        glyph_brush.queue(Section {
            text: &text,
            scale,
            screen_position: (screen_x, screen_y),
            bounds: (width, status_line_y - screen_y),
            color: [0.3, 0.3, 0.9, 1.0],
            ..Section::default()
        });

        glyph_brush.queue(Section {
            text: &text,
            scale,
            screen_position: (0.0, status_line_y),
            bounds: (width, line_height),
            color: [0.3, 0.9, 0.3, 1.0],
            layout: Layout::default_single_line(),
            ..Section::default()
        });

        gl::render(&mut gl_state, &mut glyph_brush, width as _, height as _)?;

        window.swap_buffers()?;

        if let Some(rate) = loop_helper.report_rate() {
            window.set_title(&format!("{} {:.0} FPS", title, rate));
        }
        loop_helper.loop_sleep();
    }

    gl::cleanup(gl_state)
}
