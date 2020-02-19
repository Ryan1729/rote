// This was originally based on example code for https://github.com/alexheretic/glyph-brush
// the code is licensed under the Apache 2.0 license, as described in the license file in the
// opengl folder, to the extent that the code remains as it was
// (at commit 90e7c7c331e9f991e11de6404b2ca073c0a09e61)

use glutin::{dpi::LogicalPosition, Api, GlProfile, GlRequest};
use std::time::Duration;

use macros::{c, d};
use platform_types::{screen_positioning::screen_to_text_box, *};
use shared::{Res};

mod clipboard_layer {
    pub use clipboard::ClipboardProvider;
    use shared::Res;
    /// This enum exists so we can do dynamic dispatch on `ClipboardProvider` instances even though
    /// the trait requires `Sized`. The reason  we want to do that, is so that if we try to run this
    /// on a platform where `clipboard::ClipboardContext::new` retirns an `Err` we can continue
    /// operation, just without system clipboard support.
    pub enum Clipboard {
        System(clipboard::ClipboardContext),
        Fallback(clipboard::nop_clipboard::NopClipboardContext),
    }

    impl clipboard::ClipboardProvider for Clipboard {
        fn new() -> Res<Self> {
            let result: Result<
                clipboard::ClipboardContext,
                clipboard::nop_clipboard::NopClipboardContext,
            > = clipboard::ClipboardContext::new().map_err(|err| {
                eprintln!("System clipboard not supported. {}", err);
                // `NopClipboardContext::new` always returns an `Ok`
                clipboard::nop_clipboard::NopClipboardContext::new().unwrap()
            });

            let output = match result {
                Ok(ctx) => Clipboard::System(ctx),
                Err(ctx) => Clipboard::Fallback(ctx),
            };

            // `get_clipboard` currently relies on this neer returning `Err`.
            Ok(output)
        }
        fn get_contents(&mut self) -> Res<String> {
            match self {
                Clipboard::System(ctx) => ctx.get_contents(),
                Clipboard::Fallback(ctx) => ctx.get_contents(),
            }
        }
        fn set_contents(&mut self, s: String) -> Res<()> {
            match self {
                Clipboard::System(ctx) => ctx.set_contents(s),
                Clipboard::Fallback(ctx) => ctx.set_contents(s),
            }
        }
    }

}
#[perf_viz::record]
pub fn run(_: UpdateAndRender) -> Res<()> {

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

    let title = "dissection";

    let events = glutin::event_loop::EventLoop::new();

    let glutin_context = glutin::ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
        .with_srgb(true)
        .with_depth_buffer(24)
        .build_windowed(
            glutin::window::WindowBuilder::new()
                .with_inner_size(
                    glutin::dpi::Size::Logical(glutin::dpi::LogicalSize::new(1024.0, 576.0))
                 )
                .with_title(title),
            &events,
        )?;
    let glutin_context = unsafe { glutin_context.make_current().map_err(|(_, e)| e)? };

    let mut current_hidpi_factor = 1.0;
    macro_rules! get_hidpi_factor {
        () => {
            current_hidpi_factor
        }
    }

pub const TEXT_SIZES: [f32; 1] = [72.0];

    let (mut gl_state, char_dims) = gl_layer::init(
        get_hidpi_factor!() as f32,
        &TEXT_SIZES,
        c![0x22 as f32  / 255.0, 0x22 as f32  / 255.0, 0x22 as f32  / 255.0],
        |symbol| glutin_context.get_proc_address(symbol) as _,
    )?;

    const TARGET_RATE: f64 = 128.0; //250.0);

    let mut loop_helper = spin_sleep::LoopHelper::builder().build_with_target_rate(TARGET_RATE);

    let mut running = true;
    let mut dimensions = glutin_context
        .window()
        .inner_size();

    macro_rules! screen_wh {
        () => {
            ScreenSpaceWH {
                w: dimensions.width as f32,
                h: dimensions.height as f32,
            }
        };
    }

#[derive(Debug, Default)]
pub struct UIState {
    /// This is should be in the range [0.0, 2.0]. This needs the extra space to repesent the down
    /// side of the sawtooth pattern.
    pub fade_alpha_accumulator: f32,
    pub window_is_focused: bool,
}


    let mut ui: UIState = d!();

    let mut dt = Duration::from_nanos(((1.0 / TARGET_RATE) * 1_000_000_000.0) as u64);

    {
        macro_rules! call_u_and_r {
            ($input:expr) => {
            };
        }

        events.run(move |event, _, control_flow| {
            use glutin::event::*;

            match event {
                Event::MainEventsCleared if running => {
                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    glutin_context.window().request_redraw();
                }
                Event::RedrawRequested(_) => {
use gl_layer::{TextLayout, TextSpec, VisualSpec};
        let offset = ((dt.as_millis() as u64 as f32) / 1000.0) * 1.5;

            ui.fade_alpha_accumulator += offset;
            ui.fade_alpha_accumulator = ui.fade_alpha_accumulator.rem_euclid(2.0);

        let fade_alpha = if ui.fade_alpha_accumulator > 1.0 {
                2.0 - ui.fade_alpha_accumulator
            } else {
                ui.fade_alpha_accumulator
            };
    
        let mut text_or_rects =
            Vec::with_capacity(1);

        let edit_buffer_text_rect: ScreenSpaceRect = ScreenSpaceRect {
            min: (
                0.0,
                28.0,
            ),
            max: (
                1024.0,
                550.0,
            ),
        };

        text_or_rects.push(TextSpec {
            text: "▏text_or_rects ▏",
            size: 72.0,
            layout: TextLayout::WrapInRect(edit_buffer_text_rect),
            spec: VisualSpec {
                rect: edit_buffer_text_rect,
                color: c![0xde as f32 / 255.0, 0x49 as f32 / 255.0, 0x49 as f32 / 255.0, fade_alpha],
                z: 32768,
            },
        });
    

    if !ui.window_is_focused {
        dbg!("should be white");
        for t in text_or_rects.iter_mut() {
            t.spec.color = [
                t.spec.color[0],
                t.spec.color[0],
                t.spec.color[0],
                t.spec.color[3],
            ];
        }
    } else {
        dbg!("should be red");
    }

                    let width = dimensions.width;
                    let height = dimensions.height;

                    gl_layer::render(&mut gl_state, text_or_rects, width as _, height as _)
                        .expect("gl_layer::render didn't work");

                    glutin_context
                        .swap_buffers()
                        .expect("swap_buffers didn't work!");

                    perf_viz::start_record!("sleepin'");
                    loop_helper.loop_sleep();
                    perf_viz::end_record!("sleepin'");

                    perf_viz::end_record!("main loop");
                    perf_viz::start_record!("main loop");

                    // We want to track the time that the message loop takes too!
                    dt = loop_helper.loop_start();
                }
                Event::NewEvents(StartCause::Init) => {
                    // At least try to measure the first frame accurately
                    perf_viz::start_record!("main loop");
                    dt = loop_helper.loop_start();
                }
                Event::WindowEvent { event, .. } => {
                    macro_rules! quit {
                        () => {{
                            perf_viz::end_record!("main loop");
                            call_u_and_r!(Input::Quit);

                            running = false;

                            perf_viz::output!();

                            let _ = gl_layer::cleanup(&gl_state);

                            *control_flow = glutin::event_loop::ControlFlow::Exit;
                        }};
                    }
                    
                    // As of this writing, issues on https://github.com/rust-windowing/winit ,
                    // specifically #1124 and #883, suggest that the it is up in the air as to
                    // whether the modifiers field on some of the matches below will actually
                    // be eventually removed or not. So, in the meantime, I choose the path 
                    // that is the least work right now, since it seems unlikely for the amount
                    // of work it will be later to grow significantly. Time will tell.
                    #[allow(deprecated)]
                    match event {
                        WindowEvent::CloseRequested => quit!(),
                        WindowEvent::ScaleFactorChanged {
                            scale_factor,
                            ..
                        } => {
                            current_hidpi_factor = scale_factor;
                        }
                        WindowEvent::Resized(size) => {
                            let hidpi_factor = get_hidpi_factor!();
                            glutin_context.resize(size);
                            dimensions = size;
                            call_u_and_r!(Input::SetSizeDependents(
                                get_non_font_size_dependents!(view.menu.get_mode())
                            ));
                            gl_layer::set_dimensions(
                                &mut gl_state,
                                hidpi_factor as _,
                                (dimensions.width as _, dimensions.height as _),
                            );
                        }
                        WindowEvent::Focused(is_focused) => {
                            dbg!("set to ", is_focused);
                            ui.window_is_focused = is_focused;
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        })
    }
}
