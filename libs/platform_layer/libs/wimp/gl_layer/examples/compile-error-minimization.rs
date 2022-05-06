/// This is meant to be the smallest reasonable example, and perhaps a template
/// for other examples/applications.
use glutin_wrapper::{Api, GlProfile, GlRequest};
use shared::Res;

pub fn takes_dyn(
    _: &dyn Fn(),
) {
}

fn main() -> Res<()> {
    let events = glutin_wrapper::event_loop::EventLoop::new();
    let glutin_wrapper_context = glutin_wrapper::ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
        .with_srgb(true)
        .with_depth_buffer(24)
        .build_windowed(
            glutin_wrapper::window::WindowBuilder::new()
                .with_inner_size(
                    glutin_wrapper::dpi::Size::Logical(glutin_wrapper::dpi::LogicalSize::new(683.0, 393.0))
                )
                .with_title("compile error minimization"),
            &events,
        )?;
    let glutin_wrapper_context = unsafe { glutin_wrapper_context.make_current().map_err(|(_, e)| e)? };

    type LoadFn = dyn Fn();
    let load_fn: &LoadFn = &|| { &glutin_wrapper_context; };

    takes_dyn(load_fn);

    {
        events.run(move |event, _, control_flow| {
            use glutin_wrapper::event::*;

            match event {
                Event::MainEventsCleared => {
                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    glutin_wrapper_context.window().request_redraw();
                }
                Event::WindowEvent { event, .. } => {
                    macro_rules! quit {
                        () => {{
                            *control_flow = glutin_wrapper::event_loop::ControlFlow::Exit;
                        }};
                    }

                    match event {
                        WindowEvent::CloseRequested => quit!(),
                        _ => {}
                    }
                }
                _ => {}
            }
        });
    }
}
