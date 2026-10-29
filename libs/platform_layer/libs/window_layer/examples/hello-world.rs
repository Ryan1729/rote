/// This is meant to be the smallest reasonable example, and perhaps a template
/// for other examples/applications.
use window_layer_types::{InitOutput, TextLayout, TextOrRect, TextSpec, VisualSpec};
use screen_space::{ScreenSpaceRect, ssxy};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    const TEXT_SIZE: f32 = 16.0;
    let text_colour = [0.9, 0.9, 0.9, 1.0];

    let mut frame_count: u32 = 0;

    let InitOutput {
        state: window_state,
        mut dimensions,
        ..
    } = window_layer::init(
        1.,
        [0.3, 0.3, 0.3, 1.0],
        "hello-world".into(),
        &[TEXT_SIZE],
    )?;

    window_layer::run(window_state, move |event, ref mut handle| {
        use window_layer_types::{Event, ButtonState, KeyboardInput, KeyCode};
        match event {
            Event::CloseRequested => handle.quit(),
            Event::Resized(new_dimensions) => {
                dimensions = new_dimensions;
            }
            Event::RedrawRequested => {
                frame_count = frame_count.wrapping_add(1);

                let width = f32::from(dimensions.width);
                let height = f32::from(dimensions.height);

                let mut text_and_rects = Vec::with_capacity(16);

                let rate = window_layer::report_rate(handle).rate;

                let text = &format!(
                    "Hello world! {rate:.2} FPS on frame {frame_count}"
                );

                text_and_rects.push(TextOrRect::Text(TextSpec {
                    text,
                    size: TEXT_SIZE,
                    spec: VisualSpec {
                        rect: ScreenSpaceRect {
                            min: ssxy!(0.0, height - TEXT_SIZE),
                            max: ssxy!(width, height),
                        },
                        colour: text_colour,
                        ..<_>::default()
                    },
                    layout: TextLayout::Unbounded,
                }));

                window_layer::render(handle, &text_and_rects)
                    .expect("handle.render didn't work");

                handle.loop_sleep();
                handle.loop_start();
            }
            Event::MainEventsCleared => {
                // Queue a RedrawRequested event so we draw the updated view quickly.
                handle.request_redraw();
            }
            Event::KeyboardInput(KeyboardInput {
                state: ButtonState::Pressed,
                keycode,
                ..
            }) => {
                match keycode {
                    KeyCode::Escape => {
                        handle.quit();
                    },
                    _ => (),
                };
            }
            _ => {}
        }
    });
}
