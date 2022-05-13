/// This is meant to be the smallest reasonable example, and perhaps a template
/// for other examples/applications.
use window_layer::{TextLayout, TextOrRect, TextSpec, VisualSpec};
use screen_space::{ScreenSpaceRect, ScreenSpaceXY, ssxy};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    const TEXT_SIZE: f32 = 16.0;
    let text_colour = [0.9, 0.9, 0.9, 1.0];

    let mut frame_count: u32 = 0;

    let window_state = window_layer::init::<'_, '_, ()>(
        1.,
        [0.3, 0.3, 0.3, 1.0],
        "hello-world".into()
    )?;

    window_state.run(None, move |event, mut fns| {
        use window_layer::{Event, ElementState, KeyCode};
        match event {
            Event::CloseRequested => fns.quit(),
            Event::RedrawRequested => {
                frame_count = frame_count.wrapping_add(1);
                let dimensions = fns.dimensions();
                let width = f32::from(dimensions.width);
                let height = f32::from(dimensions.height);

                let mut text_and_rects = Vec::with_capacity(16);

                let rate = fns.report_rate().unwrap_or_default();

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

                fns.render(&text_and_rects)
                    .expect("fns.render didn't work");

                fns.loop_sleep_start();
            }
            Event::KeyboardInput {
                state: ElementState::Pressed,
                keycode,
                ..
            } => {
                match keycode {
                    KeyCode::Escape => {
                        fns.quit();
                    },
                    _ => (),
                };
            }
            _ => {}
        }
    });
}
