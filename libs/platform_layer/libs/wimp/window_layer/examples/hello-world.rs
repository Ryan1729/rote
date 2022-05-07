/// This is meant to be the smallest reasonable example, and perhaps a template
/// for other examples/applications.
use window_layer::{TextLayout, TextOrRect, TextSpec, VisualSpec};
use platform_types::screen_positioning::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    const TEXT_SIZE: f32 = 16.0;
    let text_colour = [0.9, 0.9, 0.9, 1.0];

    let mut frame_count: u32 = 0;

    let mut window_state = window_layer::init::<'_, ()>(
        1.,
        [0.3, 0.3, 0.3, 1.0],
    )?;

    window_state.run(move |event, mut fns| {
        use window_layer::{Event, ElementState, KeyCode};
        match event {
            Event::RedrawRequested => {
                frame_count = frame_count.wrapping_add(1);
                let dimensions = fns.dimensions();
                let width = dimensions.0 as f32;
                let height = dimensions.1 as f32;

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
                    .expect("window_layer::render didn't work");
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
