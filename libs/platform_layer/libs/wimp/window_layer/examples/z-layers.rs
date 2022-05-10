/// This is an example/test demonstrating that all of the 2^16 = 65536 values for z that this crate
/// allows, actually work. This might seems like a silly thing to have to test, but GPUs and OpenGL
/// are silly things. Besides, this should be a nice simple example of how to setup a simple
/// scene with this crate.
use window_layer::{TextLayout, TextOrRect, TextSpec, VisualSpec};
use screen_space::{ScreenSpaceRect, ScreenSpaceXY, ssxy};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    const TEXT_SIZE: f32 = 128.0;
    const HELP_SIZE: f32 = 16.0;

    let first_colour = [0.0, 0.6, 0.6, 1.0];
    let text_colour = [0.9, 0.9, 0.9, 1.0];
    let second_colour = [0.6, 0.6, 0.0, 1.0];

    // User manipulatable state
    let mut first_z: u16 = 0;
    let mut second_z: u16 = first_z + 1;
    let mut auto_advance = false;

    let window_state = window_layer::init::<'_, '_, ()>(
        1.,
        [0.3, 0.3, 0.3, 1.0],
        "z-layers example".into()
    )?;

    window_state.run(None, move |event, mut fns| {
        use window_layer::{Event, ElementState, KeyCode};

        macro_rules! advance {
            () => {
                second_z = second_z.saturating_add(1);
                first_z = first_z.saturating_add(1);
            };
        }

        match event {
            Event::CloseRequested => fns.quit(),
            Event::RedrawRequested => {
                if auto_advance {
                    advance!();
                }
                let dimensions = fns.dimensions();
                let width = dimensions.width as f32;
                let height = dimensions.height as f32;

                // The idea here is that we impose a 4 by 4 grid on the screen
                // with the upper left corner notated as (0, 0) and the lower right as (4, 4).
                let x0 = 0.0;
                let y0 = 0.0;

                let x1 = width / 4.0;
                let y1 = height / 4.0;

                #[allow(unused_variables)]
                let x2 = x1 * 2.0;
                let y2 = y1 * 2.0;

                let x3 = x1 * 3.0;
                let y3 = y1 * 3.0;

                let x4 = width;
                let y4 = height;

                let mut text_and_rects = Vec::with_capacity(4);

                text_and_rects.push(TextOrRect::Rect(VisualSpec {
                    rect: ScreenSpaceRect {
                        min: ssxy!(x0, y0),
                        max: ssxy!(x3, y3),
                    },
                    colour: first_colour,
                    z: first_z,
                }));
                let first_z_sub_1 = first_z.saturating_sub(1);
                text_and_rects.push(TextOrRect::Text(TextSpec {
                    text: "first",
                    size: TEXT_SIZE,
                    spec: VisualSpec {
                        rect: ScreenSpaceRect {
                            min: ssxy!(x0, y1),
                            max: ssxy!(x3, y3),
                        },
                        colour: text_colour,
                        z: first_z_sub_1,
                    },
                    layout: TextLayout::Unbounded,
                }));
                let first_z_sub_2 = first_z.saturating_sub(2);
                text_and_rects.push(TextOrRect::Text(TextSpec {
                    text: "1st",
                    size: TEXT_SIZE,
                    spec: VisualSpec {
                        rect: ScreenSpaceRect {
                            min: ssxy!(x1, y2),
                            max: ssxy!(x3, y3),
                        },
                        colour: text_colour,
                        z: first_z_sub_2,
                    },
                    layout: TextLayout::Unbounded,
                }));

                text_and_rects.push(TextOrRect::Rect(VisualSpec {
                    rect: ScreenSpaceRect {
                        min: ssxy!(x1, y1),
                        max: ssxy!(x4, y4),
                    },
                    colour: second_colour,
                    z: second_z,
                }));
                let second_z_sub_1 = second_z.saturating_sub(1);
                text_and_rects.push(TextOrRect::Text(TextSpec {
                    text: "second",
                    size: TEXT_SIZE,
                    spec: VisualSpec {
                        rect: ScreenSpaceRect {
                            min: ssxy!(x1, y1),
                            max: ssxy!(x4, y4),
                        },
                        colour: text_colour,
                        z: second_z_sub_1,
                    },
                    layout: TextLayout::Unbounded,
                }));
                let second_z_sub_2 = second_z.saturating_sub(2);
                text_and_rects.push(TextOrRect::Text(TextSpec {
                    text: "2nd",
                    size: TEXT_SIZE,
                    spec: VisualSpec {
                        rect: ScreenSpaceRect {
                            min: ssxy!(x1, y2),
                            max: ssxy!(x4, y4),
                        },
                        colour: text_colour,
                        z: second_z_sub_2,
                    },
                    layout: TextLayout::Unbounded,
                }));

                let z_text = format!(
                    "{},{},{}\n{},{},{}{:#?}",
                    first_z,
                    first_z_sub_1,
                    first_z_sub_2,
                    second_z,
                    second_z_sub_1,
                    second_z_sub_2,
                    (window_layer::z_to_f32(first_z), window_layer::z_to_f32(second_z))
                );
                text_and_rects.push(TextOrRect::Text(TextSpec {
                    text: &z_text,
                    size: HELP_SIZE,
                    spec: VisualSpec {
                        rect: ScreenSpaceRect {
                            min: ssxy!(x3, y0),
                            max: ssxy!(x4, y1),
                        },
                        colour: text_colour,
                        ..<_>::default()
                    },
                    layout: TextLayout::Unbounded,
                }));

                text_and_rects.push(TextOrRect::Text(TextSpec {
                    text: "Press + or - and there should be no change to the apparent layers things are on.",
                    size: HELP_SIZE,
                    spec: VisualSpec {
                        rect: ScreenSpaceRect {
                            min: ssxy!(x0, y3),
                            max: ssxy!(x1, y4),
                        },
                        colour: text_colour,
                        ..<_>::default()
                    },
                    layout: TextLayout::Unbounded,
                }));

                fns.render(&text_and_rects)
                    .expect("fns.render didn't work");
            }
            Event::KeyboardInput {
                state: ElementState::Pressed,
                keycode,
                modifiers,
            } if modifiers.shift() => {
                const SHIFT: u16 = 65535 / 16;
                match keycode {
                    KeyCode::Escape => {
                        fns.quit();
                    }
                    KeyCode::Up => {
                        second_z = second_z.saturating_sub(SHIFT);
                    }
                    KeyCode::Down => {
                        second_z = second_z.saturating_add(SHIFT);
                    }
                    KeyCode::Left => {
                        first_z = first_z.saturating_sub(SHIFT);
                    }
                    KeyCode::Right => {
                        first_z = first_z.saturating_add(SHIFT);
                    }
                    _ => {}
                }
            },
            Event::KeyboardInput {
                state: ElementState::Pressed,
                keycode,
                modifiers,
            } if !modifiers.shift() => {
                match keycode {
                    KeyCode::Escape => {
                        fns.quit();
                    }
                    KeyCode::Up => {
                        second_z = second_z.saturating_sub(1);
                    }
                    KeyCode::Down => {
                        second_z = second_z.saturating_add(1);
                    }
                    KeyCode::Left => {
                        first_z = first_z.saturating_sub(1);
                    }
                    KeyCode::Right => {
                        first_z = first_z.saturating_add(1);
                    }
                    // AKA plus
                    KeyCode::Equals => {
                        advance!();
                    }
                    KeyCode::Minus => {
                        second_z = second_z.saturating_sub(1);
                        first_z = first_z.saturating_sub(1);
                    }
                    KeyCode::Space => {
                        auto_advance = !auto_advance;
                    }
                    _ => (),
                };
            }
            _ => {}
        }
    });
}
