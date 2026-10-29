/// This is an example/test that is used to ensure that the scroll related stuff exported by
/// `platform_types` works properly. This visual stuff is eaiser to verify visually.
use window_layer_types::{InitOutput, ModifiersState, TextLayout, TextOrRect, TextSpec, VisualSpec};
use platform_types::{abs, screen_positioning::*};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    const HELP_SIZE: f32 = 16.0;

    const CTRL: ModifiersState = ModifiersState::CTRL;
    const SHIFT: ModifiersState = ModifiersState::SHIFT;

    let first_colour = [0.0, 0.6, 0.6, 1.0];
    let text_colour = [0.9, 0.9, 0.9, 1.0];
    let second_colour = [0.6, 0.6, 0.0, 1.0];

    let error_colour = [0.6, 0.0, 0.0, 1.0];

    let first_z: u16 = 1 << 15;
    let second_z: u16 = 1 << 14;

    let apron = apron!(1.0);
    let move_amount: abs::Length = abs::Length::from(16.0);

    // User manipulable state
    let mut scroll_xy: ScrollXY = <_>::default();
    let mut cursor_xy: TextSpaceXY = <_>::default();
    let mut visibility_attempt = VisibilityAttemptResult::Succeeded;
    let mut attempt_count = 0;

    let InitOutput {
        state: window_state,
        mut dimensions,
        ..
    } = window_layer::init(
        1.,
        [0.3, 0.3, 0.3, 1.0],
        "scroll visualiztion example/test".into(),
        &[HELP_SIZE],
    )?;

    let mut text_box_xywh = {
        let width = f32::from(dimensions.width);
        let height = f32::from(dimensions.height);

        tbxywh!(
            width * 0.25,
            height * 0.25,
            width / 2.0,
            height / 2.0
        )
    };

    window_layer::run(window_state, move |event, ref mut handle| {
        use window_layer_types::{Event, ButtonState, KeyboardInput, MouseWheel, KeyCode};
        match event {
            Event::CloseRequested => handle.quit(),
            Event::Resized(new_dimensions) => {
                dimensions = new_dimensions;
            },
            Event::RedrawRequested => {
                let mut text_and_rects = Vec::with_capacity(16);

                text_and_rects.push(TextOrRect::Rect(VisualSpec {
                    rect: text_box_xywh.into(),
                    colour: first_colour,
                    z: first_z,
                }));
                let first_z_add_1 = first_z.saturating_add(1);
                let text_space_ssr = ssxywh!(
                    text_space_to_screen_space(
                        scroll_xy,
                        text_box_xywh.xy,
                        <_>::default()
                    ),
                    text_box_xywh.wh
                ).into();

                text_and_rects.push(TextOrRect::Rect(VisualSpec {
                    rect: text_space_ssr,
                    colour: second_colour,
                    z: second_z,
                }));

                let cursor_screen_xy = text_space_to_screen_space(
                    scroll_xy,
                    text_box_xywh.xy,
                    cursor_xy
                );

                let text_box_ssr = ssxywh!(
                    text_box_xywh.xy.into(),
                    text_box_xywh.wh
                ).into();

                text_and_rects.push(TextOrRect::Text(TextSpec {
                    text: "▏",
                    size: HELP_SIZE,
                    spec: VisualSpec {
                        rect: ssxywh!(
                            cursor_screen_xy,
                            text_box_xywh.wh
                        ).into(),
                        colour: if inside_rect(cursor_screen_xy, text_box_ssr) {
                            text_colour
                        } else {
                            error_colour
                        },
                        z: first_z_add_1,
                    },
                    layout: TextLayout::Unbounded,
                }));

                let info_text = &format!(
                    "scroll_xy {:?}\ntext_box_xywh: {:?} {:?}\ncursor_xy: {:?}\nattempt #{} {:?}\nSpacebar to make attempt to make the cursor visible.\nArrows or scroll wheel along with various combinations of Ctrl and Shift to move things",
                    scroll_xy,
                    text_box_xywh.xy,
                    text_box_xywh.wh,
                    cursor_xy,
                    attempt_count,
                    visibility_attempt,
                );

                text_and_rects.push(TextOrRect::Text(TextSpec {
                    text: info_text,
                    size: HELP_SIZE,
                    spec: VisualSpec {
                        rect: ScreenSpaceRect {
                            min: ssxy!(
                                0.0,
                                f32::from(dimensions.height)
                                - (info_text.lines().count() as f32 * HELP_SIZE)
                            ),
                            max: ssxy!(
                                f32::from(dimensions.width), 
                                f32::from(dimensions.height)
                            ),
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
                modifiers,
            }) if modifiers == SHIFT => {
                match keycode {
                    KeyCode::Escape => {
                        handle.quit();
                    }
                    KeyCode::Up => {
                        text_box_xywh.xy.y -= move_amount;
                    }
                    KeyCode::Down => {
                        text_box_xywh.xy.y += move_amount;
                    }
                    KeyCode::Left => {
                        text_box_xywh.xy.x -= move_amount;
                    }
                    KeyCode::Right => {
                        text_box_xywh.xy.x += move_amount;
                    }
                    _ => {}
                }
            },
            Event::KeyboardInput(KeyboardInput {
                state: ButtonState::Pressed,
                keycode,
                modifiers,
            }) if modifiers == CTRL => {
                match keycode {
                    KeyCode::Escape => {
                        handle.quit();
                    }
                    KeyCode::Up => {
                        text_box_xywh.wh.h =
                            text_box_xywh.wh.h - move_amount;
                    }
                    KeyCode::Down => {
                        text_box_xywh.wh.h =
                            text_box_xywh.wh.h + move_amount;
                    }
                    KeyCode::Left => {
                        text_box_xywh.wh.w =
                            text_box_xywh.wh.w - move_amount;
                    }
                    KeyCode::Right => {
                        text_box_xywh.wh.w =
                            text_box_xywh.wh.w + move_amount;
                    }
                    _ => {}
                }
            },
            Event::KeyboardInput(KeyboardInput {
                state: ButtonState::Pressed,
                keycode,
                modifiers,
            }) if modifiers.is_empty() => {
                match keycode {
                    KeyCode::Escape => {
                        handle.quit();
                    }
                    KeyCode::Up => {
                        cursor_xy.y -= move_amount;
                    }
                    KeyCode::Down => {
                        cursor_xy.y += move_amount;
                    }
                    KeyCode::Left => {
                        cursor_xy.x -= move_amount;
                    }
                    KeyCode::Right => {
                        cursor_xy.x += move_amount;
                    }
                    // AKA plus
                    KeyCode::Equals => {

                    }
                    KeyCode::Minus => {

                    }
                    KeyCode::Space => {
                        visibility_attempt = attempt_to_make_xy_visible(
                            &mut scroll_xy,
                            text_box_xywh,
                            apron,
                            cursor_xy,
                        );
                        attempt_count += 1;
                    }
                    _ => (),
                }
            }
            Event::MouseWheel(MouseWheel {
                delta: y,
                modifiers,
                ..
            }) if modifiers.is_empty() => {
                scroll_xy.y += abs::Vector::from(y * 16.0);
            }
            Event::MouseWheel(MouseWheel {
                delta: y,
                modifiers,
                ..
            }) if modifiers == SHIFT => {
                scroll_xy.x += abs::Vector::from(y * 16.0);
            }
            _ => {}
        }
    });
}
