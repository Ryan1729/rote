// TODO To prepare to add a second backend, reduce the API of this crate, then make a dummy cfg
// compile option that provides dummy versions of the API. Make any changes this inspires.

use platform_layer_types::{
    CustomEvent,
    EventSender,
};

use window_layer_types::{
    AttentionMode,
    ButtonState,
    CursorIcon,
    CursorMoved,
    MouseInput,
    MouseWheel,
    Dimensions,
    Event,
    Freshness,
    InitOutput,
    KeyCode,
    KeyboardInput,
    ModifiersState,
    MouseButton,
    RatePerSecond,
    RateReport,
    ScaleFactor,
    TextOrRect,
};

// https://internals.rust-lang.org/t/cfg-keyword-for-if-multiple-more-than-one/15751/4
const _: () = {
    let enabled_features = {
        cfg!(feature = "glutin-window-layer") as u32
        + cfg!(feature = "dummy-window-layer") as u32
    };

    match enabled_features {
        0 => panic!("\nNone of the window-layer features were enabled. You must enable exactly one."),
        1 => {}
        2.. => panic!("\nMultiple of the window-layer features were enabled at once. You must enable exactly one."),
    }
};

#[cfg(feature = "glutin-window-layer")]
mod glutin_window_layer {
    use super::*;

    pub use gl_layer::{FONT_LICENSE};
    use gl_layer::{U24};

    use glutin_wrapper::{
        dpi,
        event::{
            ElementState,
            StartCause,
            WindowEvent,
            VirtualKeyCode,
        },
        event_loop::{
            ControlFlow,
            EventLoop,
            EventLoopBuilder,
        },
        window::{UserAttentionType},
    };

    use screen_space::{abs, CharDim, ssxy};

    use std::marker::PhantomData;
    use std::sync::MutexGuard;
    use std::time::Duration;

    /// See [here](https://stackoverflow.com/a/71945606).
    type PhantomUnsend = PhantomData<MutexGuard<'static, ()>>;

    #[derive(Clone, Debug)]
    pub struct EventProxy(glutin_wrapper::event_loop::EventLoopProxy<CustomEvent>);

    impl EventSender for EventProxy {
        fn send(&self, event: CustomEvent) {
            let _hope_it_gets_there = self.0.send_event(event);
        }
    }

    /// Initialize the underlying windowing and/or graphics library.
    /// The clear colour currently flashes up on exit.
    /// # Errors
    /// Returns `Err` if the underlying windowing and/or graphics library calls do.
    pub fn init(
        hidpi_factor: ScaleFactor,
        clear: window_layer_types::RGBA,
        title: Option<&str>,
        text_sizes: &[f32],
    ) -> Result<
        InitOutput<State, EventProxy>,
        Box<dyn std::error::Error>
    > {
        use glutin_wrapper::{
            Api,
            GlProfile,
            GlRequest,
            ContextBuilder,
            window::WindowBuilder,
        };

        let events: EventLoop<CustomEvent> = EventLoopBuilder::with_user_event().build();

        let mut window_builder = WindowBuilder::new()
            .with_inner_size(
                dpi::Size::Logical(dpi::LogicalSize::new(683.0, 393.0))
            );
        if let Some(title) = title {
            window_builder = window_builder.with_title(title);
        }

        let context = ContextBuilder::new()
            .with_gl_profile(GlProfile::Core)
            //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
            .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
            .with_srgb(true)
            .with_depth_buffer(24)
            .build_windowed(
                window_builder,
                &events,
            )?;
        let context =
            // SAFETY: A quote from the glutin docs on `make_current`
            // > In OpenGl, only a single context can be current in a thread at a time.
            // > Making a new context current will make the old one not current.
            // > Contexts can only be sent to different threads if they are not current.
            // So, as I understand it, this is only unsafe because of issues regarding
            // sending contexts to other threads. Therefore, I think we can reasonably
            // use this here in `init` without making `init` into an `unsafe fn`, if
            // we make the struct holding the context `!Send`, meaning it cannot be sent
            // to other threads at all.
            unsafe { context.make_current() }
                .map_err(|(_, e)| e)?;

        let load_fn = &|symbol: *const u8| {
            let symbol = symbol.cast();
            // SAFETY: The underlying library has promised to pass us a nul
            // terminated pointer.
            let cstr = unsafe { std::ffi::CStr::from_ptr(symbol) };

            if let Ok(s) = cstr.to_str() {
                // The glutin docs say the following:
                // "Returns the address of an OpenGL function."
                // Given there are no qualifiers there, it seems like we can
                // assume that if we have a context then this call should work.
                context.get_proc_address(s).cast()
            } else {
                // This case is not expected to happen, but the underlying
                // library is specifically documented to not expect this to
                // panic, and instead expects a null pointer on errors, so
                // we'll return that if this case ever happens.
                std::ptr::null()
            }
        };

        // SAFETY:
        // In the below closure, we avoid panicking to uphold the documented safety
        // invariant:
        // "The passed `load_fn` must always return accurate function pointer
        // values, or null on failure."
        let gl_state = unsafe { gl_layer::init(
            hidpi_factor,
            clear,
            load_fn,
        ) }?;

        let state = State {
            gl_state,
            events,
            context,
            unsend: PhantomData,
        };

        Ok(InitOutput {
            event_proxy: EventProxy(state.events.create_proxy()),
            char_dims: gl_layer::get_char_dims(&state.gl_state, text_sizes),
            dimensions: physical_size_to_dimensions(
                state.context
                .window()
                .inner_size()
            ),
            state,
        })
    }

    type Context = glutin_wrapper::ContextWrapper<glutin_wrapper::PossiblyCurrent, glutin_wrapper::window::Window>;

    type GLState = gl_layer::State<'static>;

    #[derive(Debug)]
    pub struct State {
        gl_state: GLState,
        events: EventLoop<CustomEvent>,
        context: Context,
        // See note inside `init` for why we make this struct `!Send` by including this
        // field.
        unsend: PhantomUnsend,
    }

    fn physical_size_to_dimensions(dimensions: dpi::PhysicalSize<u32>) -> Dimensions {
        Dimensions {
            width: abs::Length::from(dimensions.width),
            height: abs::Length::from(dimensions.height),
        }
    }

    #[test]
    fn converting_to_local_dimensions_returns_the_expected_result_in_this_example() {
        let width = 1366u32;
        let height = 768u32;

        let example = dpi::PhysicalSize {
            width,
            height,
        };

        let dimensions = physical_size_to_dimensions(example);

        assert_eq!(f32::from(dimensions.width), width as f32);
        assert_eq!(f32::from(dimensions.height), height as f32);
    }

    /// A handle to various APIs that are useful to have inside the event handler.
    #[derive(Debug)]
    pub struct Handle<'handle> {
        running: &'handle mut bool,
        gl_state: &'handle mut GLState,
        control_flow: &'handle mut ControlFlow,
        last_tick_duration: &'handle mut Duration,
        interval: &'handle mut spin_sleep_util::Interval,
        last_rate: &'handle mut RatePerSecond,
        rate_reporter: &'handle mut spin_sleep_util::RateReporter,
        context: &'handle Context,
        is_focused: &'handle bool,
    }

    /// # Errors
    /// Returns `Err` if the underlying windowing and/or graphics library calls do.
    pub fn render(
        handle: &mut Handle,
        text_or_rects: &[TextOrRect],
    ) -> Result<(), Box<dyn std::error::Error>> {
        // TODO is this call expensive enough to be worth avoiding?
        let dimensions = handle.context
            .window()
            .inner_size();

        gl_layer::render(
            handle.gl_state,
            text_or_rects,
            (
                U24::from_u32_saturating(dimensions.width),
                U24::from_u32_saturating(dimensions.height)
            )
        );

        perf_viz::start_record!("swap_buffers");
        handle.context.swap_buffers()?;
        perf_viz::end_record!("swap_buffers");

        Ok(())
    }

    #[must_use]
    pub fn report_rate(handle: &mut Handle) -> RateReport {
        let mut freshness = Freshness::Stale;
        if let Some(fresh_rate) = handle.rate_reporter.increment_and_report() {
            *handle.last_rate = fresh_rate;
            freshness = Freshness::Fresh;
        }

        RateReport {
            rate: *handle.last_rate,
            freshness,
        }
    }

    pub fn set_title(handle: &mut Handle, title: &str) {
        handle.context.window().set_title(title);
    }

    pub fn quit(handle: &mut Handle) {
        if *handle.running {
            *handle.running = false;

            gl_layer::cleanup(handle.gl_state);
        }

        *handle.control_flow = ControlFlow::Exit;
    }

    /// Queues a RedrawRequested event. The reccomendation for clients is to put the rendering
    /// code in the response to that event type, so that the platform can insert extra ones
    /// in certain cases.
    pub fn request_redraw(handle: &Handle) {
        handle.context.window().request_redraw();
    }

    /// We return the char dims here because changing the scale factor
    /// causes them to change. We rquire the text sizes only in order
    /// to calculate the char dims.
    #[must_use]
    pub fn set_scale_factor(
        handle: &mut Handle,
        scale_factor: ScaleFactor,
        text_sizes: &[f32],
    ) -> Vec<CharDim> {
        gl_layer::set_hidpi_factor(
            handle.gl_state,
            scale_factor,
        );

        gl_layer::get_char_dims(handle.gl_state, text_sizes)
    }

    pub fn request_attention_mode(handle: &Handle, attention: AttentionMode) {
        request_attention_mode_context(handle.context, attention);
    }

    pub fn set_cursor_icon(handle: &mut Handle, cursor_icon: CursorIcon) {
        handle.context.window().set_cursor_icon(match cursor_icon {
            CursorIcon::Default => glutin_wrapper::window::CursorIcon::Default,
            CursorIcon::Text => glutin_wrapper::window::CursorIcon::Text,
            _ => return,
        });
    }

    pub fn loop_start(handle: &mut Handle) -> Duration {
        *handle.last_tick_duration
    }

    pub fn loop_sleep(handle: &mut Handle) {
        perf_viz::start_record!("sleepin'");
        let tick_start = if cfg!(feature="no-spinning-sleep") {
            handle.interval.tick_no_spin()
        } else if *handle.is_focused {
            handle.interval.tick()
        } else {
            handle.interval.tick_no_spin()
        };

        *handle.last_tick_duration = std::time::Instant::now() - tick_start;

        perf_viz::end_record!("sleepin'");
    }

    const DEFAULT_TARGET_RATE: RatePerSecond = 128.0; //250.0;

    fn request_attention_mode_context(context: &Context, attention: AttentionMode) {
        context.window().request_user_attention(
            match attention {
                AttentionMode::Neutral => None,
                AttentionMode::Urgent => Some(UserAttentionType::Informational),
            }
        );
    }

    pub fn run<F>(
        State { mut gl_state, events, context, unsend: _, }: State,
        mut event_handler: F
    ) -> !
    where
        F: 'static + FnMut(Event, Handle),
    {
        let target_duration = Duration::from_secs_f64(1.0 / DEFAULT_TARGET_RATE);

        let mut last_tick_duration = <_>::default();
        let mut interval = spin_sleep_util::interval(target_duration);

        // Starting with the target avoids a blip at the beginning, in the expected case
        // that we hit close to it.
        let mut last_rate = DEFAULT_TARGET_RATE;
        let mut rate_reporter = spin_sleep_util::RateReporter::new(Duration::from_secs(1));

        let mut is_focused = true;

        let mut running = true;

        let mut modifiers = ModifiersState::default();

        events.run(move |event, _, control_flow| {
            use glutin_wrapper::{
                event::{
                    KeyboardInput as GWKeyboardInput,
                    Event as GWEvent
                },
            };

            macro_rules! handle {
                () => {{
                    Handle {
                        running: &mut running,
                        gl_state: &mut gl_state,
                        control_flow,
                        last_tick_duration: &mut last_tick_duration,
                        interval: &mut interval,
                        last_rate: &mut last_rate,
                        rate_reporter: &mut rate_reporter,
                        context: &context,
                        is_focused: &is_focused,
                    }
                }};
            }

            macro_rules! pass_down {
                ($event: expr $(,)?) => {
                    event_handler(
                        $event,
                        handle!()
                    )
                };
            }

            match event {
                GWEvent::RedrawRequested(_) => pass_down!(
                    Event::RedrawRequested
                ),
                GWEvent::NewEvents(StartCause::Init) => pass_down!(
                    Event::Init
                ),
                GWEvent::MainEventsCleared if running => pass_down!(
                    Event::MainEventsCleared
                ),
                GWEvent::UserEvent(e) => pass_down!(
                    Event::UserEvent(e)
                ),
                GWEvent::WindowEvent { event, .. } => {
                    match event {
                        WindowEvent::CloseRequested => pass_down!(
                            Event::CloseRequested
                        ),
                        WindowEvent::ScaleFactorChanged {
                            scale_factor,
                            ..
                        } => {
                            // We don't need this to be totally accurate, just close
                            // enough that the  UI is comfortably usable. And this
                            // value is unlikely to fall into the range that would
                            // be truncated, anyway.
                            #[allow(clippy::cast_possible_truncation)]
                            let scale_factor = scale_factor as _;

                            gl_layer::set_hidpi_factor(
                                &mut gl_state,
                                scale_factor,
                            );

                            pass_down!(
                                Event::ScaleFactorChanged(scale_factor)
                            );
                        }
                        WindowEvent::Focused(window_is_focused) => {
                            is_focused = window_is_focused;

                            if is_focused {
                                use window_layer_types::AttentionMode;
                                // X11 requires us to explicitly unset the window
                                // urgency. Other platforms fade it out automatically.
                                request_attention_mode_context(
                                    &context,
                                    AttentionMode::Neutral
                                );
                            }

                            pass_down!(
                                Event::Focused(is_focused)
                            );
                        },
                        WindowEvent::ReceivedCharacter(c) => pass_down!(
                            Event::ReceivedCharacter(c)
                        ),
                        WindowEvent::Resized(size) => {
                            context.resize(size);
                            gl_layer::set_dimensions(
                                &mut gl_state,
                                (
                                    U24::from_u32_saturating(size.width),
                                    U24::from_u32_saturating(size.height)
                                ),
                            );

                            pass_down!(
                                Event::Resized(physical_size_to_dimensions(size))
                            );
                        }
                        WindowEvent::ModifiersChanged(modifiers_state) => {
                            modifiers = ModifiersState::from_bits_retain(
                                modifiers_state.bits()
                            );
                        }
                        WindowEvent::KeyboardInput {
                            input:
                                GWKeyboardInput {
                                    state,
                                    virtual_keycode: Some(virtual_keycode),
                                    ..
                                },
                            ..
                        } => {
                            let keycode = match virtual_keycode {
                                VirtualKeyCode::Key1 => KeyCode::Key1,
                                VirtualKeyCode::Key2 => KeyCode::Key2,
                                VirtualKeyCode::Key3 => KeyCode::Key3,
                                VirtualKeyCode::Key4 => KeyCode::Key4,
                                VirtualKeyCode::Key5 => KeyCode::Key5,
                                VirtualKeyCode::Key6 => KeyCode::Key6,
                                VirtualKeyCode::Key7 => KeyCode::Key7,
                                VirtualKeyCode::Key8 => KeyCode::Key8,
                                VirtualKeyCode::Key9 => KeyCode::Key9,
                                VirtualKeyCode::Key0 => KeyCode::Key0,
                                VirtualKeyCode::A => KeyCode::A,
                                VirtualKeyCode::B => KeyCode::B,
                                VirtualKeyCode::C => KeyCode::C,
                                VirtualKeyCode::D => KeyCode::D,
                                VirtualKeyCode::E => KeyCode::E,
                                VirtualKeyCode::F => KeyCode::F,
                                VirtualKeyCode::G => KeyCode::G,
                                VirtualKeyCode::H => KeyCode::H,
                                VirtualKeyCode::I => KeyCode::I,
                                VirtualKeyCode::J => KeyCode::J,
                                VirtualKeyCode::K => KeyCode::K,
                                VirtualKeyCode::L => KeyCode::L,
                                VirtualKeyCode::M => KeyCode::M,
                                VirtualKeyCode::N => KeyCode::N,
                                VirtualKeyCode::O => KeyCode::O,
                                VirtualKeyCode::P => KeyCode::P,
                                VirtualKeyCode::Q => KeyCode::Q,
                                VirtualKeyCode::R => KeyCode::R,
                                VirtualKeyCode::S => KeyCode::S,
                                VirtualKeyCode::T => KeyCode::T,
                                VirtualKeyCode::U => KeyCode::U,
                                VirtualKeyCode::V => KeyCode::V,
                                VirtualKeyCode::W => KeyCode::W,
                                VirtualKeyCode::X => KeyCode::X,
                                VirtualKeyCode::Y => KeyCode::Y,
                                VirtualKeyCode::Z => KeyCode::Z,
                                VirtualKeyCode::Escape => KeyCode::Escape,
                                VirtualKeyCode::F1 => KeyCode::F1,
                                VirtualKeyCode::F2 => KeyCode::F2,
                                VirtualKeyCode::F3 => KeyCode::F3,
                                VirtualKeyCode::F4 => KeyCode::F4,
                                VirtualKeyCode::F5 => KeyCode::F5,
                                VirtualKeyCode::F6 => KeyCode::F6,
                                VirtualKeyCode::F7 => KeyCode::F7,
                                VirtualKeyCode::F8 => KeyCode::F8,
                                VirtualKeyCode::F9 => KeyCode::F9,
                                VirtualKeyCode::F10 => KeyCode::F10,
                                VirtualKeyCode::F11 => KeyCode::F11,
                                VirtualKeyCode::F12 => KeyCode::F12,
                                VirtualKeyCode::F13 => KeyCode::F13,
                                VirtualKeyCode::F14 => KeyCode::F14,
                                VirtualKeyCode::F15 => KeyCode::F15,
                                VirtualKeyCode::F16 => KeyCode::F16,
                                VirtualKeyCode::F17 => KeyCode::F17,
                                VirtualKeyCode::F18 => KeyCode::F18,
                                VirtualKeyCode::F19 => KeyCode::F19,
                                VirtualKeyCode::F20 => KeyCode::F20,
                                VirtualKeyCode::F21 => KeyCode::F21,
                                VirtualKeyCode::F22 => KeyCode::F22,
                                VirtualKeyCode::F23 => KeyCode::F23,
                                VirtualKeyCode::F24 => KeyCode::F24,
                                VirtualKeyCode::Snapshot => KeyCode::Snapshot,
                                VirtualKeyCode::Scroll => KeyCode::Scroll,
                                VirtualKeyCode::Pause => KeyCode::Pause,
                                VirtualKeyCode::Insert => KeyCode::Insert,
                                VirtualKeyCode::Home => KeyCode::Home,
                                VirtualKeyCode::Delete => KeyCode::Delete,
                                VirtualKeyCode::End => KeyCode::End,
                                VirtualKeyCode::PageDown => KeyCode::PageDown,
                                VirtualKeyCode::PageUp => KeyCode::PageUp,
                                VirtualKeyCode::Left => KeyCode::Left,
                                VirtualKeyCode::Up => KeyCode::Up,
                                VirtualKeyCode::Right => KeyCode::Right,
                                VirtualKeyCode::Down => KeyCode::Down,
                                VirtualKeyCode::Back => KeyCode::Back,
                                VirtualKeyCode::Return => KeyCode::Return,
                                VirtualKeyCode::Space => KeyCode::Space,
                                VirtualKeyCode::Compose => KeyCode::Compose,
                                VirtualKeyCode::Caret => KeyCode::Caret,
                                VirtualKeyCode::Numlock => KeyCode::Numlock,
                                VirtualKeyCode::Numpad0 => KeyCode::Numpad0,
                                VirtualKeyCode::Numpad1 => KeyCode::Numpad1,
                                VirtualKeyCode::Numpad2 => KeyCode::Numpad2,
                                VirtualKeyCode::Numpad3 => KeyCode::Numpad3,
                                VirtualKeyCode::Numpad4 => KeyCode::Numpad4,
                                VirtualKeyCode::Numpad5 => KeyCode::Numpad5,
                                VirtualKeyCode::Numpad6 => KeyCode::Numpad6,
                                VirtualKeyCode::Numpad7 => KeyCode::Numpad7,
                                VirtualKeyCode::Numpad8 => KeyCode::Numpad8,
                                VirtualKeyCode::Numpad9 => KeyCode::Numpad9,
                                VirtualKeyCode::NumpadAdd => KeyCode::NumpadAdd,
                                VirtualKeyCode::NumpadDivide => KeyCode::NumpadDivide,
                                VirtualKeyCode::NumpadDecimal => KeyCode::NumpadDecimal,
                                VirtualKeyCode::NumpadComma => KeyCode::NumpadComma,
                                VirtualKeyCode::NumpadEnter => KeyCode::NumpadEnter,
                                VirtualKeyCode::NumpadEquals => KeyCode::NumpadEquals,
                                VirtualKeyCode::NumpadMultiply => KeyCode::NumpadMultiply,
                                VirtualKeyCode::NumpadSubtract => KeyCode::NumpadSubtract,
                                VirtualKeyCode::AbntC1 => KeyCode::AbntC1,
                                VirtualKeyCode::AbntC2 => KeyCode::AbntC2,
                                VirtualKeyCode::Apostrophe => KeyCode::Apostrophe,
                                VirtualKeyCode::Apps => KeyCode::Apps,
                                VirtualKeyCode::Asterisk => KeyCode::Asterisk,
                                VirtualKeyCode::At => KeyCode::At,
                                VirtualKeyCode::Ax => KeyCode::Ax,
                                VirtualKeyCode::Backslash => KeyCode::Backslash,
                                VirtualKeyCode::Calculator => KeyCode::Calculator,
                                VirtualKeyCode::Capital => KeyCode::Capital,
                                VirtualKeyCode::Colon => KeyCode::Colon,
                                VirtualKeyCode::Comma => KeyCode::Comma,
                                VirtualKeyCode::Convert => KeyCode::Convert,
                                VirtualKeyCode::Equals => KeyCode::Equals,
                                VirtualKeyCode::Grave => KeyCode::Grave,
                                VirtualKeyCode::Kana => KeyCode::Kana,
                                VirtualKeyCode::Kanji => KeyCode::Kanji,
                                VirtualKeyCode::LAlt => KeyCode::LAlt,
                                VirtualKeyCode::LBracket => KeyCode::LBracket,
                                VirtualKeyCode::LControl => KeyCode::LControl,
                                VirtualKeyCode::LShift => KeyCode::LShift,
                                VirtualKeyCode::LWin => KeyCode::LWin,
                                VirtualKeyCode::Mail => KeyCode::Mail,
                                VirtualKeyCode::MediaSelect => KeyCode::MediaSelect,
                                VirtualKeyCode::MediaStop => KeyCode::MediaStop,
                                VirtualKeyCode::Minus => KeyCode::Minus,
                                VirtualKeyCode::Mute => KeyCode::Mute,
                                VirtualKeyCode::MyComputer => KeyCode::MyComputer,
                                VirtualKeyCode::NavigateForward => KeyCode::NavigateForward,
                                VirtualKeyCode::NavigateBackward => KeyCode::NavigateBackward,
                                VirtualKeyCode::NextTrack => KeyCode::NextTrack,
                                VirtualKeyCode::NoConvert => KeyCode::NoConvert,
                                VirtualKeyCode::OEM102 => KeyCode::OEM102,
                                VirtualKeyCode::Period => KeyCode::Period,
                                VirtualKeyCode::PlayPause => KeyCode::PlayPause,
                                VirtualKeyCode::Plus => KeyCode::Plus,
                                VirtualKeyCode::Power => KeyCode::Power,
                                VirtualKeyCode::PrevTrack => KeyCode::PrevTrack,
                                VirtualKeyCode::RAlt => KeyCode::RAlt,
                                VirtualKeyCode::RBracket => KeyCode::RBracket,
                                VirtualKeyCode::RControl => KeyCode::RControl,
                                VirtualKeyCode::RShift => KeyCode::RShift,
                                VirtualKeyCode::RWin => KeyCode::RWin,
                                VirtualKeyCode::Semicolon => KeyCode::Semicolon,
                                VirtualKeyCode::Slash => KeyCode::Slash,
                                VirtualKeyCode::Sleep => KeyCode::Sleep,
                                VirtualKeyCode::Stop => KeyCode::Stop,
                                VirtualKeyCode::Sysrq => KeyCode::Sysrq,
                                VirtualKeyCode::Tab => KeyCode::Tab,
                                VirtualKeyCode::Underline => KeyCode::Underline,
                                VirtualKeyCode::Unlabeled => KeyCode::Unlabeled,
                                VirtualKeyCode::VolumeDown => KeyCode::VolumeDown,
                                VirtualKeyCode::VolumeUp => KeyCode::VolumeUp,
                                VirtualKeyCode::Wake => KeyCode::Wake,
                                VirtualKeyCode::WebBack => KeyCode::WebBack,
                                VirtualKeyCode::WebFavorites => KeyCode::WebFavorites,
                                VirtualKeyCode::WebForward => KeyCode::WebForward,
                                VirtualKeyCode::WebHome => KeyCode::WebHome,
                                VirtualKeyCode::WebRefresh => KeyCode::WebRefresh,
                                VirtualKeyCode::WebSearch => KeyCode::WebSearch,
                                VirtualKeyCode::WebStop => KeyCode::WebStop,
                                VirtualKeyCode::Yen => KeyCode::Yen,
                                VirtualKeyCode::Copy => KeyCode::Copy,
                                VirtualKeyCode::Paste => KeyCode::Paste,
                                VirtualKeyCode::Cut => KeyCode::Cut,
                            };

                            pass_down!(
                                Event::KeyboardInput(KeyboardInput{
                                    state: match state {
                                        ElementState::Pressed => ButtonState::Pressed,
                                        ElementState::Released => ButtonState::Released,
                                    },
                                    keycode,
                                    modifiers
                                })
                            )
                        },
                        WindowEvent::MouseInput {
                            button: mouse_button,
                            state,
                            ..
                        } => {
                            match mouse_button {
                                glutin_wrapper::event::MouseButton::Left => {
                                    pass_down!(
                                        Event::MouseInput(MouseInput{
                                            button: MouseButton::Left,
                                            state: match state {
                                                ElementState::Pressed => ButtonState::Pressed,
                                                ElementState::Released => ButtonState::Released,
                                            },
                                            modifiers
                                        })
                                    )
                                },
                                glutin_wrapper::event::MouseButton::Right
                                | glutin_wrapper::event::MouseButton::Middle
                                | glutin_wrapper::event::MouseButton::Other(_) => {},
                            };
                        },
                        WindowEvent::MouseWheel {
                            delta: glutin_wrapper::event::MouseScrollDelta::LineDelta(_, delta),
                            ..
                        } => {
                            pass_down!(
                                Event::MouseWheel(MouseWheel{ delta, modifiers })
                            )
                        },
                        WindowEvent::CursorMoved {
                            position,
                            ..
                        } => {
                            // We don't need this to be totally accurate,
                            // just close enough that the  UI is comfortably
                            // usable. And this value is unlikely to fall
                            // into the range that would be truncated,
                            // anyway.
                            #[allow(clippy::cast_possible_truncation)]
                            let x = position.x as f32;
                            #[allow(clippy::cast_possible_truncation)]
                            let y = position.y as f32;
                            pass_down!(
                                Event::CursorMoved(CursorMoved{
                                    position: ssxy!{x, y},
                                    modifiers
                                })
                            );
                        },
                        _ => {}
                    }
                },
                _ => {}
            }
        })
    }
}
#[cfg(feature = "glutin-window-layer")]
use glutin_window_layer as selected_window_layer;

#[cfg(feature = "dummy-window-layer")]
mod dummy_window_layer {
    use super::*;

    // No font implies no license needed.
    pub const FONT_LICENSE: &str = "";

    #[derive(Clone, Debug)]
    pub struct EventProxy;

    impl EventSender for EventProxy {
        fn send(&self, event: CustomEvent) {}
    }

    #[derive(Debug)]
    pub struct State;

    pub fn init(
        hidpi_factor: ScaleFactor,
        clear: window_layer_types::RGBA,
        title: Option<&str>,
        text_sizes: &[f32],
    ) -> Result<
        InitOutput<State, EventProxy>,
        Box<dyn std::error::Error>
    >
    // See https://github.com/rust-lang/rust/issues/80618 for description of this one
    // weird trick.
    where 'title: 'title
    {
        Err("dummy init impl".into())
    }

    #[derive(Debug)]
    pub struct Handle;

    pub fn run<F>(
        _state: State,
        _event_handler: F
    ) -> !
    where
        F: 'static + FnMut(Event, Handle),
    {
        loop {}
    }
}
#[cfg(feature = "dummy-window-layer")]
use dummy_window_layer as selected_window_layer;

// Having things be free functions makes checking that different layer backends
// have implemented all the pieces easier, since the compiler encforces it when
// building this crate alone, or in a very small example.
pub use selected_window_layer::{
    EventProxy,
    FONT_LICENSE,
    Handle,
    State,
    init,
    loop_start,
    loop_sleep,
    quit,
    render,
    report_rate,
    request_attention_mode,
    request_redraw,
    run,
    set_cursor_icon,
    set_title,
    set_scale_factor,
};