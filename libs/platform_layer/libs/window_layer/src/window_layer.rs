// TODO To prepare to add a second backend, reduce the API of this crate, then make a dummy cfg
// compile option that provides dummy versions of the API. Make any changes this inspires.

#![cfg_attr(feature = "dummy-window-layer", allow(unused_imports))]

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
        cfg!(feature = "glutin-window-layer-0-29") as u32
        + cfg!(feature = "glutin-window-layer-0-32") as u32
        + cfg!(feature = "dummy-window-layer") as u32
        + cfg!(feature = "c-dummy-window-layer") as u32
    };

    match enabled_features {
        0 => panic!("\nNone of the window-layer features were enabled. You must enable exactly one."),
        1 => {}
        2.. => panic!("\nMultiple of the window-layer features were enabled at once. You must enable exactly one."),
    }
};

#[cfg(feature = "glutin-window-layer-0-32")]
mod glutin_window_layer {
    #![allow(unused)] // TODO remove this, and actual unused stuff, once this builds

    use super::*;

    pub use gl_layer::{FONT_LICENSE};
    use window_layer_types::{U24};

    //
    // Planned steps:
    //
    // * Get this compiling and ruinning at all, by copying from [the example](https://github.com/rust-windowing/glutin/blob/master/glutin_examples/src/lib.rs)
    //     * At first it's fine if it just renders the example triangle, instead of the actual app
    // * Make it render the actual app, copying stuff out of the relevant crates of ours, like gl_layer and open_gl.
    // * Cut out what we can, without breaking things
    // * Decide whether to retire the old versions (leaning yes, once we have the new version working unless it's like slow or something)
    // * See whether we still have motivation to do the rgfw backend

    use glutin_wrapper::{
        config::{GlConfig, Config, ConfigTemplateBuilder, GetGlConfig},
        context::{ContextApi, ContextAttributesBuilder, NotCurrentContext, NotCurrentGlContext, PossiblyCurrentContext, PossiblyCurrentGlContext, Version},
        display::{GetGlDisplay, GlDisplay},
        surface::{GlSurface, Surface, SwapInterval, WindowSurface},
        glutin_winit_wrapper::{
            DisplayBuilder,
            GlWindow,
            finalize_window,
        },
        winit_wrapper::{
            application::ApplicationHandler,
            dpi,
            event::{
                self,
                ElementState,
                StartCause,
                WindowEvent,
                Event as GWEvent,
                KeyEvent,
            },
            event_loop::{
                self,
                ActiveEventLoop,
                EventLoop,
                EventLoopBuilder,
            },
            keyboard::{self, Key, KeyCode as InputKeyCode, NamedKey},
            raw_window_handle::HasWindowHandle,
            window::{
                self,
                Window,
                WindowAttributes,
                WindowId,
                UserAttentionType,
            },
        },
        gl_wrapper as gl,
    };

    type GLfloat = f32;

    use screen_space::{abs, CharDim, ssxy};

    use std::cell::RefCell;
    use std::error::Error;
    use std::ffi::{CStr, CString};
    use std::marker::PhantomData;
    use std::num::NonZeroU32;
    use std::ops::Deref;
    use std::rc::Rc;
    use std::sync::MutexGuard;
    use std::time::Duration;

    /// See [here](https://stackoverflow.com/a/71945606).
    type PhantomUnsend = PhantomData<MutexGuard<'static, ()>>;

    #[derive(Clone, Debug)]
    pub struct EventProxy(event_loop::EventLoopProxy<CustomEvent>);

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
        initial_scale_factor: ScaleFactor,
        clear: window_layer_types::RGBA,
        title: Option<&str>,
        text_sizes: &[f32],
    ) -> Result<
        InitOutput<State, EventProxy>,
        Box<dyn std::error::Error>
    > {
        let events: EventLoop<CustomEvent> = EventLoop::<CustomEvent>::with_user_event().build()?;

        let state = State {
            events,
            initial_scale_factor,
            clear,
            title: title.map(|s| s.to_owned()),
            text_rendering_state: text_rendering::new(
                initial_scale_factor
            )?,
        };

        Ok(InitOutput {
            event_proxy: EventProxy(state.events.create_proxy()),
            char_dims: state.text_rendering_state.get_char_dims(text_sizes),
            // We would need to defer getting the actual size until the window actually appears!
            // So, to retain the API for now, we stuff something in here.
            // Eventually, I guess change the API to mind this limitation?
            dimensions: Dimensions {
                width: abs::Length::from(1024),
                height: abs::Length::from(768),
            },
            state,
        })
    }

    type GLState = gl_layer::State<'static>;

    #[derive(Debug)]
    pub struct State {
        events: EventLoop<CustomEvent>,
        initial_scale_factor: ScaleFactor,
        clear: window_layer_types::RGBA,
        title: Option<String>,
        text_rendering_state: text_rendering::State<'static>,
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
        active_event_loop: &'handle ActiveEventLoop,
        last_tick_duration: &'handle mut Duration,
        interval: &'handle mut spin_sleep_util::Interval,
        last_rate: &'handle mut RatePerSecond,
        rate_reporter: &'handle mut spin_sleep_util::RateReporter,
        app_state: &'handle mut Option<AppState>,
        gl_context: &'handle mut GlContext,
        clear: window_layer_types::RGBA,
        is_focused: &'handle bool,
    }

    /// # Errors
    /// Returns `Err` if the underlying windowing and/or graphics library calls do.
    pub fn render(
        handle: &mut Handle,
        text_or_rects: &[TextOrRect],
    ) -> Result<(), Box<dyn std::error::Error>> {
        if let Some(AppState { gl_surface, window, gl_state }) = handle.app_state.as_mut() {
            // TODO is this call expensive enough to be worth avoiding?
            let dimensions = window.inner_size();

            gl_layer::render(
                gl_state,
                text_or_rects,
                (
                    U24::from_u32_saturating(dimensions.width),
                    U24::from_u32_saturating(dimensions.height)
                )
            );

            let gl_context = handle.gl_context.as_ref().unwrap();

            gl_surface.swap_buffers(gl_context).unwrap();
        }

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
        if let Some(AppState { window, .. }) = handle.app_state.as_mut() {
            window.set_title(title);
        }
    }

    pub fn quit(handle: &mut Handle) {
        if *handle.running {
            *handle.running = false;

            if let Some(AppState { gl_state, .. }) = handle.app_state.as_ref() {
                gl_layer::cleanup(gl_state);
            }
        }

        handle.active_event_loop.exit();
    }

    /// Queues a RedrawRequested event. The reccomendation for clients is to put the rendering
    /// code in the response to that event type, so that the platform can insert extra ones
    /// in certain cases.
    pub fn request_redraw(handle: &Handle) {
        if let Some(AppState { window, .. }) = handle.app_state.as_ref() {
            window.request_redraw();
        }
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
        if let Some(AppState { gl_state, .. }) = handle.app_state.as_mut() {
            gl_layer::set_hidpi_factor(
                gl_state,
                scale_factor,
            );

            gl_layer::get_char_dims(&gl_state, text_sizes)
        } else {
            vec![]
        }
    }

    pub fn request_attention_mode(handle: &Handle, attention: AttentionMode) {
        if let Some(AppState { window, .. }) = handle.app_state.as_ref() {
            request_attention_mode_window(window, attention);
        }
    }

    pub fn set_cursor_icon(handle: &mut Handle, cursor_icon: CursorIcon) {
        if let Some(AppState { window, .. }) = handle.app_state.as_ref() {
            window.set_cursor(match cursor_icon {
                CursorIcon::Default => window::CursorIcon::Default,
                CursorIcon::Text => window::CursorIcon::Text,
                _ => return,
            });
        }
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

    fn request_attention_mode_window(window: &Window, attention: AttentionMode) {
        window.request_user_attention(
            match attention {
                AttentionMode::Neutral => None,
                AttentionMode::Urgent => Some(UserAttentionType::Informational),
            }
        );
    }

    fn window_attributes(title: Option<&str>) -> WindowAttributes {
        let mut window_attrs = WindowAttributes::default()
            .with_inner_size(
                dpi::Size::Logical(dpi::LogicalSize::new(683.0, 393.0))
            );
        if let Some(title) = title {
            window_attrs = window_attrs.with_title(title);
        }

        window_attrs
    }

    enum GlDisplayCreationState {
        /// The display was not build yet.
        Builder(Box<DisplayBuilder>),
        /// The display was already created for the application.
        Init,
    }

    fn create_gl_context(window: &Window, gl_config: &Config) -> NotCurrentContext {
        let raw_window_handle = window.window_handle().ok().map(|wh| wh.as_raw());

        // The context creation part.
        let context_attributes = ContextAttributesBuilder::new().build(raw_window_handle);

        // Since glutin by default tries to create OpenGL core context, which may not be
        // present we should try gles.
        let fallback_context_attributes = ContextAttributesBuilder::new()
            .with_context_api(ContextApi::Gles(None))
            .build(raw_window_handle);

        // There are also some old devices that support neither modern OpenGL nor GLES.
        // To support these we can try and create a 2.1 context.
        let legacy_context_attributes = ContextAttributesBuilder::new()
            .with_context_api(ContextApi::OpenGl(Some(Version::new(2, 1))))
            .build(raw_window_handle);

        // Reuse the uncurrented context from a suspended() call if it exists, otherwise
        // this is the first time resumed() is called, where the context still
        // has to be created.
        let gl_display = gl_config.display();

        unsafe {
            gl_display.create_context(gl_config, &context_attributes).unwrap_or_else(|_| {
                gl_display.create_context(gl_config, &fallback_context_attributes).unwrap_or_else(
                    |_| {
                        gl_display
                            .create_context(gl_config, &legacy_context_attributes)
                            .expect("failed to create context")
                    },
                )
            })
        }
    }

    fn gl_config_picker(configs: Box<dyn Iterator<Item = Config> + '_>) -> Config {
        configs
            .reduce(|accum, config| {
                if config.num_samples() > accum.num_samples() {
                    config
                } else {
                    accum
                }
            })
            .unwrap()
    }

    #[derive(Debug)]
    struct AppState {
        gl_state: GLState,
        gl_surface: Surface<WindowSurface>,
        // NOTE: Window should be dropped after all resources created using its
        // raw-window-handle.
        window: Window,
    }

    type ExitState = Rc<RefCell<Result<(), Box<dyn Error>>>>;

    type EventHandler = Box<dyn 'static + FnMut(Event, Handle)>;

    type GlContext = Option<PossiblyCurrentContext>;

    struct App {
        template: ConfigTemplateBuilder,
        // NOTE: `AppState` carries the `Window`, thus it should be dropped after everything else.
        state: Option<AppState>,
        gl_context: GlContext,
        gl_display: GlDisplayCreationState,
        exit_state: ExitState,
        modifiers: ModifiersState,
        is_focused: bool,
        running: bool,
        initial_scale_factor: ScaleFactor,
        clear: window_layer_types::RGBA,
        title: Option<String>,
        last_tick_duration: Duration,
        interval: spin_sleep_util::Interval,
        last_rate: RatePerSecond,
        rate_reporter: spin_sleep_util::RateReporter,
        event_handler: EventHandler,
    }

    impl App {
        fn new(
            template: ConfigTemplateBuilder,
            display_builder: DisplayBuilder,
            exit_state: ExitState,
            initial_scale_factor: ScaleFactor,
            clear: window_layer_types::RGBA,
            title: Option<String>,
            event_handler: EventHandler,
        ) -> Self {
            let target_duration = Duration::from_secs_f64(1.0 / DEFAULT_TARGET_RATE);

            let mut last_tick_duration = <_>::default();
            let mut interval = spin_sleep_util::interval(target_duration);
    
            // Starting with the target avoids a blip at the beginning, in the expected case
            // that we hit close to it.
            let mut last_rate = DEFAULT_TARGET_RATE;
            let mut rate_reporter = spin_sleep_util::RateReporter::new(Duration::from_secs(1));

            Self {
                template,
                gl_display: GlDisplayCreationState::Builder(Box::new(display_builder)),
                exit_state,
                gl_context: None,
                state: None,
                modifiers: ModifiersState::default(),
                is_focused: false,
                running: true,
                initial_scale_factor,
                clear,
                title,
                last_tick_duration,
                interval,
                last_rate,
                rate_reporter,
                event_handler,
            }
        }
    }

    impl Drop for App {
        fn drop(&mut self) {
            use glutin_wrapper::display::GetGlDisplay;
            // NOTE: The handling below is only needed due to nvidia on Wayland to
            // not crash on exit due to nvidia driver touching the Wayland
            // display from on `exit` hook.
            let Some(_gl_display) = self.gl_context.take().map(|context| context.display()) else {
                return;
            };

            // Clear the window.
            self.state = None;
            #[cfg(egl_backend)]
            #[allow(irrefutable_let_patterns)]
            if let glutin::display::Display::Egl(display) = _gl_display {
                unsafe {
                    display.terminate();
                }
            }
        }
    }

    fn pass_down(app: &mut App, active_event_loop: &ActiveEventLoop, event: Event) {
         (app.event_handler)(
            event,
            Handle {
                running: &mut app.running,
                active_event_loop,
                last_tick_duration: &mut app.last_tick_duration,
                interval: &mut app.interval,
                last_rate: &mut app.last_rate,
                rate_reporter: &mut app.rate_reporter,
                is_focused: &app.is_focused,
                app_state: &mut app.state,
                gl_context: &mut app.gl_context,
                clear: app.clear,
            }
        )
    }

    impl ApplicationHandler<CustomEvent> for App {
        fn resumed(&mut self, event_loop: &ActiveEventLoop) {
            let (window, gl_config) = match &self.gl_display {
                // We just created the event loop, so initialize the display, pick the config, and
                // create the context.
                GlDisplayCreationState::Builder(display_builder) => {
                    let (window, gl_config) = match display_builder.clone().build(
                        event_loop,
                        self.template.clone(),
                        gl_config_picker,
                    ) {
                        Ok((window, gl_config)) => (window.unwrap(), gl_config),
                        Err(err) => {
                            *self.exit_state.borrow_mut() = Err(err);
                            event_loop.exit();
                            return;
                        },
                    };

                    // Mark the display as initialized to not recreate it on resume, since the
                    // display is valid until we explicitly destroy it.
                    self.gl_display = GlDisplayCreationState::Init;

                    // Create gl context.
                    self.gl_context =
                        Some(create_gl_context(&window, &gl_config).treat_as_possibly_current());

                    (window, gl_config)
                },
                GlDisplayCreationState::Init => {
                    println!("Recreating window in `resumed`");
                    // Pick the config which we already use for the context.
                    let gl_config = self.gl_context.as_ref().unwrap().config();
                    match finalize_window(event_loop, window_attributes(self.title.as_deref()), &gl_config) {
                        Ok(window) => (window, gl_config),
                        Err(err) => {
                            *self.exit_state.borrow_mut() = Err(err.into());
                            event_loop.exit();
                            return;
                        },
                    }
                },
            };

            let attrs = window
                .build_surface_attributes(Default::default())
                .expect("Failed to build surface attributes");
            let gl_surface =
                unsafe { gl_config.display().create_window_surface(&gl_config, &attrs).unwrap() };

            // The context needs to be current for the Renderer to set up shaders and
            // buffers. It also performs function loading, which needs a current context on
            // WGL.
            let gl_context = self.gl_context.as_ref().unwrap();
            gl_context.make_current(&gl_surface).unwrap();

            // Try setting vsync.
            if let Err(res) = gl_surface
                .set_swap_interval(gl_context, SwapInterval::Wait(NonZeroU32::new(1).unwrap()))
            {
                eprintln!("Error setting vsync: {res:?}");
            }

            let load_fn = &|symbol: *const u8| {
                let symbol = symbol.cast();
                // SAFETY: The underlying library has promised to pass us a nul
                // terminated pointer.
                let cstr = unsafe { std::ffi::CStr::from_ptr(symbol) };
    
                // The glutin docs say the following:
                // "Returns the address of an OpenGL function."
                // Given there are no qualifiers there, it seems like we can
                // assume that if we have a context then this call should work.
                gl_config.display().get_proc_address(cstr).cast()
            };

            // SAFETY:
            // In the passed closure, we avoid panicking to uphold the documented safety
            // invariant:
            // "The passed `load_fn` must always return accurate function pointer
            // values, or null on failure."
            let gl_state = unsafe { gl_layer::init(
                self.initial_scale_factor,
                self.clear,
                load_fn,
            ) }.expect("gl_layer::init error");

            let new_state = AppState {
                gl_surface,
                window,
                gl_state,
            };

            assert!(self.state.replace(new_state).is_none());
        }

        fn suspended(&mut self, _event_loop: &ActiveEventLoop) {
            use glutin_wrapper::context::{NotCurrentGlContext, PossiblyCurrentGlContext};
            // This event is only raised on Android, where the backing NativeWindow for a GL
            // Surface can appear and disappear at any moment.
            println!("Android window removed");

            // Destroy the GL Surface and un-current the GL Context before ndk-glue releases
            // the window back to the system.
            self.state = None;

            // Make context not current.
            self.gl_context = Some(
                self.gl_context.take().unwrap().make_not_current().unwrap().treat_as_possibly_current(),
            );
        }

        fn new_events(&mut self, event_loop: &ActiveEventLoop, cause: StartCause) {
            if let StartCause::Init = cause {
                pass_down(self, event_loop, Event::Init)
            }
        }
        fn user_event(&mut self, event_loop: &ActiveEventLoop, event: CustomEvent) {
            pass_down(self, event_loop, Event::UserEvent(event))
        }

        fn window_event(
            &mut self,
            event_loop: &ActiveEventLoop,
            _window_id: WindowId,
            event: WindowEvent,
        ) {
            macro_rules! pass_down {
                ($event: expr) => {
                    pass_down(self, event_loop, $event)
                }
            }

            match event {
                WindowEvent::Resized(size) if size.width != 0 && size.height != 0 => {
                    // Some platforms like EGL require resizing GL surface to update the size
                    // Notable platforms here are Wayland and macOS, other don't require it
                    // and the function is no-op, but it's wise to resize it for portability
                    // reasons.
                    if let Some(AppState { gl_surface, gl_state, ..}) = self.state.as_mut() {
                        let gl_context = self.gl_context.as_ref().unwrap();
                        gl_surface.resize(
                            gl_context,
                            NonZeroU32::new(size.width).unwrap(),
                            NonZeroU32::new(size.height).unwrap(),
                        );

                        gl_layer::set_dimensions(
                            gl_state,
                            (
                                U24::from_u32_saturating(size.width),
                                U24::from_u32_saturating(size.height)
                            ),
                        );

                        pass_down!(
                            Event::Resized(physical_size_to_dimensions(size))
                        );
                    }
                },
                WindowEvent::RedrawRequested => pass_down!(
                    Event::RedrawRequested
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

                    if let Some(AppState { gl_state, ..}) = self.state.as_mut() {
                        gl_layer::set_hidpi_factor(
                            gl_state,
                            scale_factor,
                        );

                        pass_down!(
                            Event::ScaleFactorChanged(scale_factor)
                        );
                    }
                }
                WindowEvent::Focused(window_is_focused) => {
                    self.is_focused = window_is_focused;

                    if self.is_focused
                    && let Some(AppState { window, ..}) = self.state.as_mut() {
                        use window_layer_types::AttentionMode;
                        // X11 requires us to explicitly unset the window
                        // urgency. Other platforms fade it out automatically.
                        request_attention_mode_window(
                            window,
                            AttentionMode::Neutral
                        );
                    }

                    pass_down!(
                        Event::Focused(self.is_focused)
                    );
                },
                WindowEvent::ModifiersChanged(new_modifiers) => {
                    self.modifiers = ModifiersState::from_bits_retain(
                        new_modifiers.state().bits()
                    );
                }
                WindowEvent::KeyboardInput {
                    event:
                        KeyEvent {
                            state,
                            physical_key: keyboard::PhysicalKey::Code(input_keycode),
                            text,
                            ..
                        },
                    ..
                } => {
                    if let Some(keycode) = calculate_keycode(input_keycode) {
                        pass_down!(
                            Event::KeyboardInput(KeyboardInput{
                                state: match state {
                                    ElementState::Pressed => ButtonState::Pressed,
                                    ElementState::Released => ButtonState::Released,
                                },
                                keycode,
                                modifiers: self.modifiers,
                            })
                        )
                    }

                    if let Some(c) =
                        text.and_then(|s| if s.len() == 1 { s.as_str().chars().next() } else { None })
                    {
                        pass_down!(Event::ReceivedCharacter(c))
                    }
                },
                WindowEvent::MouseInput {
                    button: mouse_button,
                    state,
                    ..
                } => {
                    match mouse_button {
                        event::MouseButton::Left => {
                            pass_down!(
                                Event::MouseInput(MouseInput{
                                    button: MouseButton::Left,
                                    state: match state {
                                        ElementState::Pressed => ButtonState::Pressed,
                                        ElementState::Released => ButtonState::Released,
                                    },
                                    modifiers: self.modifiers
                                })
                            )
                        },
                        event::MouseButton::Right
                        | event::MouseButton::Middle
                        | event::MouseButton::Back
                        | event::MouseButton::Forward
                        | event::MouseButton::Other(_) => {},
                    };
                },
                WindowEvent::MouseWheel {
                    delta: event::MouseScrollDelta::LineDelta(_, delta),
                    ..
                } => {
                    pass_down!(
                        Event::MouseWheel(MouseWheel{ delta, modifiers: self.modifiers })
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
                            modifiers: self.modifiers,
                        })
                    );
                },
                WindowEvent::CloseRequested => {
                    pass_down!(
                        Event::CloseRequested
                    );
                    event_loop.exit();
                },
                _ => {},
            }
        }

        fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
            pass_down(self, event_loop, Event::MainEventsCleared)
        }

        fn exiting(&mut self, _event_loop: &ActiveEventLoop) {
            // NOTE: The handling below is only needed due to nvidia on Wayland to not crash
            // on exit due to nvidia driver touching the Wayland display from on
            // `exit` hook.
            let _gl_display = self.gl_context.take().unwrap().display();

            // Clear the window.
            self.state = None;
            #[cfg(egl_backend)]
            #[allow(irrefutable_let_patterns)]
            if let glutin::display::Display::Egl(display) = _gl_display {
                unsafe {
                    display.terminate();
                }
            }
        }
    }

    pub fn run<F>(
        State { events, title, text_rendering_state, clear, initial_scale_factor }: State,
        mut event_handler: F
    ) -> !
    where
        F: 'static + FnMut(Event, Handle),
    {
        let template =
            ConfigTemplateBuilder::new().with_alpha_size(8);

        let display_builder = DisplayBuilder::new().with_window_attributes(Some(window_attributes(title.as_deref())));

        let exit_state = Rc::new(RefCell::new(Ok(())));
        let mut app = App::new(
            template,
            display_builder,
            exit_state.clone(),
            initial_scale_factor,
            clear,
            title,
            Box::new(event_handler),
        );

        events.run_app(&mut app).expect("events.run returned an error!");

        exit_state.replace(Ok(())).expect("exit_state was an error!");
        
        std::process::exit(0)
    }

    fn calculate_keycode(input_keycode: InputKeyCode) -> Option<KeyCode> {
        let keycode = match input_keycode {
            InputKeyCode::Digit1 => KeyCode::Key1,
            InputKeyCode::Digit2 => KeyCode::Key2,
            InputKeyCode::Digit3 => KeyCode::Key3,
            InputKeyCode::Digit4 => KeyCode::Key4,
            InputKeyCode::Digit5 => KeyCode::Key5,
            InputKeyCode::Digit6 => KeyCode::Key6,
            InputKeyCode::Digit7 => KeyCode::Key7,
            InputKeyCode::Digit8 => KeyCode::Key8,
            InputKeyCode::Digit9 => KeyCode::Key9,
            InputKeyCode::Digit0 => KeyCode::Key0,
            InputKeyCode::KeyA => KeyCode::A,
            InputKeyCode::KeyB => KeyCode::B,
            InputKeyCode::KeyC => KeyCode::C,
            InputKeyCode::KeyD => KeyCode::D,
            InputKeyCode::KeyE => KeyCode::E,
            InputKeyCode::KeyF => KeyCode::F,
            InputKeyCode::KeyG => KeyCode::G,
            InputKeyCode::KeyH => KeyCode::H,
            InputKeyCode::KeyI => KeyCode::I,
            InputKeyCode::KeyJ => KeyCode::J,
            InputKeyCode::KeyK => KeyCode::K,
            InputKeyCode::KeyL => KeyCode::L,
            InputKeyCode::KeyM => KeyCode::M,
            InputKeyCode::KeyN => KeyCode::N,
            InputKeyCode::KeyO => KeyCode::O,
            InputKeyCode::KeyP => KeyCode::P,
            InputKeyCode::KeyQ => KeyCode::Q,
            InputKeyCode::KeyR => KeyCode::R,
            InputKeyCode::KeyS => KeyCode::S,
            InputKeyCode::KeyT => KeyCode::T,
            InputKeyCode::KeyU => KeyCode::U,
            InputKeyCode::KeyV => KeyCode::V,
            InputKeyCode::KeyW => KeyCode::W,
            InputKeyCode::KeyX => KeyCode::X,
            InputKeyCode::KeyY => KeyCode::Y,
            InputKeyCode::KeyZ => KeyCode::Z,
            InputKeyCode::Escape => KeyCode::Escape,
            InputKeyCode::F1 => KeyCode::F1,
            InputKeyCode::F2 => KeyCode::F2,
            InputKeyCode::F3 => KeyCode::F3,
            InputKeyCode::F4 => KeyCode::F4,
            InputKeyCode::F5 => KeyCode::F5,
            InputKeyCode::F6 => KeyCode::F6,
            InputKeyCode::F7 => KeyCode::F7,
            InputKeyCode::F8 => KeyCode::F8,
            InputKeyCode::F9 => KeyCode::F9,
            InputKeyCode::F10 => KeyCode::F10,
            InputKeyCode::F11 => KeyCode::F11,
            InputKeyCode::F12 => KeyCode::F12,
            InputKeyCode::F13 => KeyCode::F13,
            InputKeyCode::F14 => KeyCode::F14,
            InputKeyCode::F15 => KeyCode::F15,
            InputKeyCode::F16 => KeyCode::F16,
            InputKeyCode::F17 => KeyCode::F17,
            InputKeyCode::F18 => KeyCode::F18,
            InputKeyCode::F19 => KeyCode::F19,
            InputKeyCode::F20 => KeyCode::F20,
            InputKeyCode::F21 => KeyCode::F21,
            InputKeyCode::F22 => KeyCode::F22,
            InputKeyCode::F23 => KeyCode::F23,
            InputKeyCode::F24 => KeyCode::F24,
            InputKeyCode::ScrollLock => KeyCode::Scroll,
            InputKeyCode::Pause => KeyCode::Pause,
            InputKeyCode::Insert => KeyCode::Insert,
            InputKeyCode::Home => KeyCode::Home,
            InputKeyCode::Delete => KeyCode::Delete,
            InputKeyCode::End => KeyCode::End,
            InputKeyCode::PageDown => KeyCode::PageDown,
            InputKeyCode::PageUp => KeyCode::PageUp,
            InputKeyCode::ArrowLeft => KeyCode::Left,
            InputKeyCode::ArrowUp => KeyCode::Up,
            InputKeyCode::ArrowRight => KeyCode::Right,
            InputKeyCode::ArrowDown => KeyCode::Down,
            InputKeyCode::Enter => KeyCode::Return,
            InputKeyCode::Space => KeyCode::Space,
            InputKeyCode::NumLock => KeyCode::Numlock,
            InputKeyCode::Numpad0 => KeyCode::Numpad0,
            InputKeyCode::Numpad1 => KeyCode::Numpad1,
            InputKeyCode::Numpad2 => KeyCode::Numpad2,
            InputKeyCode::Numpad3 => KeyCode::Numpad3,
            InputKeyCode::Numpad4 => KeyCode::Numpad4,
            InputKeyCode::Numpad5 => KeyCode::Numpad5,
            InputKeyCode::Numpad6 => KeyCode::Numpad6,
            InputKeyCode::Numpad7 => KeyCode::Numpad7,
            InputKeyCode::Numpad8 => KeyCode::Numpad8,
            InputKeyCode::Numpad9 => KeyCode::Numpad9,
            InputKeyCode::NumpadAdd => KeyCode::NumpadAdd,
            InputKeyCode::NumpadDivide => KeyCode::NumpadDivide,
            InputKeyCode::NumpadDecimal => KeyCode::NumpadDecimal,
            InputKeyCode::NumpadComma => KeyCode::NumpadComma,
            InputKeyCode::NumpadEnter => KeyCode::NumpadEnter,
            InputKeyCode::NumpadEqual => KeyCode::NumpadEquals,
            InputKeyCode::NumpadMultiply => KeyCode::NumpadMultiply,
            InputKeyCode::NumpadSubtract => KeyCode::NumpadSubtract,
            InputKeyCode::Quote => KeyCode::Apostrophe,
            InputKeyCode::Backslash => KeyCode::Backslash,
            InputKeyCode::LaunchApp2 => KeyCode::Calculator,
            InputKeyCode::Comma => KeyCode::Comma,
            InputKeyCode::Convert => KeyCode::Convert,
            InputKeyCode::Equal => KeyCode::Equals,
            InputKeyCode::Backquote => KeyCode::Grave,
            InputKeyCode::AltLeft => KeyCode::LAlt,
            InputKeyCode::BracketLeft => KeyCode::LBracket,
            InputKeyCode::ControlLeft => KeyCode::LControl,
            InputKeyCode::ShiftLeft => KeyCode::LShift,
            //InputKeyCode::SuperLeft => KeyCode::LWin,
            InputKeyCode::LaunchMail => KeyCode::Mail,
            InputKeyCode::MediaSelect => KeyCode::MediaSelect,
            InputKeyCode::MediaStop => KeyCode::MediaStop,
            InputKeyCode::Minus => KeyCode::Minus,
            InputKeyCode::AudioVolumeMute => KeyCode::Mute,
            InputKeyCode::LaunchApp1 => KeyCode::MyComputer,
            InputKeyCode::BrowserForward => KeyCode::NavigateForward,
            InputKeyCode::BrowserBack => KeyCode::NavigateBackward,
            InputKeyCode::MediaTrackNext => KeyCode::NextTrack,
            InputKeyCode::NonConvert => KeyCode::NoConvert,
            InputKeyCode::Period => KeyCode::Period,
            InputKeyCode::MediaPlayPause => KeyCode::PlayPause,
            InputKeyCode::Power => KeyCode::Power,
            InputKeyCode::MediaTrackPrevious => KeyCode::PrevTrack,
            InputKeyCode::AltRight => KeyCode::RAlt,
            InputKeyCode::BracketRight => KeyCode::RBracket,
            InputKeyCode::ControlRight => KeyCode::RControl,
            InputKeyCode::ShiftRight => KeyCode::RShift,
            //InputKeyCode::SuperRight => KeyCode::RWin,
            InputKeyCode::Semicolon => KeyCode::Semicolon,
            InputKeyCode::Slash => KeyCode::Slash,
            InputKeyCode::Sleep => KeyCode::Sleep,
            InputKeyCode::PrintScreen => KeyCode::Sysrq,
            InputKeyCode::Tab => KeyCode::Tab,
            InputKeyCode::AudioVolumeDown => KeyCode::VolumeDown,
            InputKeyCode::AudioVolumeUp => KeyCode::VolumeUp,
            InputKeyCode::WakeUp => KeyCode::Wake,
            InputKeyCode::IntlYen => KeyCode::Yen,
            InputKeyCode::Copy => KeyCode::Copy,
            InputKeyCode::Paste => KeyCode::Paste,
            InputKeyCode::Cut => KeyCode::Cut,
            _ => return None,
        };

        Some(keycode)
    }
}
#[cfg(feature = "glutin-window-layer-0-32")]
use glutin_window_layer as selected_window_layer;

#[cfg(feature = "glutin-window-layer-0-29")]
mod glutin_window_layer {
    use super::*;

    pub use gl_layer::{FONT_LICENSE};
    use window_layer_types::{U24};

    use glutin_wrapper::{
        dpi,
        event::{
            ElementState,
            KeyboardInput as GWKeyboardInput,
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
        // In the passed closure, we avoid panicking to uphold the documented safety
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
#[cfg(feature = "glutin-window-layer-0-29")]
use glutin_window_layer as selected_window_layer;

#[cfg(feature = "dummy-window-layer")]
mod dummy_window_layer {
    use super::*;

    // No font implies no license needed.
    pub const FONT_LICENSE: &str = "";

    #[derive(Clone, Debug)]
    pub struct EventProxy;

    impl EventSender for EventProxy {
        fn send(&self, _event: CustomEvent) {}
    }

    #[derive(Debug)]
    pub struct State;

    pub fn init(
        _hidpi_factor: ScaleFactor,
        _clear: window_layer_types::RGBA,
        _title: Option<&str>,
        _text_sizes: &[f32],
    ) -> Result<
        InitOutput<State, EventProxy>,
        Box<dyn std::error::Error>
    >
    {
        Err("dummy init impl".into())
    }

    #[derive(Debug)]
    pub struct Handle;

    pub fn render(
        _handle: &mut Handle,
        _text_or_rects: &[TextOrRect],
    ) -> Result<(), Box<dyn std::error::Error>> {
        Ok(())
    }

    #[must_use]
    pub fn report_rate(_handle: &mut Handle) -> RateReport {
        RateReport {
            rate: <_>::default(),
            freshness: Freshness::Stale,
        }
    }

    pub fn set_title(_handle: &mut Handle, _title: &str) {}

    pub fn quit(_handle: &mut Handle) {}

    pub fn request_redraw(_handle: &Handle) {}

    #[must_use]
    pub fn set_scale_factor(
        _handle: &mut Handle,
        _scale_factor: ScaleFactor,
        _text_sizes: &[f32],
    ) -> Vec<screen_space::CharDim> {
        <_>::default()
    }

    pub fn request_attention_mode(_handle: &Handle, _attention: AttentionMode) {}

    pub fn set_cursor_icon(_handle: &mut Handle, _cursor_icon: CursorIcon) {}

    pub fn loop_start(_handle: &mut Handle) -> std::time::Duration {
        <_>::default()
    }

    pub fn loop_sleep(_handle: &mut Handle) {}

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

#[cfg(feature = "c-dummy-window-layer")]
mod c_dummy_window_layer {
    use super::*;

    // No font implies no license needed.
    pub const FONT_LICENSE: &str = "";

    #[derive(Clone, Debug)]
    pub struct EventProxy;

    impl EventSender for EventProxy {
        fn send(&self, _event: CustomEvent) {}
    }

    #[derive(Debug)]
    pub struct State;

    pub fn init(
        hidpi_factor: ScaleFactor,
        _clear: window_layer_types::RGBA,
        _title: Option<&str>,
        text_sizes: &[f32],
    ) -> Result<
        InitOutput<State, EventProxy>,
        Box<dyn std::error::Error>
    >
    {
        let text_rendering_state = text_rendering::new(
            hidpi_factor
        )?;

        let state = State {
            text_rendering_state,
        };

        // A real implementation should create the window now, and report the actual dimensions.
        let dimensions = <_>::default();

        Ok(InitOutput {
            event_proxy: EventProxy,
            char_dims: text_rendering::get_char_dims(&state.text_rendering_state, text_sizes),
            dimension,
            state,
        })
    }

    #[derive(Debug)]
    pub struct Handle;

    pub fn render(
        _handle: &mut Handle,
        _text_or_rects: &[TextOrRect],
    ) -> Result<(), Box<dyn std::error::Error>> {
        Ok(())
    }

    #[must_use]
    pub fn report_rate(_handle: &mut Handle) -> RateReport {
        RateReport {
            rate: <_>::default(),
            freshness: Freshness::Stale,
        }
    }

    pub fn set_title(_handle: &mut Handle, _title: &str) {}

    pub fn quit(_handle: &mut Handle) {}

    pub fn request_redraw(_handle: &Handle) {}

    #[must_use]
    pub fn set_scale_factor(
        _handle: &mut Handle,
        _scale_factor: ScaleFactor,
        _text_sizes: &[f32],
    ) -> Vec<screen_space::CharDim> {
        <_>::default()
    }

    pub fn request_attention_mode(_handle: &Handle, _attention: AttentionMode) {}

    pub fn set_cursor_icon(_handle: &mut Handle, _cursor_icon: CursorIcon) {}

    pub fn loop_start(_handle: &mut Handle) -> std::time::Duration {
        <_>::default()
    }

    pub fn loop_sleep(_handle: &mut Handle) {}

    pub fn run<F>(
        _state: State,
        _event_handler: F
    ) -> !
    where
        F: 'static + FnMut(Event, Handle),
    {
        c_platform::run()
    }
}
#[cfg(feature = "c-dummy-window-layer")]
use c_dummy_window_layer as selected_window_layer;

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