use std::marker::PhantomData;
use std::sync::MutexGuard;

/// See [here](https://stackoverflow.com/a/71945606).
type PhantomUnsend = PhantomData<MutexGuard<'static, ()>>;

pub use screen_space::{abs, CharDim, ScreenSpaceXY, ssxy};

use window_layer_types::{
    KeyCode,
    ModifiersState,
    MouseButton,
};

pub use gl_layer::{
    z_to_f32,
    DEFAULT_Z,
    FONT_LICENSE,
    ColouredText,
    MulticolourTextSpec,
    TextLayout,
    TextOrRect,
    TextSpec,
    U24,
    VisualSpec,
};
pub use glutin_wrapper::{
    window::{
        CursorIcon,
        UserAttentionType,
    },
    event::{
        WindowEvent,
        ElementState,
        StartCause,
    },
    event_loop::{
        EventLoop,
        EventLoopProxy
    },
};

use glutin_wrapper::{
    dpi, 
    event_loop::{
        ControlFlow,
        EventLoopBuilder,
    },
    event::VirtualKeyCode,
};
pub use std::time::Duration;

#[must_use]
pub fn initial_dt(target_rate: Option<RatePerSecond>) -> Duration {
    // This value only affects the first frame, and probably will only be used for
    // visual stuff anyway.
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    Duration::from_nanos((
        (1.0 / target_rate.unwrap_or(DEFAULT_TARGET_RATE)) * 1_000_000_000.0
    ) as u64)
}

pub type RGBA = [f32; 4];

/// The clear colour currently flashes up on exit.
/// # Errors
/// Returns `Err` if the underlying windowing and/or graphics library calls do.
pub fn init<'font, 'title, CustomEvent>(
    hidpi_factor: ScaleFactor,
    clear: RGBA,
    title: Option<&'title str>,
) -> Result<State<'font, CustomEvent>, Box<dyn std::error::Error>>
// See https://github.com/rust-lang/rust/issues/80618 for description of this one
// weird trick.
where 'title: 'title
{
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

    Ok(State {
        gl_state,
        events,
        context,
        unsend: PhantomData,
    })
}

type Context = glutin_wrapper::ContextWrapper<glutin_wrapper::PossiblyCurrent, glutin_wrapper::window::Window>;

#[derive(Debug)]
pub struct State<'font, CustomEvent: 'static = ()> {
    gl_state: gl_layer::State<'font>,
    events: EventLoop<CustomEvent>,
    context: Context,
    // See note inside `init` for why we make this struct `!Send` by including this
    // field.
    unsend: PhantomUnsend,
}

pub fn get_char_dims<CustomEvent>(
    state: &State<'_, CustomEvent>,
    text_sizes: &[f32],
) -> Vec<CharDim> {
    gl_layer::get_char_dims(&state.gl_state, text_sizes)
}

pub fn create_event_proxy<CustomEvent>(
    state: &State<'_, CustomEvent>,
) -> EventLoopProxy<CustomEvent> {
    state.events.create_proxy()
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Dimensions {
    pub width: abs::Length,
    pub height: abs::Length,
}

impl core::fmt::Display for Dimensions {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "({}, {})", self.width, self.height)
    }
}

impl From<dpi::PhysicalSize<u32>> for Dimensions {
    fn from(dimensions: dpi::PhysicalSize<u32>) -> Self {
        Dimensions {
            width: abs::Length::from(dimensions.width),
            height: abs::Length::from(dimensions.height),
        }
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

    let dimensions = Dimensions::from(example);

    assert_eq!(f32::from(dimensions.width), width as f32);
    assert_eq!(f32::from(dimensions.height), height as f32);
}

#[must_use]
pub fn dimensions<A>(
    state: &State<'_, A>,
) -> Dimensions {
    state.context
        .window()
        .inner_size()
        .into()
}

fn render_inner<'font>(
    state: &mut gl_layer::State<'font>,
    context: &Context,
    text_or_rects: &[TextOrRect],
) -> Result<(), Box<dyn std::error::Error>> {
    // TODO is this call expensive enough to be worth avoiding?
    let dimensions = context
        .window()
        .inner_size();

    gl_layer::render(
        state,
        text_or_rects,
        (
            U24::from_u32_saturating(dimensions.width),
            U24::from_u32_saturating(dimensions.height)
        )
    );

    perf_viz::start_record!("swap_buffers");
    context.swap_buffers()?;
    perf_viz::end_record!("swap_buffers");

    Ok(())
}

pub type RatePerSecond = spin_sleep::RatePerSecond;

/// AKA `hidpi_factor`/`DpiFactor`. The text rendering library only accepts `f32`,
/// so there's not much point in higher precision elsewhere.
pub type ScaleFactor = f32;

#[derive(Debug)]
pub struct Fns<'running, 'gl, 'font, 'control, 'loop_helper, 'context> {
    running: &'running mut bool,
    gl_state: &'gl mut gl_layer::State<'font>,
    control_flow: &'control mut ControlFlow,
    dimensions: Dimensions,
    loop_helper: &'loop_helper mut spin_sleep::LoopHelper,
    context: &'context Context,
    is_focused: bool,
}

impl Fns<'_, '_, '_, '_, '_, '_> {
    pub fn quit(&mut self) {
        if *self.running {
            *self.running = false;

            gl_layer::cleanup(self.gl_state);
        }

        *self.control_flow = ControlFlow::Exit;
    }

    #[must_use]
    pub fn dimensions(&self) -> Dimensions {
        self.dimensions
    }

    #[must_use]
    pub fn report_rate(&mut self) -> Option<RatePerSecond> {
        self.loop_helper.report_rate()
    }

    /// # Errors
    /// Returns `Err` if the underlying windowing and/or graphics library calls do.
    pub fn render(&mut self, text_or_rects: &[TextOrRect]) -> Result<(), Box<dyn std::error::Error>> {
        render_inner(
            self.gl_state,
            self.context,
            text_or_rects,
        )
    }

    #[must_use]
    pub fn get_char_dims(
        &self,
        text_sizes: &[f32],
    ) -> Vec<CharDim> {
        gl_layer::get_char_dims(self.gl_state, text_sizes)
    }

    pub fn set_dimensions(
        &mut self,
        scale_factor: ScaleFactor,
        dimensions: Dimensions
    ) {
        gl_layer::set_dimensions(
            self.gl_state,
            scale_factor,
            (
                U24::from_u32_saturating(dimensions.width.trunc_to_u32()),
                U24::from_u32_saturating(dimensions.height.trunc_to_u32())
            ),
        );
    }

    pub fn request_user_attention(&self, attention: Option<UserAttentionType>) {
        self.context.window().request_user_attention(
            attention
        );
    }

    pub fn set_cursor_icon(&mut self, cursor_icon: CursorIcon) {
        self.context.window().set_cursor_icon(cursor_icon);
    }

    pub fn request_redraw(&self) {
        self.context.window().request_redraw();
    }

    pub fn set_title(&self, title: &str) {
        self.context.window().set_title(title);
    }

    #[must_use]
    pub fn is_focused(&self) -> bool {
        self.is_focused
    }

    pub fn loop_sleep(&mut self) {
        perf_viz::start_record!("sleepin'");
        if cfg!(feature="no-spinning-sleep") {
            self.loop_helper.loop_sleep_no_spin();
        } else if self.is_focused {
            self.loop_helper.loop_sleep();
        } else {
            self.loop_helper.loop_sleep_no_spin();
        }
        perf_viz::end_record!("sleepin'");
    }

    pub fn loop_start(&mut self) -> Duration {
        self.loop_helper.loop_start()
    }

    /// Equivalent to calling `loop_sleep` then calling `loop_start`
    pub fn loop_sleep_start(&mut self) -> Duration {
        self.loop_sleep();
        self.loop_start()
    }
}

#[non_exhaustive]
pub enum Event<CustomEvent: 'static = ()> {
    CloseRequested,
    RedrawRequested,
    KeyboardInput {
        state: ElementState,
        keycode: KeyCode,
        modifiers: ModifiersState,
    },
    MouseInput {
        state: ElementState,
        button: MouseButton,
        modifiers: ModifiersState,
    },
    MouseWheel {
        delta: f32,
        modifiers: ModifiersState,
    },
    ScaleFactorChanged(ScaleFactor),
    Resized,
    Focused(bool),
    ReceivedCharacter(char),
    CursorMoved {
        position: ScreenSpaceXY,
        modifiers: ModifiersState
    },
    /// Only sent if we are not in the process of shutting down.
    MainEventsCleared,
    Init,
    UserEvent(CustomEvent)
}

pub const DEFAULT_TARGET_RATE: RatePerSecond = 250.0;



impl <A> State<'static, A> {
    pub fn run<F>(self, target_rate: Option<RatePerSecond>, mut event_handler: F) -> !
    where
        F: 'static + FnMut(Event<A>, Fns),
    {
        let mut gl_state = self.gl_state;
        let events: EventLoop<A> = self.events;
        let context = self.context;

        let mut hidpi_factor = 1.0;

        let chosen_target_rate = target_rate.unwrap_or(DEFAULT_TARGET_RATE);

        let report_interval = chosen_target_rate * (1023./1024.);

        let mut loop_helper = spin_sleep::LoopHelper::builder()
            .report_interval_s(report_interval)
            .build_with_target_rate(chosen_target_rate);
        let mut is_focused = true;

        let mut running = true;
        let mut dimensions = context
            .window()
            .inner_size();

        let mut modifiers = ModifiersState::default();

        events.run(move |event, _, control_flow| {
            use glutin_wrapper::{
                event::{
                    KeyboardInput,
                    Event as GWEvent
                },
            };

            macro_rules! fns {
                () => {{
                    Fns {
                        running: &mut running,
                        gl_state: &mut gl_state,
                        control_flow,
                        dimensions: Dimensions {
                            width: abs::Length::from(dimensions.width),
                            height: abs::Length::from(dimensions.height),
                        },
                        loop_helper: &mut loop_helper,
                        context: &context,
                        is_focused,
                    }
                }};
            }

            macro_rules! pass_down {
                ($event: expr $(,)?) => {
                    event_handler(
                        $event,
                        fns!()
                    )
                };
            }

            match event {
                GWEvent::MainEventsCleared if running => pass_down!(
                    Event::MainEventsCleared
                ),
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
                    Event::UserEvent::<A>(e)
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
                            hidpi_factor = scale_factor;

                            gl_layer::set_dimensions(
                                &mut gl_state,
                                hidpi_factor,
                                (
                                    U24::from_u32_saturating(dimensions.width),
                                    U24::from_u32_saturating(dimensions.height)
                                ),
                            );

                            pass_down!(
                                Event::ScaleFactorChanged(hidpi_factor)
                            );
                        }
                        WindowEvent::Focused(window_is_focused) => {
                            is_focused = window_is_focused;
                            pass_down!(
                                Event::Focused(is_focused)
                            );
                        },
                        WindowEvent::ReceivedCharacter(c) => pass_down!(
                            Event::ReceivedCharacter(c)
                        ),
                        WindowEvent::Resized(size) => {
                            context.resize(size);
                            dimensions = size;
                            gl_layer::set_dimensions(
                                &mut gl_state,
                                hidpi_factor as _,
                                (
                                    U24::from_u32_saturating(dimensions.width),
                                    U24::from_u32_saturating(dimensions.height)
                                ),
                            );

                            pass_down!(
                                Event::Resized
                            );
                        }
                        WindowEvent::ModifiersChanged(modifiers_state) => {
                            modifiers = ModifiersState::from_bits_retain(
                                modifiers_state.bits()
                            );
                        }
                        WindowEvent::KeyboardInput {
                            input:
                                KeyboardInput {
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
                                Event::KeyboardInput { state, keycode, modifiers }
                            )
                        },
                        WindowEvent::MouseInput {
                            button: mouse_button,
                            state,
                            ..
                        } => {
                            let button = match mouse_button {
                                glutin_wrapper::event::MouseButton::Left => MouseButton::Left,
                                glutin_wrapper::event::MouseButton::Right => MouseButton::Right,
                                glutin_wrapper::event::MouseButton::Middle => MouseButton::Middle,
                                glutin_wrapper::event::MouseButton::Other(other) => MouseButton::Other(other),
                            };

                            pass_down!(
                                Event::MouseInput { button, state, modifiers }
                            )
                        },
                        WindowEvent::MouseWheel {
                            delta: glutin_wrapper::event::MouseScrollDelta::LineDelta(_, delta),
                            ..
                        } => {
                            pass_down!(
                                Event::MouseWheel { delta, modifiers }
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
                                Event::CursorMoved {
                                    position: ssxy!{x, y},
                                    modifiers
                                }
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