use std::marker::PhantomData;
use std::sync::MutexGuard;

/// See https://stackoverflow.com/a/71945606
type PhantomUnsend = PhantomData<MutexGuard<'static, ()>>;

pub use screen_space::{abs, CharDim, ScreenSpaceXY, ssxy};

pub use gl_layer::{
    z_to_f32,
    DEFAULT_Z,
    FONT_LICENSE,
    ColouredText,
    MulticolourTextSpec,
    TextLayout,
    TextOrRect,
    TextSpec,
    VisualSpec
};
pub use glutin_wrapper::{
    window::{
        CursorIcon,
        UserAttentionType,
    },
    event::{
        MouseButton,
        MouseScrollDelta,
        WindowEvent,
        ElementState,
        StartCause,
        ModifiersState,
        VirtualKeyCode as KeyCode,
    },
    event_loop::{
        EventLoop,
        EventLoopProxy
    },
};

use glutin_wrapper::event_loop::ControlFlow;

pub type RGBA = [f32; 4];

/// The clear colour currently flashes up on exit.
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
        dpi,
        window::WindowBuilder,
    };

    let events: EventLoop<CustomEvent> = EventLoop::with_user_event();

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
        // SAFTEY: A quote from the glutin docs on `make_current`
        // > In OpenGl, only a single context can be current in a thread at a time. 
        // > Making a new context current will make the old one not current. 
        // > Contexts can only be sent to different threads if they are not current.
        // So, as I understand it, this is only unsafe becasue of issues regarding
        // sending contexts to other threads. Therefore, I think we can reasonably
        // use this here in `init` without making `init` into an `unsafe fn`, if 
        // we make the struct holding the context `!Send`, meaning it cannot be sent
        // to other threads at all.
        unsafe { context.make_current() }
            .map_err(|(_, e)| e)?;

    let gl_state = gl_layer::init(
        hidpi_factor,
        clear,
        &|symbol| {
            // SAFETY: The underlying library has promised to pass us a nul 
            // terminated pointer.
            let cstr = unsafe { std::ffi::CStr::from_ptr(symbol as _) };
    
            let s = cstr.to_str().unwrap();
    
            context.get_proc_address(s) as _
        },
    )?;

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

pub fn dimensions(
    state: &State,
) -> Dimensions {
    let dimensions = state.context
        .window()
        .inner_size();

    Dimensions {
        width: abs::Length::from(dimensions.width),
        height: abs::Length::from(dimensions.height),
    }
}

pub fn render(
    state: &mut State,
    text_or_rects: &[TextOrRect],
) -> Result<(), Box<dyn std::error::Error>> {
    render_inner(
        &mut state.gl_state,
        &state.context,
        text_or_rects,
    )
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
        dimensions.width as _,
        dimensions.height as _
    )?;

    context.swap_buffers()?;

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
}

impl Fns<'_, '_, '_, '_, '_, '_> {
    pub fn quit(&mut self) {
        *self.running = false;

        let _ = gl_layer::cleanup(&self.gl_state);

        *self.control_flow = ControlFlow::Exit;
    }

    pub fn dimensions(&self) -> Dimensions {
        self.dimensions
    }

    pub fn report_rate(&mut self) -> Option<RatePerSecond> {
        self.loop_helper.report_rate()
    }

    pub fn render(&mut self, text_or_rects: &[TextOrRect]) -> Result<(), Box<dyn std::error::Error>> {
        render_inner(
            &mut self.gl_state,
            &self.context,
            text_or_rects,
        )
    }

    pub fn set_dimensions(
        &mut self,
        scale_factor: ScaleFactor,
        dimensions: Dimensions
    ) {
        gl_layer::set_dimensions(
            &mut self.gl_state,
            scale_factor,
            (dimensions.width.trunc_to_u32(), dimensions.height.trunc_to_u32()),
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
}

#[non_exhaustive]
pub enum Event {
    CloseRequested,
    RedrawRequested,
    KeyboardInput {
        state: ElementState,
        keycode: KeyCode,
        modifiers: ModifiersState,
    },
    MouseWheel {
        delta: MouseScrollDelta,
        modifiers: ModifiersState,
    },
    ScaleFactorChanged(ScaleFactor),
    Resized,
    Focused(bool),
    ReceivedCharacter(char),
    CursorMoved {
        position: ScreenSpaceXY,
        modifiers: ModifiersState
    }
}

impl <A> State<'static, A> {
    pub fn run<F>(self, target_rate: Option<f32>, mut event_handler: F) -> !
    where
        F: 'static + FnMut(Event, Fns),
    {
        let mut gl_state = self.gl_state;
        let events = self.events;
        let context = self.context;

        let mut hidpi_factor = 1.0;
    
        let mut loop_helper = spin_sleep::LoopHelper::builder()
            .report_interval_s(1./500.)
            .build_with_target_rate(target_rate.unwrap_or(250.0));
    
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
                GWEvent::MainEventsCleared if running => {
                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    context.window().request_redraw();
                }
                GWEvent::RedrawRequested(_) => {
                    pass_down!(
                        Event::RedrawRequested
                    );

                    loop_helper.loop_sleep();
    
                    // We want to track the time that the message loop takes too!
                    loop_helper.loop_start();
                }
                GWEvent::NewEvents(StartCause::Init) => {
                    // At least try to measure the first frame accurately
                    loop_helper.loop_start();
                }
                GWEvent::WindowEvent { event, .. } => {
                    match event {
                        WindowEvent::CloseRequested => pass_down!(
                            Event::CloseRequested
                        ),
                        WindowEvent::ScaleFactorChanged {
                            scale_factor,
                            ..
                        } => {
                            hidpi_factor = scale_factor as _;
                            gl_layer::set_dimensions(
                                &mut gl_state,
                                hidpi_factor,
                                (dimensions.width as _, dimensions.height as _),
                            );

                            pass_down!(
                                Event::ScaleFactorChanged(hidpi_factor)
                            );
                        }
                        WindowEvent::Focused(is_focused) => pass_down!(
                            Event::Focused(is_focused)
                        ),
                        WindowEvent::ReceivedCharacter(c) => pass_down!(
                            Event::ReceivedCharacter(c)
                        ),
                        WindowEvent::Resized(size) => {
                            context.resize(size);
                            dimensions = size;
                            gl_layer::set_dimensions(
                                &mut gl_state,
                                hidpi_factor as _,
                                (dimensions.width as _, dimensions.height as _),
                            );

                            pass_down!(
                                Event::Resized
                            );
                        }
                        WindowEvent::ModifiersChanged(modifiers_state) => {
                            modifiers = modifiers_state;
                        }
                        WindowEvent::KeyboardInput {
                            input:
                                KeyboardInput {
                                    state,
                                    virtual_keycode: Some(keycode),
                                    ..
                                },
                            ..
                        } => pass_down!(
                            Event::KeyboardInput { state, keycode, modifiers }
                        ),
                        WindowEvent::MouseWheel {
                            delta,
                            ..
                        } => pass_down!(
                            Event::MouseWheel { delta, modifiers }
                        ),
                        WindowEvent::CursorMoved {
                            position,
                            ..
                        } => pass_down!(
                            Event::CursorMoved { 
                                position: ssxy!{
                                    position.x as f32,
                                    position.y as f32,
                                },
                                modifiers
                            }
                        ),
                        _ => {}
                    }
                },
                _ => {}
            }
        })
    }
}