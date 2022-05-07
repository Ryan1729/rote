use std::marker::PhantomData;
use std::sync::MutexGuard;

/// See https://stackoverflow.com/a/71945606
type PhantomUnsend = PhantomData<MutexGuard<'static, ()>>;

pub use gl_layer::{
    get_char_dims,
    z_to_f32,
    FONT_LICENSE,
    ColouredText,
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
        KeyboardInput,
        MouseButton,
        MouseScrollDelta,
        WindowEvent,
        ElementState,
        StartCause,
        ModifiersState,
        VirtualKeyCode as KeyCode,
        VirtualKeyCode,
    },
    event_loop::{
        EventLoop,
        EventLoopProxy
    },
};

use glutin_wrapper::event_loop::ControlFlow;

pub type RGBA = [f32; 4];

/// The clear colour currently flashes up on exit.
pub fn init<'font, CustomEvent>(
    hidpi_factor: f32,
    clear: RGBA,
) -> Result<State<'font, CustomEvent>, Box<dyn std::error::Error>> {
    use glutin_wrapper::{
        Api,
        GlProfile,
        GlRequest,
        ContextBuilder,
        dpi,
        window::WindowBuilder,
    };

    let events: EventLoop<CustomEvent> = EventLoop::with_user_event();
    let event_proxy = events.create_proxy();

    let context = ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
        .with_srgb(true)
        .with_depth_buffer(24)
        .build_windowed(
            WindowBuilder::new()
                .with_inner_size(
                    dpi::Size::Logical(dpi::LogicalSize::new(683.0, 393.0))
                )
                .with_title("hello-world"),
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
        event_proxy,
        context,
        unsend: PhantomData,
    })
}

type Context = glutin_wrapper::ContextWrapper<glutin_wrapper::PossiblyCurrent, glutin_wrapper::window::Window>;

#[derive(Debug)]
pub struct State<'font, CustomEvent: 'static = ()> {
    gl_state: gl_layer::State<'font>,
    events: EventLoop<CustomEvent>,
    event_proxy: EventLoopProxy<CustomEvent>,
    context: Context,
    // See note inside `init` for why we make this struct `!Send` by including this 
    // field.
    unsend: PhantomUnsend,
}

#[non_exhaustive]
pub enum Event {
    RedrawRequested,
    KeyboardInput {
        state: ElementState,
        keycode: KeyCode,
    },
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

#[derive(Debug)]
pub struct Fns<'running, 'gl, 'font, 'control, 'loop_helper, 'context> {
    running: &'running mut bool,
    gl_state: &'gl mut gl_layer::State<'font>,
    control_flow: &'control mut ControlFlow,
    dimensions: (u32, u32),
    loop_helper: &'loop_helper mut spin_sleep::LoopHelper,
    context: &'context Context,
}

impl Fns<'_, '_, '_, '_, '_, '_> {
    pub fn quit(&mut self) {
        *self.running = false;

        let _ = gl_layer::cleanup(&self.gl_state);

        *self.control_flow = ControlFlow::Exit;
    }

    pub fn dimensions(&self) -> (u32, u32) {
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
}

impl <A> State<'static, A> {
    pub fn run<F>(self, mut event_handler: F) -> !
    where
        F: 'static + FnMut(Event, Fns),
    {
        let mut gl_state = self.gl_state;
        let mut events = self.events;
        let mut event_proxy = self.event_proxy;
        let context = self.context;

        let mut hidpi_factor = 1.0;
    
        let mut loop_helper = spin_sleep::LoopHelper::builder()
            .report_interval_s(1./500.)
            .build_with_target_rate(250.0);
    
        let mut running = true;
        let mut dimensions = context
            .window()
            .inner_size();

        events.run(move |event, _, control_flow| {
            use glutin_wrapper::{
                event::Event as GWEvent,
            };

            macro_rules! fns {
                () => {{
                    Fns {
                        running: &mut running,
                        gl_state: &mut gl_state,
                        control_flow,
                        dimensions: (dimensions.width, dimensions.height),
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
                        WindowEvent::CloseRequested => fns!().quit(),
                        WindowEvent::ScaleFactorChanged {
                            scale_factor,
                            ..
                        } => {
                            hidpi_factor = scale_factor;
                        }
                        WindowEvent::Resized(size) => {
                            context.resize(size);
                            dimensions = size;
                            gl_layer::set_dimensions(
                                &mut gl_state,
                                hidpi_factor as _,
                                (dimensions.width as _, dimensions.height as _),
                            );
                        }
                        WindowEvent::KeyboardInput {
                            input:
                                KeyboardInput {
                                    state,
                                    virtual_keycode: Some(keycode),
                                    ..
                                },
                            ..
                        }=> {
                            pass_down!(
                                Event::KeyboardInput { state, keycode }
                            );
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        })
    }
}