pub use gl_layer::{
    init,
    render,
    set_dimensions,
    cleanup,
    get_char_dims,
    z_to_f32,
    FONT_LICENSE,
    ColouredText,
    State,
    TextLayout,
    TextOrRect,
    TextSpec,
    VisualSpec
};
pub use glutin_wrapper::{*, event::{*, VirtualKeyCode as KeyCode}, event_loop::EventLoopProxy};

