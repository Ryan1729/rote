// We might have different platform layer options later, so let's keep this file separate.
pub fn display() {
    opengl::display();
}

mod opengl;
