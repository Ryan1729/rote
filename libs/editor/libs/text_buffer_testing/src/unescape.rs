use std::borrow::Cow;

pub fn str(s: &str) -> Cow<str> {
    // TODO map stuff like "\n" to "
    //"
    Cow::Borrowed(s)
}