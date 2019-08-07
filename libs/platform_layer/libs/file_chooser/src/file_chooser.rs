use std::path::PathBuf;

/// Presents a file chooser to the user. If the user selects a file then the callback will be
/// called with their choice. Otherwise, including if the user presses cancel, the callback will
/// never be called. (TODO)
pub fn single<F: FnOnce(PathBuf)>(callback: F) {
    // TODO start up a thread and inside it use nfd-rs to actually show a dialog.
    let _ = dbg!(std::env::current_dir());
    callback(PathBuf::from("./text/slipsum.txt"))
}
