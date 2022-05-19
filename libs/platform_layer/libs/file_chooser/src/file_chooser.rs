use std::path::{PathBuf};

macro_rules! open_dialog {
    ($rfd_method: ident($default_path: expr), $callback: ident) => {
        let join_handle = std::thread::Builder::new()
            .name("file chooser".to_string())
            .spawn(move || {
                let default_path: Option<_> = $default_path
                    .as_mut()
                    .map(|p: &mut PathBuf| {
                        if p.is_file() {
                            p.pop();
                        }
                        p
                    });

                let mut dialog = rfd::FileDialog::new();

                if let Some(p) = default_path {
                    dialog = dialog.set_directory(p);
                }

                match dialog.$rfd_method() {
                    Some(s) => $callback(s.into()),
                    None => {},
                };
            });

        if let Err(e) = join_handle {
            eprintln!("{}", e);
        }
    };
}

/// Presents a file chooser to the user. If the user selects a file then the callback will be
/// called with their choice. Otherwise, including if the user presses cancel, the callback will
/// never be called.
pub fn single<F>(mut default_path: Option<PathBuf>, callback: F)
where
    F: Fn(PathBuf) + Send + Sync + 'static,
{
    open_dialog!(pick_file(default_path), callback);
}

/// Presents a file save dialog to the user. If the user selects a file then the callback will be
/// called with their choice. Otherwise, including if the user presses cancel, the callback will
/// never be called.
pub fn save<F>(mut default_path: Option<PathBuf>, callback: F)
where
    F: Fn(PathBuf) + Send + Sync + 'static,
{
    open_dialog!(save_file(default_path), callback);
}
