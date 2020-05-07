use std::path::PathBuf;

macro_rules! open_dialog {
    ($nfd_func: ident, $callback: ident) => {
        let join_handle = std::thread::Builder::new()
            .name("file chooser".to_string())
            .spawn(move || {
                match nfd::$nfd_func(None, None) {
                    Ok(nfd::Response::Okay(s)) => $callback(s.into()),
                    Ok(nfd::Response::OkayMultiple(v)) => {
                        for s in v {
                            $callback(s.into());
                            break;
                        }
                    }
                    Ok(nfd::Response::Cancel) => {}
                    Err(e) => {
                        eprintln!("{}", e);
                    }
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
pub fn single<F>(callback: F)
where
    F: Fn(PathBuf) + Send + Sync + 'static,
{
    open_dialog!(open_file_dialog, callback);
}

/// Presents a file save dialog to the user. If the user selects a file then the callback will be
/// called with their choice. Otherwise, including if the user presses cancel, the callback will
/// never be called.
pub fn save<F>(callback: F)
where
    F: Fn(PathBuf) + Send + Sync + 'static,
{
    open_dialog!(open_save_dialog, callback);
}
