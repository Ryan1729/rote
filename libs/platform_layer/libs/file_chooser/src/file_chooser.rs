use std::path::PathBuf;

/// Presents a file chooser to the user. If the user selects a file then the callback will be
/// called with their choice. Otherwise, including if the user presses cancel, the callback will
/// never be called. (TODO)
pub fn single<F>(callback: F)
where
    F: Fn(PathBuf) + Send + Sync + 'static,
{
    let join_handle = std::thread::Builder::new()
        .name("file chooser".to_string())
        .spawn(move || {
            match nfd::open_file_dialog(None, None) {
                Ok(nfd::Response::Okay(s)) => callback(s.into()),
                Ok(nfd::Response::OkayMultiple(v)) => {
                    for s in v {
                        callback(s.into());
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
}
