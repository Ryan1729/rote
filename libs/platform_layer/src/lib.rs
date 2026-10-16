// We might have different platform layer options later, so let's keep this separate.
use platform_types::EditorAPI;

pub fn run(editor_api: EditorAPI) {
    let result = init::perform_or_exit()
        .and_then(|start_config| wimp::run(editor_api, start_config));

    if let Err(e) = result {
        println!("wimp::run(editor_api) error:\n{}", e);
    }
}

mod init {
    use edited_storage::{canonicalize};
    use shared::{Res, TITLE};
    use wimp_types::StartConfig;

    use std::io::Write;
    use std::path::PathBuf;

    enum InitTask {
        StdoutThenExit(String),
        Start(StartConfig),
    }

    fn calculate_init_task(mut args: impl Iterator<Item = String>) -> Res<InitTask> {
        use std::fmt::Write;

        let mut data_dir = None;
        let mut hidpi_factor_override = None;
        // We expect the program to most often be opened with either 0 or 1 extra paths.
        let mut extra_paths = Vec::with_capacity(1);

        const VERSION: &str = "--version";
        const HELP: &str = "--help";
        const DATA_DIR_OVERRIDE: &str = "--data-dir-override";
        const HIDPI_OVERRIDE: &str = "--hidpi-override";
        const LICENSE: &str = "--license";
        const FILE: &str = "--file";

        while let Some(s) = args.next() {
            let s: &str = &s;
            match s {
                HELP => {
                    let accepted_args = [VERSION, HELP, DATA_DIR_OVERRIDE, HIDPI_OVERRIDE, LICENSE, FILE];
                    let mut output = String::with_capacity(128);
                    writeln!(&mut output, "accepted args: ")?;
                    for arg in &accepted_args {
                        write!(&mut output, "    {}", arg)?;
                        if *arg == DATA_DIR_OVERRIDE {
                            write!(&mut output, " <data directory path>")?;
                        }
                        if *arg == HIDPI_OVERRIDE {
                            write!(&mut output, " <hidpi factor (positive floating point number)>")?;
                        }
                        if *arg == FILE {
                            write!(&mut output, " <path of file to open>")?;
                        }
                        writeln!(&mut output, "")?;
                    }
                    return Ok(InitTask::StdoutThenExit(output))
                }
                VERSION => {
                    // We expect the main crate to unconditionally print the version.
                    // This is because the main crate has access to the correct
                    // version value through the "CARGO_PKG_VERSION" env var.
                    return Ok(InitTask::StdoutThenExit(String::with_capacity(0)))
                }
                DATA_DIR_OVERRIDE => {
                    data_dir = Some(args.next().ok_or_else(|| {
                        format!(
                            "{0} needs an argument. For example: {0} ./data",
                            DATA_DIR_OVERRIDE
                        )
                    })?)
                    .map(PathBuf::from);

                    data_dir = data_dir.map(|dd| dd.canonicalize().unwrap_or(dd));
                }
                HIDPI_OVERRIDE => {
                    hidpi_factor_override = Some(args.next().ok_or_else(|| {
                        format!(
                            "{0} needs an argument. For example: {0} 1.5",
                            HIDPI_OVERRIDE
                        )
                    })?)
                    .and_then(|s| {
                        use std::str::FromStr;
                        window_layer::ScaleFactor::from_str(&s)
                            .ok()
                    });
                }
                LICENSE => {
                    let mut output = String::with_capacity(128);
                    writeln!(&mut output, "{} program by Ryan Wiedemann.", TITLE)?;
                    writeln!(&mut output, "Source and license available at:")?;
                    writeln!(&mut output, "    https://github.com/Ryan1729/{}", TITLE)?;
                    writeln!(&mut output, "")?;
                    writeln!(&mut output, "License for the font:")?;
                    writeln!(&mut output, "{}", window_layer::FONT_LICENSE)?;
                    return Ok(InitTask::StdoutThenExit(output))
                }
                FILE => {
                    let path = args.next().ok_or_else(|| {
                        format!(
                            "{0} needs an argument. For example: {0} ./file.txt",
                            FILE
                        )
                    })
                    // Would it be better to open the window and display a path error
                    // there instead of `?` here?
                    // If someone specified a file, they probably want to open that
                    // particular file, or maybe they made a typo, either way, they
                    // probably would rather have the feedback in their terminal or
                    // whatever, so they can correct the file name. Hence, `?`.
                    .map(PathBuf::from)?;

                    // See comment above for why `?`.
                    extra_paths.push(canonicalize(path)?);
                }
                _ => {
                    return Err(format!("unknown arg {:?}", s).into());
                }
            }
        }

        let data_dir = data_dir
            .or_else(|| {
                directories::ProjectDirs::from("com", "ryanwiedemann", TITLE)
                    .map(|proj_dirs| proj_dirs.data_dir().to_owned())
            })
            .ok_or("Could not find app data dir")?;

        let path_mailbox_path = data_dir.join("path_mailbox_path.txt");    

        let edited_files_dir_buf = data_dir.join("edited_files_v1/");
        let edited_files_index_path_buf = data_dir.join("edited_files_v1_index.txt");
        let running_lock_path = data_dir.join("running.lock");

        let startup_description = format!(
            "data_dir: \n{}", data_dir.to_string_lossy()
        );
        
        Ok(InitTask::Start(StartConfig {
            data_dir,
            running_lock_path,
            edited_files_index_path_buf,
            edited_files_dir_buf,
            path_mailbox_path,
            extra_paths,
            startup_description,
            hidpi_factor_override,
        }))
    }

    pub fn perform_or_exit() -> Res<StartConfig> {
        let mut args = std::env::args();
        //exe name
        args.next();
    
        match calculate_init_task(args)? {
            InitTask::StdoutThenExit(s) => {
                println!("{s}");
                std::process::exit(0)
            },
            InitTask::Start(start_config) => {
                match std::fs::metadata(&start_config.data_dir) {
                    Ok(meta) => {
                        if meta.is_dir() {
                            Ok(())
                        } else {
                            Err("data_dir existed but was not a directory!".to_owned())
                        }
                    }
                    Err(err) => {
                        if err.kind() == std::io::ErrorKind::NotFound {
                            std::fs::create_dir_all(&start_config.data_dir)
                                .map_err(|e| e.to_string())
                                .and_then(|_| {
                                    std::fs::metadata(&start_config.data_dir)
                                        .map_err(|e| e.to_string())
                                        .and_then(|meta| {
                                            if meta.is_dir() {
                                                Ok(())
                                            } else {
                                                Err("data_dir was created but was not a directory!".to_owned())
                                            }
                                        })
                                })
                        } else {
                            Err(err.to_string())
                        }
                    }
                }?;
            
                use atomically::Overwrites;
                let previous_instance_is_running = {
                    let res = atomically::write(
                        &start_config.running_lock_path,
                        Overwrites::Disallow,
                        |f| {
                            // In some sense it doesn't matter what we write here, but it seems like it
                            // would be nice for it to be different each time it is written.
                            use std::time::{SystemTime, UNIX_EPOCH};
                            let thing_to_write = match SystemTime::now().duration_since(UNIX_EPOCH) {
                                Ok(duration) => format!("{}\n", duration.as_nanos()),
                                Err(e) => format!("-{}\n", e.duration().as_nanos()),
                            };
            
                            f.write_all(thing_to_write.as_bytes())
                        }
                    );
            
                    match res {
                        Ok(()) => false,
                        Err(io_error) if io_error.kind() == std::io::ErrorKind::AlreadyExists =>  {
                            true
                        },
                        Err(e) => { return Err(e.into()); }
                    }
                };
            
                if previous_instance_is_running {
                    println!(
                        "Previous instance of {} detected, or at least a file at:\n{}",
                        TITLE,
                        start_config.running_lock_path.to_string_lossy()
                    );
            
                    let path_count = start_config.extra_paths.len();
                    if path_count > 0 {
                        println!(
                            "Adding path{} to {} for the other instance to read.",
                            if path_count == 1 { "" } else { "s" },
                            start_config.path_mailbox_path.to_string_lossy(),
                        );
            
                        // This `inner` function lets us use `?` while also capturing the `Res`
                        // afterward.
                        let inner = |path_mailbox_path: PathBuf| -> Res<()> {
                            let path_list = {
                                use std::fs::OpenOptions;
            
                                let mut path_mailbox = OpenOptions::new()
                                    .read(true)
                                    .write(true)
                                    .create(true)
                                    .open(&path_mailbox_path)?;
            
                                // `read_to_string` apparently already reads the file metadata
                                // to figure out how much to reserve. TODO: confirm this.
                                let mut path_list = String::new();
            
                                use std::io::Read;
                                path_mailbox.read_to_string(&mut path_list)?;
            
                                for p in start_config.extra_paths {
                                    use std::fmt::Write;
                                    // Regarding `to_string_lossy`, in other places, e.g.
                                    // `edited_storage`, we already don't support non-Unicode
                                    // paths.
                                    // TODO: is there a reasonable way to avoid these extra
                                    // allocations?
                                    let _write_on_string_cannot_fail =
                                        writeln!(&mut path_list, "{}", p.to_string_lossy());
                                }
            
                                path_list
                            };
            
                            atomically::write(
                                &path_mailbox_path,
                                Overwrites::Allow,
                                |f| {
                                    f.write_all(path_list.as_bytes())
                                }
                            ).map_err(From::from)
                        };
            
                        let res = inner(start_config.path_mailbox_path);
            
                        match res {
                            Ok(()) => {
                                println!("Seems like we were able to write to the mailbox.");
                            }
                            Err(e) => {
                                println!("Mailbox write failed.");
                                return Err(e);
                            }
                        }
                    }
            
                    std::process::exit(0)
                }

                Ok(start_config)
            },
        }
    }
}