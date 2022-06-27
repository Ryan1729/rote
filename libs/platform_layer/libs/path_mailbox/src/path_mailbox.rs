use std::path::PathBuf;
use std::sync::mpsc::{channel, Sender};
use std::thread::JoinHandle;
use std::time::Duration;
use window_layer::EventLoopProxy;
use wimp_types::{CustomEvent, PidKind};

#[derive(Debug)]
pub enum Thread {
    Quit,
}

pub struct Paths {
    pub running_lock: PathBuf,
    pub mailbox: PathBuf,
}

pub fn start_thread(
    paths: Paths,
    proxy: EventLoopProxy<CustomEvent>,
    on_shutdown: fn(PathBuf),
) -> (Sender<Thread>, JoinHandle<()>) {
    // into the path mailbox thread
    let (in_sink, in_source) = channel();

    let join_handle = {
        std::thread::Builder::new()
            .name("path_mailbox".to_string())
            .spawn(move || {
                {
                    let _hope_it_gets_there = proxy.send_event(
                        CustomEvent::Pid(
                            PidKind::PathMailbox,
                            std::process::id()
                        )
                    );
                }
                // If we got an error we should still keep this thread going
                // in case the error is temporary.
                macro_rules! continue_if_err {
                    ($result: expr) => {
                        match $result {
                            Ok(x) => {x},
                            Err(_) => {
                                continue;
                            }
                        }
                    }
                }

                loop {
                    std::thread::sleep(Duration::from_millis(50));

                    if let Ok(message) = in_source.try_recv() {
                        use Thread::*;
                        match message {
                            Quit => {
                                (on_shutdown)(paths.running_lock);
                                return
                            },
                        }
                    }

                    let path_mailbox_string = continue_if_err!(
                        std::fs::read_to_string(
                            &paths.mailbox
                        )
                    );

                    // We want to replace the file with a blank one.
                    // Not writing anything to the file achieves this.
                    continue_if_err!(
                        atomically::write(
                            &paths.mailbox,
                            atomically::Overwrites::Allow,
                            |_| Ok(())
                        )
                    );

                    for line in path_mailbox_string.lines() {
                        let _hope_it_gets_there =
                            proxy.send_event(CustomEvent::OpenFile(
                                // We don't know the CWD of the instance that put
                                // this in the mailbox, so it does not make sense
                                // to canonicalize this path ourselves. The code
                                // putting the line in here must do that.
                                // ... That said, it's convenient to fuse the
                                // canonicalization and position parsing, so any
                                // canonicalizing that happens shouldn't hurt,
                                // given the path is canonical already.
                                std::path::PathBuf::from(line)
                            ));
                    }
                }
            })
            .expect("Could not start path_mailbox thread!")
    };

    (in_sink, join_handle)
}