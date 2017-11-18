extern crate libc;

/*** includes ***/

use libc::{perror, tcgetattr, tcsetattr, termios, CS8, BRKINT, ECHO, ICANON, ICRNL, IEXTEN, INPCK,
           ISIG, ISTRIP, IXON, OPOST, TCSAFLUSH, VMIN, VTIME};
use std::io::{self, ErrorKind, Read, Write};
use std::os::unix::io::AsRawFd;
use std::ffi::CString;

/*** defines ***/
macro_rules! CTRL_KEY {
    ($k :expr) => (($k) & 0b0001_1111)
}


/*** data ***/

struct EditorConfig {
    orig_termios: termios,
}

// This is a reasonably nice way to have a "uninitialized/zeroed" global,
// given what is stable in Rust 1.21.0
static mut EDITOR_CONFIG: Option<EditorConfig> = None;

/*** terminal ***/

fn die(s: &str) {
    let mut stdout = io::stdout();
    stdout.write(b"\x1b[2J").unwrap_or_default();
    stdout.write(b"\x1b[H").unwrap_or_default();

    stdout.flush().unwrap_or_default();

    if let Ok(c_s) = CString::new(s) {
        unsafe { perror(c_s.as_ptr()) };
    }
    std::process::exit(1);
}

fn disable_raw_mode() {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        unsafe {
            if tcsetattr(
                io::stdin().as_raw_fd(),
                TCSAFLUSH,
                &mut editor_config.orig_termios as *mut termios,
            ) == -1
            {
                die("tcsetattr");
            }
        }
    }
}

fn enable_raw_mode() {
    unsafe {
        EDITOR_CONFIG = Some(std::mem::zeroed());
        if let Some(editor_config) = EDITOR_CONFIG.as_mut() {
            let stdin_fileno = io::stdin().as_raw_fd();

            if tcgetattr(
                stdin_fileno,
                &mut editor_config.orig_termios as *mut termios,
            ) == -1
            {
                die("tcgetattr");
            }

            let mut raw = editor_config.orig_termios;

            raw.c_iflag &= !(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
            raw.c_oflag &= !(OPOST);
            raw.c_cflag |= CS8;
            raw.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);

            raw.c_cc[VMIN] = 0;
            raw.c_cc[VTIME] = 1;


            if tcsetattr(stdin_fileno, TCSAFLUSH, &mut raw as *mut termios) == -1 {
                die("tcsetattr");
            }
        }
    }
}

fn editor_read_key() -> u8 {
    let mut buffer = [0; 1];
    let mut stdin = io::stdin();
    stdin
        .read_exact(&mut buffer)
        .or_else(|e| if e.kind() == ErrorKind::UnexpectedEof {
            buffer[0] = 0;
            Ok(())
        } else {
            Err(e)
        })
        .unwrap();

    buffer[0]
}

/*** output ***/

fn editor_draw_rows() {
    let mut stdout = io::stdout();

    for _ in 0..24 {
        stdout.write(b"~\r\n").unwrap_or_default();
    }
}

fn editor_refresh_screen() {
    let mut stdout = io::stdout();

    stdout.write(b"\x1b[2J").unwrap_or_default();
    stdout.write(b"\x1b[H").unwrap_or_default();

    editor_draw_rows();

    stdout.write(b"\x1b[H").unwrap_or_default();

    stdout.flush().unwrap_or_default();
}

/*** input ***/

fn editor_process_keypress() {
    let c = editor_read_key();

    if c == CTRL_KEY!(b'q') {
        let mut stdout = io::stdout();
        stdout.write(b"\x1b[2J").unwrap_or_default();
        stdout.write(b"\x1b[H").unwrap_or_default();

        stdout.flush().unwrap_or_default();

        disable_raw_mode();
        std::process::exit(0);
    }
}

/*** init ***/

fn main() {
    enable_raw_mode();

    loop {
        editor_refresh_screen();
        editor_process_keypress();
    }
}
