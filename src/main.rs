extern crate libc;

/*** includes ***/

use libc::{ioctl, iscntrl, perror, tcgetattr, tcsetattr, termios, winsize, CS8, BRKINT, ECHO,
           ICANON, ICRNL, IEXTEN, INPCK, ISIG, ISTRIP, IXON, OPOST, STDIN_FILENO, STDOUT_FILENO,
           TCSAFLUSH, TIOCGWINSZ, VMIN, VTIME};
use std::io::{self, ErrorKind, Read, Write};
use std::os::unix::io::AsRawFd;
use std::ffi::CString;

/*** defines ***/
macro_rules! CTRL_KEY {
    ($k :expr) => (($k) & 0b0001_1111)
}


/*** data ***/

struct EditorConfig {
    screen_rows: u32,
    screen_cols: u32,
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
        if let Some(editor_config) = EDITOR_CONFIG.as_mut() {
            if tcgetattr(
                STDIN_FILENO,
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


            if tcsetattr(STDIN_FILENO, TCSAFLUSH, &mut raw as *mut termios) == -1 {
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

fn get_cursor_position() -> Option<(u32, u32)> {
    let mut stdout = io::stdout();
    if stdout.write(b"\x1b[6n").is_err() || stdout.flush().is_err() {
        return None;
    }

    print!("\r\n");

    let mut buffer = [0; 32];
    let mut i = 0;
    while i < buffer.len() {
        if io::stdin().read_exact(&mut buffer[i..i + 1]).is_err() {
            break;
        }

        if buffer[i] == b'R' {
            break;
        }

        i += 1;
    }

    if buffer[0] == b'\x1b' && buffer[1] == b'[' {
        if let Ok(s) = std::str::from_utf8(&buffer[2..i]) {
            let mut split = s.split(";").map(str::parse::<u32>);

            match (split.next(), split.next()) {
                (Some(Ok(rows)), Some(Ok(cols))) => {
                    return Some((rows, cols));
                }
                _ => {}
            }
        }
    }

    None
}

fn get_window_size() -> Option<(u32, u32)> {
    unsafe {
        let mut ws: winsize = std::mem::zeroed();
        if ioctl(STDOUT_FILENO, TIOCGWINSZ, &mut ws) == -1 || ws.ws_col == 0 {
            let mut stdout = io::stdout();
            if stdout.write(b"\x1b[999C\x1b[999B").is_err() || stdout.flush().is_err() {
                return None;
            }
            get_cursor_position()
        } else {
            Some((ws.ws_row as u32, ws.ws_col as u32))
        }
    }
}

/*** output ***/

fn editor_draw_rows(buf: &mut String) {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        for y in 0..editor_config.screen_rows {
            buf.push('~');

            if y < editor_config.screen_rows - 1 {
                buf.push_str("\r\n");
            }
        }
    }
}

fn editor_refresh_screen(buf: &mut String) {
    buf.clear();

    buf.push_str("\x1b[?25l");
    buf.push_str("\x1b[2J");
    buf.push_str("\x1b[H");

    editor_draw_rows(buf);

    buf.push_str("\x1b[H");
    buf.push_str("\x1b[?25h");

    let mut stdout = io::stdout();
    stdout.write(buf.as_bytes()).unwrap_or_default();
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

fn init_editor() {
    let mut editor_config: EditorConfig = unsafe { std::mem::zeroed() };
    match get_window_size() {
        None => die("get_window_size"),
        Some((rows, cols)) => {
            editor_config.screen_rows = rows;
            editor_config.screen_cols = cols;
        }
    }
    unsafe {
        EDITOR_CONFIG = Some(editor_config);
    }
}

fn main() {
    init_editor();
    enable_raw_mode();

    let mut buf = String::new();

    loop {
        editor_refresh_screen(&mut buf);
        editor_process_keypress();
    }
}
