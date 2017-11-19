extern crate libc;

/*** includes ***/

use libc::{ioctl, perror, tcgetattr, tcsetattr, termios, winsize, CS8, BRKINT, ECHO, ICANON,
           ICRNL, IEXTEN, INPCK, ISIG, ISTRIP, IXON, OPOST, STDIN_FILENO, STDOUT_FILENO,
           TCSAFLUSH, TIOCGWINSZ, VMIN, VTIME};
use std::io::{self, ErrorKind, Read, Write};
use std::os::unix::io::AsRawFd;
use std::ffi::CString;

/*** defines ***/
const KILO_VERSION: &'static str = "0.0.1";

macro_rules! CTRL_KEY {
    ($k :expr) => (($k) & 0b0001_1111)
}

#[derive(Clone, Copy)]
enum EditorKey {
    Byte(u8),
    Arrow(Arrow),
    Page(Page),
    Delete,
    Home,
    End,
}
use EditorKey::*;

#[derive(Clone, Copy)]
enum Arrow {
    Left,
    Right,
    Up,
    Down,
}

#[derive(Clone, Copy)]
enum Page {
    Up,
    Down,
}

/*** data ***/

type Row = String;

struct EditorConfig {
    cx: u32,
    cy: u32,
    row_offset: u32,
    col_offset: u32,
    screen_rows: u32,
    screen_cols: u32,
    num_rows: u32,
    rows: Vec<Row>,
    orig_termios: termios,
}

impl Default for EditorConfig {
    fn default() -> EditorConfig {
        EditorConfig {
            cx: Default::default(),
            cy: Default::default(),
            row_offset: Default::default(),
            col_offset: Default::default(),
            screen_rows: Default::default(),
            screen_cols: Default::default(),
            num_rows: Default::default(),
            rows: Default::default(),
            orig_termios: unsafe { std::mem::zeroed() },
        }
    }
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

fn editor_read_key() -> EditorKey {
    let mut buffer = [0; 1];
    let mut stdin = io::stdin();
    stdin
        .read_exact(&mut buffer)
        .or_else(|e| {
            if e.kind() == ErrorKind::UnexpectedEof {
                buffer[0] = 0;
                Ok(())
            } else {
                Err(e)
            }
        })
        .unwrap();

    let c = buffer[0];

    if c == b'\x1b' {
        let mut seq = [0; 3];

        if stdin.read_exact(&mut seq[0..1]).is_err() {
            return Byte(b'\x1b');
        }
        if stdin.read_exact(&mut seq[1..2]).is_err() {
            return Byte(b'\x1b');
        }
        if seq[0] == b'[' {
            match seq[1] {
                c if c >= b'0' && c <= b'9' => {
                    if stdin.read_exact(&mut seq[2..3]).is_err() {
                        return Byte(b'\x1b');
                    }
                    if seq[2] == b'~' {
                        match c {
                            b'3' => return Delete,
                            b'5' => return Page(Page::Up),
                            b'6' => return Page(Page::Down),
                            b'1' | b'7' => return Home,
                            b'4' | b'8' => return End,
                            _ => {}
                        }
                    }
                }
                b'A' => {
                    return Arrow(Arrow::Up);
                }
                b'B' => {
                    return Arrow(Arrow::Down);
                }
                b'C' => {
                    return Arrow(Arrow::Right);
                }
                b'D' => {
                    return Arrow(Arrow::Left);
                }
                b'H' => {
                    return Home;
                }
                b'F' => {
                    return End;
                }
                _ => {}
            }
        } else if seq[0] == b'O' {
            match seq[1] {
                b'H' => {
                    return Home;
                }
                b'F' => {
                    return End;
                }
                _ => {}
            }
        }

        Byte(b'\x1b')
    } else {
        Byte(c)
    }
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

/*** row operations ***/

fn editor_append_row(row: String) {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        editor_config.rows.push(row);
        editor_config.num_rows += 1;
    }
}

/*** file i/o ***/
use std::io::{BufRead, BufReader};
use std::fs::File;
use std::path::Path;

fn editor_open<P: AsRef<Path>>(filename: P) {
    if let Ok(file) = File::open(filename) {
        for res in BufReader::new(file).lines() {
            match res {
                Ok(mut line) => {
                    while line.ends_with(|c| c == '\n' || c == '\r') {
                        line.pop();
                    }
                    editor_append_row(line);
                }
                Err(e) => {
                    die(&e.to_string());
                }
            }
        }
    } else {
        die("editor_open");
    }
}

/*** output ***/

fn editor_scroll() {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        if editor_config.cy < editor_config.row_offset {
            editor_config.row_offset = editor_config.cy;
        }
        if editor_config.cy >= editor_config.row_offset + editor_config.screen_rows {
            editor_config.row_offset = editor_config.cy - editor_config.screen_rows + 1;
        }
        if editor_config.cx < editor_config.col_offset {
            editor_config.col_offset = editor_config.cx;
        }
        if editor_config.cx >= editor_config.col_offset + editor_config.screen_cols {
            editor_config.col_offset = editor_config.cx - editor_config.screen_cols + 1;
        }
    }
}

fn editor_draw_rows(buf: &mut String) {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        for y in 0..editor_config.screen_rows {
            let file_row = y + editor_config.row_offset;
            if file_row >= editor_config.num_rows {
                if editor_config.num_rows == 0 && y == editor_config.screen_rows / 3 {
                    let mut welcome = format!("Kilo editor -- version {}", KILO_VERSION);
                    let mut padding = (editor_config.screen_cols as usize - welcome.len()) / 2;

                    if padding > 0 {
                        buf.push('~');
                        padding -= 1;
                    }
                    for _ in 0..padding {
                        buf.push(' ');
                    }

                    welcome.truncate(editor_config.screen_cols as _);
                    buf.push_str(&welcome);
                } else {
                    buf.push('~');
                }
            } else {
                let mut len = std::cmp::min(
                    editor_config.rows[file_row as usize]
                        .len()
                        .saturating_sub(editor_config.col_offset as _),
                    editor_config.screen_cols as usize,
                );

                for (i, c) in editor_config.rows[file_row as usize]
                    .chars()
                    .skip(editor_config.col_offset as _)
                    .enumerate()
                {
                    if i >= len {
                        break;
                    }

                    buf.push(c);
                }
            }

            buf.push_str("\x1b[K");
            if y < editor_config.screen_rows - 1 {
                buf.push_str("\r\n");
            }
        }
    }
}

fn editor_refresh_screen(buf: &mut String) {
    editor_scroll();
    buf.clear();

    buf.push_str("\x1b[?25l");
    buf.push_str("\x1b[H");

    editor_draw_rows(buf);

    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        buf.push_str(&format!(
            "\x1b[{};{}H",
            (editor_config.cy - editor_config.row_offset) + 1,
            editor_config.cx + 1
        ));
    }

    buf.push_str("\x1b[?25h");

    let mut stdout = io::stdout();
    stdout.write(buf.as_bytes()).unwrap_or_default();
    stdout.flush().unwrap_or_default();
}

/*** input ***/

fn editor_move_cursor(arrow: Arrow) {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        match arrow {
            Arrow::Left => {
                editor_config.cx = editor_config.cx.saturating_sub(1);
            }
            Arrow::Right => {
                editor_config.cx += 1;
            }
            Arrow::Up => {
                editor_config.cy = editor_config.cy.saturating_sub(1);
            }
            Arrow::Down => if editor_config.cy < editor_config.num_rows {
                editor_config.cy += 1;
            },
        }
    }
}

fn editor_process_keypress() {
    let key = editor_read_key();

    match key {
        Byte(c0) if c0 == CTRL_KEY!(b'q') => {
            let mut stdout = io::stdout();
            stdout.write(b"\x1b[2J").unwrap_or_default();
            stdout.write(b"\x1b[H").unwrap_or_default();

            stdout.flush().unwrap_or_default();

            disable_raw_mode();
            std::process::exit(0);
        }
        Home => if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
            editor_config.cx = 0;
        },
        End => if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
            editor_config.cx = editor_config.screen_cols - 1;
        },
        Page(page) => if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
            let arrow = match page {
                Page::Up => Arrow::Up,
                Page::Down => Arrow::Down,
            };

            for _ in 0..editor_config.screen_rows {
                editor_move_cursor(arrow);
            }
        },
        Arrow(arrow) => {
            editor_move_cursor(arrow);
        }
        _ => {}
    }
}

/*** init ***/

fn init_editor() {
    let mut editor_config: EditorConfig = Default::default();
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

    let mut args = std::env::args();
    //skip binary name
    args.next();
    if let Some(filename) = args.next() {
        editor_open(filename);
    }
    enable_raw_mode();

    let mut buf = String::new();

    loop {
        editor_refresh_screen(&mut buf);
        editor_process_keypress();
    }
}
