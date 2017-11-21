extern crate libc;

/*** includes ***/

use libc::{ioctl, perror, tcgetattr, tcsetattr, termios, winsize, CS8, BRKINT, ECHO, ICANON,
           ICRNL, IEXTEN, INPCK, ISIG, ISTRIP, IXON, OPOST, STDIN_FILENO, STDOUT_FILENO,
           TCSAFLUSH, TIOCGWINSZ, VMIN, VTIME};
use std::io::{self, ErrorKind, Read, Write};
use std::os::unix::io::AsRawFd;
use std::ffi::CString;
use std::time::{Duration, Instant};
use std::io::{BufRead, BufReader};
use std::fs::File;
use std::path::Path;

/*** defines ***/
const KILO_VERSION: &'static str = "0.0.1";
const KILO_TAB_STOP: usize = 8;
const KILO_QUIT_TIMES: u32 = 3;
const BACKSPACE: u8 = 127;

macro_rules! CTRL_KEY {
    ($k :expr) => (($k) & 0b0001_1111)
}

macro_rules! editor_set_status_message {
    ($($arg:tt)*) => {
        if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
            editor_config.status_msg.clear();
            std::fmt::write(
                &mut editor_config.status_msg,
                format_args!($($arg)*)
            ).unwrap_or_default();
            editor_config.status_msg_time = Instant::now();
        }
    }
}

//returns An Option which may contain a prompted for string
macro_rules! editor_prompt {
    ($format_str: expr) => {{
      let mut buf = String::new();
      let mut display_buf = String::new();
      let mut result = None;

      loop {
            editor_set_status_message!($format_str, buf);

            editor_refresh_screen(&mut display_buf);
            let key = editor_read_key();
            match key {
                Byte(b'\x1b') => {
                    editor_set_status_message!("");
                    break;
                }
                Byte(b'\r') => {
                    if buf.len() != 0 {
                      editor_set_status_message!("");
                      result = Some(buf);
                      break;
                    }
                }
                Byte(c) if !(c as char).is_control() => {
                    buf.push(c as char);
                }
                _ => {}
            }
      }

      result
  }}
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

struct Row {
    row: String,
    render: String,
}

struct EditorConfig {
    cx: u32,
    cy: u32,
    rx: u32,
    row_offset: u32,
    col_offset: u32,
    screen_rows: u32,
    screen_cols: u32,
    num_rows: u32,
    rows: Vec<Row>,
    dirty: bool,
    filename: Option<String>,
    status_msg: String,
    status_msg_time: Instant,
    orig_termios: termios,
}

impl Default for EditorConfig {
    fn default() -> EditorConfig {
        EditorConfig {
            cx: Default::default(),
            cy: Default::default(),
            rx: Default::default(),
            row_offset: Default::default(),
            col_offset: Default::default(),
            screen_rows: Default::default(),
            screen_cols: Default::default(),
            num_rows: Default::default(),
            rows: Default::default(),
            dirty: false,
            filename: Default::default(),
            status_msg: Default::default(),
            status_msg_time: Instant::now(),
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

fn editor_row_cx_to_rx(row: &Row, cx: u32) -> u32 {
    let mut rx = 0;

    for c in row.row.chars().take(cx as usize) {
        if c == '\t' {
            rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
        }
        rx += 1;
    }

    rx as u32
}

fn editor_update_row(row: &mut Row) {
    let mut tabs = 0;

    for c in row.row.chars() {
        if c == '\t' {
            tabs += 1;
        }
    }

    row.render = String::with_capacity(row.row.len() + tabs * (KILO_TAB_STOP - 1));

    for c in row.row.chars() {
        if c == '\t' {
            tabs += 1;
            row.render.push(' ');
            while row.render.len() % KILO_TAB_STOP != 0 {
                row.render.push(' ');
            }
        } else {
            row.render.push(c);
        }
    }
}

fn editor_insert_row(at: u32, s: String) {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        if at > editor_config.num_rows {
            return;
        }

        let mut row = Row {
            row: s,
            render: String::new(),
        };
        editor_update_row(&mut row);
        editor_config.rows.insert(at as usize, row);
        editor_config.num_rows += 1;
        editor_config.dirty = true;
    }
}

fn editor_del_row(at: u32) {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        if at >= editor_config.num_rows {
            return;
        }

        editor_config.rows.remove(at as usize);

        editor_config.num_rows -= 1;
        editor_config.dirty = true;
    }
}

fn editor_row_insert_char(row: &mut Row, at: u32, c: char) {
    //we allow at == len so we can add c to the end.
    let mut i = at as usize;
    if i > row.row.len() {
        i = row.row.len();
    }
    row.row.insert(i, c);
    editor_update_row(row);
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        editor_config.dirty = true;
    }
}

fn editor_row_append_string(row: &mut Row, s: &str) {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        row.row.push_str(s);
        editor_update_row(row);
        editor_config.dirty = true;
    }
}

fn editor_row_del_char(row: &mut Row, at: u32) {
    let i = at as usize;
    if i >= row.row.len() {
        return;
    }
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        row.row.remove(i);
        editor_update_row(row);
        editor_config.dirty = true;
    }
}

/*** editor operations ***/

fn editor_insert_char(c: char) {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        if editor_config.cy == editor_config.num_rows {
            editor_insert_row(editor_config.num_rows, String::new());
        }
        editor_row_insert_char(
            &mut editor_config.rows[editor_config.cy as usize],
            editor_config.cx,
            c,
        );
        editor_config.cx += 1;
    }
}

fn editor_insert_newline() {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        if editor_config.cx == 0 {
            editor_insert_row(editor_config.cy, String::new());
        } else {
            let row = &mut editor_config.rows[editor_config.cy as usize];
            editor_insert_row(
                editor_config.cy + 1,
                row.row.split_off(editor_config.cx as usize),
            );
            editor_update_row(row);
        }
        editor_config.cy += 1;
        editor_config.cx = 0;
    }
}

fn editor_del_char() {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        if editor_config.cy == editor_config.num_rows {
            return;
        };
        if editor_config.cx == 0 && editor_config.cy == 0 {
            return;
        };

        if editor_config.cx > 0 {
            editor_row_del_char(
                &mut editor_config.rows[editor_config.cy as usize],
                editor_config.cx - 1,
            );
            editor_config.cx -= 1;
        } else {
            {
                let (before, after) = editor_config.rows.split_at_mut(editor_config.cy as usize);
                match (before.last_mut(), after.first_mut()) {
                    (Some(previous_row), Some(row)) => {
                        editor_config.cx = previous_row.row.len() as u32;
                        editor_row_append_string(previous_row, &row.row);
                    }
                    _ => die("editor_del_char"),
                }
            }
            editor_del_row(editor_config.cy);
            editor_config.cy -= 1;
        }
    }
}

/*** file i/o ***/

fn editor_rows_to_string() -> String {
    let mut buf = String::new();
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        for row in editor_config.rows.iter() {
            buf.push_str(&row.row);
            buf.push('\n');
        }
    }
    buf
}

fn editor_open<P: AsRef<Path>>(filename: P) {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        editor_config.filename = Some(format!("{}", filename.as_ref().display()));

        if let Ok(file) = File::open(filename) {
            for res in BufReader::new(file).lines() {
                match res {
                    Ok(mut line) => {
                        while line.ends_with(|c| c == '\n' || c == '\r') {
                            line.pop();
                        }
                        editor_insert_row(editor_config.num_rows, line);
                    }
                    Err(e) => {
                        die(&e.to_string());
                    }
                }
            }
        } else {
            die("editor_open");
        }
        editor_config.dirty = false;
    }
}

fn editor_save() {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        if editor_config.filename.is_none() {
            editor_config.filename = editor_prompt!("Save as: {}");
        }

        if let Some(filename) = editor_config.filename.as_ref() {
            use std::fs::OpenOptions;

            let s = editor_rows_to_string();
            let data = s.as_bytes();
            let len = data.len();
            match OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(filename)
            {
                Ok(mut file) => if let Ok(()) = file.write_all(data) {
                    editor_config.dirty = false;
                    editor_set_status_message!("{} bytes written to disk", len);
                },
                Err(err) => {
                    editor_set_status_message!("Can't save! I/O error: {}", err);
                }
            }
        } else {
            editor_set_status_message!("Save aborted");
        }
    }
}

/*** output ***/

fn editor_scroll() {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        editor_config.rx = 0;
        if editor_config.cy < editor_config.num_rows {
            editor_config.rx = editor_row_cx_to_rx(
                &editor_config.rows[editor_config.cy as usize],
                editor_config.cx,
            )
        }

        if editor_config.cy < editor_config.row_offset {
            editor_config.row_offset = editor_config.cy;
        }
        if editor_config.cy >= editor_config.row_offset + editor_config.screen_rows {
            editor_config.row_offset = editor_config.cy - editor_config.screen_rows + 1;
        }
        if editor_config.rx < editor_config.col_offset {
            editor_config.col_offset = editor_config.rx;
        }
        if editor_config.rx >= editor_config.col_offset + editor_config.screen_cols {
            editor_config.col_offset = editor_config.rx - editor_config.screen_cols + 1;
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
                        .render
                        .len()
                        .saturating_sub(editor_config.col_offset as _),
                    editor_config.screen_cols as usize,
                );

                for (i, c) in editor_config.rows[file_row as usize]
                    .render
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

            buf.push_str("\r\n");
        }
    }
}

fn editor_draw_status_bar(buf: &mut String) {
    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        buf.push_str("\x1b[7m");

        let name = match &editor_config.filename {
            &Some(ref f_n) => f_n,
            &None => "[No Name]",
        };

        let status = format!(
            "{:.20} - {} lines {}",
            name,
            editor_config.num_rows,
            if editor_config.dirty {
                "(modified)"
            } else {
                ""
            }
        );
        let r_status = format!("{}/{}", editor_config.cy + 1, editor_config.num_rows);

        buf.push_str(&status);

        let screen_cols = editor_config.screen_cols as usize;
        let mut len = std::cmp::min(status.len(), screen_cols);
        let rlen = r_status.len();
        while len < screen_cols {
            if screen_cols - len == rlen {
                buf.push_str(&r_status);
                break;
            }
            buf.push(' ');
            len += 1;
        }

        buf.push_str("\x1b[m");
        buf.push_str("\r\n");
    }
}

fn editor_draw_message_bar(buf: &mut String) {
    buf.push_str("\x1b[K");

    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        let msglen = std::cmp::min(
            editor_config.status_msg.len(),
            editor_config.screen_cols as usize,
        );

        if msglen > 0
            && Instant::now().duration_since(editor_config.status_msg_time) < Duration::from_secs(5)
        {
            buf.push_str(&editor_config.status_msg[..msglen]);
        }
    }
}

fn editor_refresh_screen(buf: &mut String) {
    editor_scroll();
    buf.clear();

    buf.push_str("\x1b[?25l");
    buf.push_str("\x1b[H");

    editor_draw_rows(buf);
    editor_draw_status_bar(buf);
    editor_draw_message_bar(buf);

    if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
        buf.push_str(&format!(
            "\x1b[{};{}H",
            (editor_config.cy - editor_config.row_offset) + 1,
            (editor_config.rx - editor_config.col_offset) + 1
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
        let row_len = if editor_config.cy < editor_config.num_rows {
            Some(editor_config.rows[editor_config.cy as usize].row.len())
        } else {
            None
        };

        match arrow {
            Arrow::Left => if editor_config.cx != 0 {
                editor_config.cx -= 1;
            } else if editor_config.cy > 0 {
                editor_config.cy -= 1;
                editor_config.cx = editor_config.rows[editor_config.cy as usize].row.len() as u32;
            },
            Arrow::Right => match row_len {
                Some(len) if (editor_config.cx as usize) < len => {
                    editor_config.cx += 1;
                }
                Some(len) if (editor_config.cx as usize) == len => {
                    editor_config.cy += 1;
                    editor_config.cx = 0;
                }
                _ => {}
            },
            Arrow::Up => {
                editor_config.cy = editor_config.cy.saturating_sub(1);
            }
            Arrow::Down => if editor_config.cy < editor_config.num_rows {
                editor_config.cy += 1;
            },
        }


        let new_row_len = if editor_config.cy < editor_config.num_rows {
            editor_config.rows[editor_config.cy as usize].row.len() as u32
        } else {
            0
        };
        if editor_config.cx > new_row_len {
            editor_config.cx = new_row_len;
        }
    }
}

fn editor_process_keypress() {
    static mut QUIT_TIMES: u32 = KILO_QUIT_TIMES;
    let key = editor_read_key();

    const CTRL_H: u8 = CTRL_KEY!(b'h');

    match key {
        Byte(b'\r') => editor_insert_newline(),
        Byte(c0) if c0 == CTRL_KEY!(b'q') => {
            if unsafe { EDITOR_CONFIG.as_mut() }
                .map(|e| e.dirty)
                .unwrap_or(true) && unsafe { QUIT_TIMES > 0 }
            {
                editor_set_status_message!(
                    "WARNING!!! File has unsaved changes. Press Ctrl-Q {} more times to quit.",
                    unsafe { QUIT_TIMES }
                );
                unsafe {
                    QUIT_TIMES -= 1;
                }
                return;
            }

            let mut stdout = io::stdout();
            stdout.write(b"\x1b[2J").unwrap_or_default();
            stdout.write(b"\x1b[H").unwrap_or_default();

            stdout.flush().unwrap_or_default();

            disable_raw_mode();
            std::process::exit(0);
        }
        Byte(c0) if c0 == CTRL_KEY!(b's') => {
            editor_save();
        }
        Home => if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
            editor_config.cx = 0;
        },
        End => if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
            if editor_config.cy < editor_config.num_rows {
                editor_config.cx = editor_config.rows[editor_config.cy as usize].row.len() as u32;
            }
        },
        Byte(BACKSPACE) | Delete | Byte(CTRL_H) => {
            match key {
                Delete => {
                    editor_move_cursor(Arrow::Right);
                }
                _ => {}
            }
            editor_del_char();
        }
        Page(page) => if let Some(editor_config) = unsafe { EDITOR_CONFIG.as_mut() } {
            match page {
                Page::Up => {
                    editor_config.cy = editor_config.row_offset;
                }
                Page::Down => {
                    editor_config.cy = editor_config.row_offset + editor_config.screen_rows - 1;
                    if editor_config.cy > editor_config.num_rows {
                        editor_config.cy = editor_config.num_rows;
                    }
                }
            };


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
        Byte(c0) if c0 == CTRL_KEY!(b'l') || c0 == b'\x1b' => {}
        Byte(c0) if c0 == 0 => {
            return;
        }
        Byte(c0) => {
            editor_insert_char(c0 as char);
        }
    }
    unsafe {
        QUIT_TIMES = KILO_QUIT_TIMES;
    }
}

/*** init ***/

fn init_editor() {
    let mut editor_config: EditorConfig = Default::default();
    match get_window_size() {
        None => die("get_window_size"),
        Some((rows, cols)) => {
            //leave room for the status bar
            editor_config.screen_rows = rows - 2;
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

    editor_set_status_message!("HELP: Ctrl-S = save | Ctrl-Q = quit");

    let mut buf = String::new();

    loop {
        editor_refresh_screen(&mut buf);
        editor_process_keypress();
    }
}
