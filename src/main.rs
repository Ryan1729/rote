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

const CTRL_H: u8 = CTRL_KEY!(b'h');

macro_rules! set_status_message {
    ($($arg:tt)*) => {
        if let Some(state) = unsafe { STATE.as_mut() } {
            state.status_msg.clear();
            std::fmt::write(
                &mut state.status_msg,
                format_args!($($arg)*)
            ).unwrap_or_default();
            state.status_msg_time = Instant::now();
        }
    }
}

//returns An Option which may contain a prompted for string
macro_rules! prompt {
    ($format_str: expr) => {prompt!($format_str, None)};
    ($format_str: expr, $callback: expr) => {{
      let mut buf = String::new();
      let mut display_buf = String::new();
      let mut result = None;

      let callback : Option<&Fn(&str, EditorKey)> = $callback;

      loop {
            set_status_message!($format_str, buf);
            refresh_screen(&mut display_buf);

            let key = read_key();
            match key {

                Byte(BACKSPACE) | Delete | Byte(CTRL_H) => {
                    buf.pop();
                }

                Byte(b'\x1b') => {
                    set_status_message!("");
                    if let Some(cb) = callback {
                        cb(&mut buf, key);
                    }
                    break;
                }
                Byte(b'\r') => {
                    if buf.len() != 0 {
                      set_status_message!("");
                      if let Some(cb) = callback {
                          cb(&mut buf, key);
                      }
                      result = Some(buf);
                      break;
                    }
                }
                Byte(c) if !(c as char).is_control() => {
                    buf.push(c as char);
                }
                _ => {}
            }

            match key {
                Byte(0) => {}
                _ => {
                    if let Some(cb) = callback {
                        cb(&mut buf, key);
                    }
                }
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

#[derive(Clone, Copy, PartialEq)]
enum EditorHighlight {
    Normal,
    Comment,
    MultilineComment,
    Keyword1,
    Keyword2,
    String,
    Number,
    Match,
}

const HL_HIGHLIGHT_NUMBERS: u32 = 1 << 0;
const HL_HIGHLIGHT_STRINGS: u32 = 1 << 1;

/*** data ***/

#[derive(Clone)]
struct EditorSyntax {
    file_type: &'static str,
    file_match: [Option<&'static str>; 8],
    singleline_comment_start: &'static str,
    multiline_comment_start: &'static str,
    multiline_comment_end: &'static str,
    flags: u32,
    keywords1: [Option<&'static str>; 32],
    keywords2: [Option<&'static str>; 32],
    keywords3: [Option<&'static str>; 32],
    keywords4: [Option<&'static str>; 32],
}

struct Row {
    index: u32,
    row: String,
    render: String,
    highlight: Vec<EditorHighlight>,
    highlight_open_comment: bool,
}

#[derive(Default)]
struct EditBuffer {
    cx: u32,
    cy: u32,
    rx: u32,
    row_offset: u32,
    col_offset: u32,
    rows: Vec<Row>,
    dirty: bool,
    filename: Option<String>,
}

struct EditorState {
    edit_buffer: EditBuffer,
    screen_rows: u32,
    screen_cols: u32,
    status_msg: String,
    status_msg_time: Instant,
    syntax: Option<EditorSyntax>,
    orig_termios: termios,
}

impl Default for EditorState {
    fn default() -> EditorState {
        EditorState {
            edit_buffer: Default::default(),
            screen_rows: Default::default(),
            screen_cols: Default::default(),
            status_msg: Default::default(),
            status_msg_time: Instant::now(),
            syntax: Default::default(),
            orig_termios: unsafe { std::mem::zeroed() },
        }
    }
}

// This is a reasonably nice way to have a "uninitialized/zeroed" global,
// given what is stable in Rust 1.21.0+
static mut STATE: Option<EditorState> = None;

/*** filetypes ***/

const HLDB: [EditorSyntax; 1] = [
    EditorSyntax {
        file_type: "c",
        file_match: [
            Some(".c"),
            Some(".h"),
            Some(".cpp"),
            None,
            None,
            None,
            None,
            None,
        ],
        singleline_comment_start: "//",
        multiline_comment_start: "/*",
        multiline_comment_end: "*/",
        flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        keywords1: [
            Some("switch"),
            Some("if"),
            Some("while"),
            Some("for"),
            Some("break"),
            Some("continue"),
            Some("return"),
            Some("else"),
            Some("struct"),
            Some("union"),
            Some("typedef"),
            Some("static"),
            Some("enum"),
            Some("class"),
            Some("case"),
            Some("int|"),
            Some("long|"),
            Some("double|"),
            Some("float|"),
            Some("char|"),
            Some("unsigned|"),
            Some("signed|"),
            Some("void|"),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        ],
        keywords2: [None; 32],
        keywords3: [None; 32],
        keywords4: [None; 32],
    },
];


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
    if let Some(state) = unsafe { STATE.as_mut() } {
        unsafe {
            if tcsetattr(
                io::stdin().as_raw_fd(),
                TCSAFLUSH,
                &mut state.orig_termios as *mut termios,
            ) == -1
            {
                die("tcsetattr");
            }
        }
    }
}

fn enable_raw_mode() {
    unsafe {
        if let Some(state) = STATE.as_mut() {
            if tcgetattr(STDIN_FILENO, &mut state.orig_termios as *mut termios) == -1 {
                die("tcgetattr");
            }

            let mut raw = state.orig_termios;

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

fn read_key() -> EditorKey {
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

/*** syntax highlighting ***/

fn is_separator(c: char) -> bool {
    c.is_whitespace() || c == '\0' || ",.()+-/*=~%<>[];".contains(c)
}

fn update_syntax(row: &mut Row) {
    row.highlight.clear();
    let extra_needed = row.render.len().saturating_sub(row.highlight.capacity());
    if extra_needed != 0 {
        row.highlight.reserve(extra_needed);
    }

    if let Some(state) = unsafe { STATE.as_mut() } {
        if let Some(ref syntax) = state.syntax {
            let mut prev_sep = true;
            let mut in_string = None;
            let mut in_comment = row.index > 0
                && state.edit_buffer.rows[(row.index - 1) as usize].highlight_open_comment;

            let mut char_indices = row.render.char_indices();

            'char_indices: while let Some((i, c)) = char_indices.next() {
                let prev_highlight = if i > 0 {
                    row.highlight[i - 1]
                } else {
                    EditorHighlight::Normal
                };

                if syntax.singleline_comment_start.len() > 0 && in_string.is_none() && !in_comment {
                    if row.render[i..].starts_with(syntax.singleline_comment_start) {
                        for _ in i..row.render.len() {
                            row.highlight.push(EditorHighlight::Comment);
                        }
                        break;
                    }
                }

                if syntax.multiline_comment_start.len() > 0
                    && syntax.multiline_comment_end.len() > 0
                    && in_string.is_none()
                {
                    if in_comment {
                        if (&row.render[i..]).starts_with(syntax.multiline_comment_end) {
                            let one_past_comment_end = i + syntax.multiline_comment_end.len();
                            for j in i..one_past_comment_end {
                                row.highlight.push(EditorHighlight::MultilineComment);
                                if j < one_past_comment_end - 2 {
                                    char_indices.next();
                                }
                            }

                            in_comment = false;
                            prev_sep = true;
                        } else {
                            row.highlight.push(EditorHighlight::MultilineComment);
                        }
                        continue;
                    } else if (&row.render[i..]).starts_with(syntax.multiline_comment_start) {
                        let one_past_comment_start = i + syntax.multiline_comment_start.len();
                        for j in i..one_past_comment_start {
                            row.highlight.push(EditorHighlight::MultilineComment);
                            if j < one_past_comment_start - 1 {
                                char_indices.next();
                            }
                        }


                        in_comment = true;
                        continue;
                    }
                }

                if syntax.flags & HL_HIGHLIGHT_STRINGS != 0 {
                    if let Some(delim) = in_string {
                        row.highlight.push(EditorHighlight::String);
                        if c == '\\' && i + 1 < row.render.len() {
                            row.highlight.push(EditorHighlight::String);
                            char_indices.next();
                        }

                        if c == delim {
                            in_string = None;
                        }

                        prev_sep = true;
                        continue;
                    } else {
                        if c == '"' || c == '\'' {
                            in_string = Some(c);
                            row.highlight.push(EditorHighlight::String);

                            continue;
                        }
                    }
                }

                if syntax.flags & HL_HIGHLIGHT_NUMBERS != 0 {
                    if c.is_digit(10) && (prev_sep || prev_highlight == EditorHighlight::Number)
                        || (c == '.' && prev_highlight == EditorHighlight::Number)
                    {
                        row.highlight.push(EditorHighlight::Number);
                        prev_sep = false;
                        continue;
                    }
                }

                if prev_sep {
                    let mut keywords = syntax
                        .keywords1
                        .iter()
                        .chain(syntax.keywords2.iter())
                        .chain(syntax.keywords3.iter())
                        .chain(syntax.keywords4.iter());
                    while let Some(&Some(ref keyword)) = keywords.next() {
                        let mut k_len = keyword.len();
                        let is_kw2 = keyword.ends_with('|');
                        if is_kw2 {
                            k_len -= 1;
                        }
                        let one_past_keyword = i + k_len;
                        if (&row.render[i..]).starts_with(&keyword[..k_len])
                            && row.render[one_past_keyword..one_past_keyword + 1]
                                .chars()
                                .next()
                                .map(is_separator)
                                .unwrap_or(false)
                        {
                            for j in i..one_past_keyword {
                                row.highlight.push(if is_kw2 {
                                    EditorHighlight::Keyword2
                                } else {
                                    EditorHighlight::Keyword1
                                });
                                if j < one_past_keyword - 1 {
                                    char_indices.next();
                                }
                            }

                            prev_sep = false;
                            continue 'char_indices;
                        }
                    }
                }

                row.highlight.push(EditorHighlight::Normal);
                prev_sep = is_separator(c);
            }

            let changed = row.highlight_open_comment != in_comment;
            row.highlight_open_comment = in_comment;
            if changed && row.index + 1 < state.edit_buffer.rows.len() as u32 {
                update_syntax(&mut state.edit_buffer.rows[(row.index + 1) as usize]);
            }
        } else {
            for _ in 0..row.render.len() {
                row.highlight.push(EditorHighlight::Normal);
            }
        }
    } else {
        for _ in 0..row.render.len() {
            row.highlight.push(EditorHighlight::Normal);
        }
    }
}

fn syntax_to_color(highlight: EditorHighlight) -> i32 {
    match highlight {
        EditorHighlight::Comment | EditorHighlight::MultilineComment => 36,
        EditorHighlight::Keyword1 => 33,
        EditorHighlight::Keyword2 => 32,
        EditorHighlight::String => 35,
        EditorHighlight::Number => 31,
        EditorHighlight::Match => 34,
        EditorHighlight::Normal => 37,
    }
}

fn select_syntax_highlight() {
    if let Some(state) = unsafe { STATE.as_mut() } {
        state.syntax = None;
        if let Some(ref filename) = state.edit_buffer.filename {
            for s in HLDB.iter() {
                let mut i = 0;
                while let Some(ref file_match) = s.file_match[i] {
                    let is_ext = file_match.starts_with('.');
                    if (is_ext && filename.ends_with(file_match))
                        || (!is_ext && filename.contains(file_match))
                    {
                        state.syntax = Some(s.clone());

                        for row in state.edit_buffer.rows.iter_mut() {
                            update_syntax(row);
                        }

                        return;
                    }
                    i += 1;
                    if i >= file_match.len() {
                        return;
                    }
                }
            }
        }
    }
}

/*** row operations ***/

fn row_cx_to_rx(row: &Row, cx: u32) -> u32 {
    let mut rx = 0;

    for c in row.row.chars().take(cx as usize) {
        if c == '\t' {
            rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
        }
        rx += 1;
    }

    rx as u32
}

fn row_rx_to_cx(row: &Row, rx: u32) -> u32 {
    let rx_usize = rx as usize;
    let mut cur_rx = 0;

    for (cx, c) in row.row.char_indices() {
        if c == '\t' {
            cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
        }
        cur_rx += 1;
        if cur_rx > rx_usize {
            return cx as u32;
        }
    }
    return row.row.len() as u32;
}

fn update_row(row: &mut Row) {
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

    update_syntax(row);
}

fn insert_row(at: u32, s: String) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        if at > state.edit_buffer.rows.len() as u32 {
            return;
        }

        let s_capacity = s.capacity();

        let mut row = Row {
            index: at,
            row: s,
            render: String::with_capacity(s_capacity),
            highlight: Vec::with_capacity(s_capacity),
            highlight_open_comment: false,
        };
        update_row(&mut row);
        state.edit_buffer.rows.insert(at as usize, row);

        for i in (at + 1) as usize..state.edit_buffer.rows.len() {
            state.edit_buffer.rows[i as usize].index += 1;
        }

        state.edit_buffer.dirty = true;
    }
}

fn del_row(at: u32) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        if at >= state.edit_buffer.rows.len() as u32 {
            return;
        }

        state.edit_buffer.rows.remove(at as usize);

        for i in at as usize..state.edit_buffer.rows.len() {
            state.edit_buffer.rows[i as usize].index -= 1;
        }

        state.edit_buffer.dirty = true;
    }
}

fn row_insert_char(row: &mut Row, at: u32, c: char) {
    //we allow at == len so we can add c to the end.
    let mut i = at as usize;
    if i > row.row.len() {
        i = row.row.len();
    }
    row.row.insert(i, c);
    update_row(row);
    if let Some(state) = unsafe { STATE.as_mut() } {
        state.edit_buffer.dirty = true;
    }
}

fn row_append_string(row: &mut Row, s: &str) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        row.row.push_str(s);
        update_row(row);
        state.edit_buffer.dirty = true;
    }
}

fn row_del_char(row: &mut Row, at: u32) {
    let i = at as usize;
    if i >= row.row.len() {
        return;
    }
    if let Some(state) = unsafe { STATE.as_mut() } {
        row.row.remove(i);
        update_row(row);
        state.edit_buffer.dirty = true;
    }
}

/*** editor operations ***/

fn insert_char(c: char) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        if state.edit_buffer.cy == state.edit_buffer.rows.len() as u32 {
            insert_row(state.edit_buffer.rows.len() as u32, String::new());
        }
        row_insert_char(
            &mut state.edit_buffer.rows[state.edit_buffer.cy as usize],
            state.edit_buffer.cx,
            c,
        );
        state.edit_buffer.cx += 1;
    }
}

fn insert_newline() {
    if let Some(state) = unsafe { STATE.as_mut() } {
        if state.edit_buffer.cx == 0 {
            insert_row(state.edit_buffer.cy, String::new());
        } else {
            let row = &mut state.edit_buffer.rows[state.edit_buffer.cy as usize];
            insert_row(
                state.edit_buffer.cy + 1,
                row.row.split_off(state.edit_buffer.cx as usize),
            );
            update_row(row);
        }
        state.edit_buffer.cy += 1;
        state.edit_buffer.cx = 0;
    }
}

fn del_char() {
    if let Some(state) = unsafe { STATE.as_mut() } {
        if state.edit_buffer.cy == state.edit_buffer.rows.len() as u32 {
            return;
        };
        if state.edit_buffer.cx == 0 && state.edit_buffer.cy == 0 {
            return;
        };

        if state.edit_buffer.cx > 0 {
            row_del_char(
                &mut state.edit_buffer.rows[state.edit_buffer.cy as usize],
                state.edit_buffer.cx - 1,
            );
            state.edit_buffer.cx -= 1;
        } else {
            {
                let (before, after) = state
                    .edit_buffer
                    .rows
                    .split_at_mut(state.edit_buffer.cy as usize);
                match (before.last_mut(), after.first_mut()) {
                    (Some(previous_row), Some(row)) => {
                        state.edit_buffer.cx = previous_row.row.len() as u32;
                        row_append_string(previous_row, &row.row);
                    }
                    _ => die("del_char"),
                }
            }
            del_row(state.edit_buffer.cy);
            state.edit_buffer.cy -= 1;
        }
    }
}

/*** file i/o ***/

fn rows_to_string() -> String {
    let mut buf = String::new();
    if let Some(state) = unsafe { STATE.as_mut() } {
        for row in state.edit_buffer.rows.iter() {
            buf.push_str(&row.row);
            buf.push('\n');
        }
    }
    buf
}

fn open<P: AsRef<Path>>(filename: P) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        state.edit_buffer.filename = Some(format!("{}", filename.as_ref().display()));

        select_syntax_highlight();

        if let Ok(file) = File::open(filename) {
            for res in BufReader::new(file).lines() {
                match res {
                    Ok(mut line) => {
                        while line.ends_with(|c| c == '\n' || c == '\r') {
                            line.pop();
                        }
                        insert_row(state.edit_buffer.rows.len() as u32, line);
                    }
                    Err(e) => {
                        die(&e.to_string());
                    }
                }
            }
        } else {
            die("open");
        }
        state.edit_buffer.dirty = false;
    }
}

fn save() {
    if let Some(state) = unsafe { STATE.as_mut() } {
        if state.edit_buffer.filename.is_none() {
            state.edit_buffer.filename = prompt!("Save as: {}");
            select_syntax_highlight();
        }

        if let Some(filename) = state.edit_buffer.filename.as_ref() {
            use std::fs::OpenOptions;

            let s = rows_to_string();
            let data = s.as_bytes();
            let len = data.len();
            match OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(filename)
            {
                Ok(mut file) => if let Ok(()) = file.write_all(data) {
                    state.edit_buffer.dirty = false;
                    set_status_message!("{} bytes written to disk", len);
                },
                Err(err) => {
                    set_status_message!("Can't save! I/O error: {}", err);
                }
            }
        } else {
            set_status_message!("Save aborted");
        }
    }
}

/*** find ***/

fn find_callback(query: &str, key: EditorKey) {
    static mut LAST_MATCH: i32 = -1;
    static mut FORWARD: bool = true;

    static mut SAVED_HIGHLIGHT_LINE: u32 = 0;
    static mut SAVED_HIGHLIGHT: Option<Vec<EditorHighlight>> = None;

    unsafe {
        if let Some(ref highlight) = SAVED_HIGHLIGHT {
            if let Some(state) = STATE.as_mut() {
                state.edit_buffer.rows[SAVED_HIGHLIGHT_LINE as usize]
                    .highlight
                    .copy_from_slice(highlight);
            }
            SAVED_HIGHLIGHT = None;
        }
    }

    match key {
        Byte(b'\r') | Byte(b'\x1b') => {
            unsafe {
                LAST_MATCH = -1;
                FORWARD = true;
            }
            return;
        }
        Arrow(Arrow::Right) | Arrow(Arrow::Down) => unsafe {
            FORWARD = true;
        },
        Arrow(Arrow::Left) | Arrow(Arrow::Up) => unsafe {
            FORWARD = false;
        },
        Byte(c0) if c0 == 0 => {
            return;
        }
        _ => unsafe {
            LAST_MATCH = -1;
            FORWARD = true;
        },
    }

    if let Some(state) = unsafe { STATE.as_mut() } {
        unsafe {
            if LAST_MATCH == -1 {
                FORWARD = true;
            }
        }
        let mut current: i32 = unsafe { LAST_MATCH };
        let row_count = state.edit_buffer.rows.len() as u32;
        for _ in 0..row_count {
            current += if unsafe { FORWARD } { 1 } else { -1 };
            if current == -1 {
                current = (row_count as i32) - 1;
            } else if current == row_count as _ {
                current = 0;
            }

            let row = &mut state.edit_buffer.rows[current as usize];
            if let Some(index) = row.render.find(query) {
                unsafe {
                    LAST_MATCH = current;
                }
                state.edit_buffer.cy = current as u32;
                state.edit_buffer.cx = row_rx_to_cx(row, index as u32);
                state.edit_buffer.row_offset = row_count;

                unsafe {
                    SAVED_HIGHLIGHT_LINE = current as u32;
                    SAVED_HIGHLIGHT = Some(row.highlight.clone());
                }
                for i in index..index + query.len() {
                    row.highlight[i] = EditorHighlight::Match;
                }

                break;
            }
        }
    }
}

fn find() {
    if let Some(state) = unsafe { STATE.as_mut() } {
        let saved_cx = state.edit_buffer.cx;
        let saved_cy = state.edit_buffer.cy;
        let saved_col_offset = state.edit_buffer.col_offset;
        let saved_row_offset = state.edit_buffer.row_offset;

        if prompt!("Search: {} (Use ESC/Arrows/Enter)", Some(&find_callback)).is_none() {
            state.edit_buffer.cx = saved_cx;
            state.edit_buffer.cy = saved_cy;
            state.edit_buffer.col_offset = saved_col_offset;
            state.edit_buffer.row_offset = saved_row_offset;
        }
    }
}

/*** output ***/

fn scroll() {
    if let Some(state) = unsafe { STATE.as_mut() } {
        state.edit_buffer.rx = 0;
        if state.edit_buffer.cy < state.edit_buffer.rows.len() as u32 {
            state.edit_buffer.rx = row_cx_to_rx(
                &state.edit_buffer.rows[state.edit_buffer.cy as usize],
                state.edit_buffer.cx,
            )
        }

        if state.edit_buffer.cy < state.edit_buffer.row_offset {
            state.edit_buffer.row_offset = state.edit_buffer.cy;
        }
        if state.edit_buffer.cy >= state.edit_buffer.row_offset + state.screen_rows {
            state.edit_buffer.row_offset = state.edit_buffer.cy - state.screen_rows + 1;
        }
        if state.edit_buffer.rx < state.edit_buffer.col_offset {
            state.edit_buffer.col_offset = state.edit_buffer.rx;
        }
        if state.edit_buffer.rx >= state.edit_buffer.col_offset + state.screen_cols {
            state.edit_buffer.col_offset = state.edit_buffer.rx - state.screen_cols + 1;
        }
    }
}

fn draw_rows(buf: &mut String) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        for y in 0..state.screen_rows {
            let file_index = y + state.edit_buffer.row_offset;
            if file_index >= state.edit_buffer.rows.len() as u32 {
                if state.edit_buffer.rows.len() == 0 && y == state.screen_rows / 3 {
                    let mut welcome = format!("Kilo editor -- version {}", KILO_VERSION);
                    let mut padding = (state.screen_cols as usize - welcome.len()) / 2;

                    if padding > 0 {
                        buf.push('~');
                        padding -= 1;
                    }
                    for _ in 0..padding {
                        buf.push(' ');
                    }

                    welcome.truncate(state.screen_cols as _);
                    buf.push_str(&welcome);
                } else {
                    buf.push('~');
                }
            } else {
                let current_row = &state.edit_buffer.rows[file_index as usize];
                let mut len = std::cmp::min(
                    current_row
                        .render
                        .len()
                        .saturating_sub(state.edit_buffer.col_offset as _),
                    state.screen_cols as usize,
                );


                let mut current_colour = None;
                for (i, c) in current_row
                    .render
                    .chars()
                    .skip(state.edit_buffer.col_offset as _)
                    .enumerate()
                {
                    if i >= len {
                        break;
                    }

                    if c.is_control() {
                        let symbol = if c as u32 <= 26 {
                            (b'@' + c as u8) as char
                        } else {
                            '?'
                        };
                        buf.push_str("\x1b[7m");
                        buf.push(symbol);
                        buf.push_str("\x1b[m");
                        if let Some(colour) = current_colour {
                            buf.push_str(&format!("\x1b[{}m", colour));
                        }
                    } else {
                        match current_row.highlight[i] {
                            EditorHighlight::Normal => {
                                if current_colour.is_some() {
                                    buf.push_str("\x1b[39m");
                                    current_colour = None;
                                }
                                buf.push(c);
                            }
                            _ => {
                                let colour = syntax_to_color(current_row.highlight[i]);
                                if Some(colour) != current_colour {
                                    current_colour = Some(colour);
                                    buf.push_str(&format!("\x1b[{}m", colour));
                                }
                                buf.push(c);
                            }
                        }
                    }
                }
                buf.push_str("\x1b[39m");
            }

            buf.push_str("\x1b[K");

            buf.push_str("\r\n");
        }
    }
}

fn draw_status_bar(buf: &mut String) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        buf.push_str("\x1b[7m");

        let name = match &state.edit_buffer.filename {
            &Some(ref f_n) => f_n,
            &None => "[No Name]",
        };

        let status = format!(
            "{:.20} - {} lines {}",
            name,
            state.edit_buffer.rows.len(),
            if state.edit_buffer.dirty {
                "(modified)"
            } else {
                ""
            }
        );
        let r_status = format!(
            "{} | {}/{}",
            match state.syntax {
                Some(ref syntax) => syntax.file_type,
                None => "no ft",
            },
            state.edit_buffer.cy + 1,
            state.edit_buffer.rows.len()
        );

        buf.push_str(&status);

        let screen_cols = state.screen_cols as usize;
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

fn draw_message_bar(buf: &mut String) {
    buf.push_str("\x1b[K");

    if let Some(state) = unsafe { STATE.as_mut() } {
        let msglen = std::cmp::min(state.status_msg.len(), state.screen_cols as usize);

        if msglen > 0
            && Instant::now().duration_since(state.status_msg_time) < Duration::from_secs(5)
        {
            buf.push_str(&state.status_msg[..msglen]);
        }
    }
}

fn refresh_screen(buf: &mut String) {
    scroll();
    buf.clear();

    buf.push_str("\x1b[?25l");
    buf.push_str("\x1b[H");

    draw_rows(buf);
    draw_status_bar(buf);
    draw_message_bar(buf);

    if let Some(state) = unsafe { STATE.as_mut() } {
        buf.push_str(&format!(
            "\x1b[{};{}H",
            (state.edit_buffer.cy - state.edit_buffer.row_offset) + 1,
            (state.edit_buffer.rx - state.edit_buffer.col_offset) + 1
        ));
    }

    buf.push_str("\x1b[?25h");

    let mut stdout = io::stdout();
    stdout.write(buf.as_bytes()).unwrap_or_default();
    stdout.flush().unwrap_or_default();
}

/*** input ***/

fn move_cursor(arrow: Arrow) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        let row_len = if state.edit_buffer.cy < state.edit_buffer.rows.len() as u32 {
            Some(
                state.edit_buffer.rows[state.edit_buffer.cy as usize]
                    .row
                    .len(),
            )
        } else {
            None
        };

        match arrow {
            Arrow::Left => if state.edit_buffer.cx != 0 {
                state.edit_buffer.cx -= 1;
            } else if state.edit_buffer.cy > 0 {
                state.edit_buffer.cy -= 1;
                state.edit_buffer.cx = state.edit_buffer.rows[state.edit_buffer.cy as usize]
                    .row
                    .len() as u32;
            },
            Arrow::Right => match row_len {
                Some(len) if (state.edit_buffer.cx as usize) < len => {
                    state.edit_buffer.cx += 1;
                }
                Some(len) if (state.edit_buffer.cx as usize) == len => {
                    state.edit_buffer.cy += 1;
                    state.edit_buffer.cx = 0;
                }
                _ => {}
            },
            Arrow::Up => {
                state.edit_buffer.cy = state.edit_buffer.cy.saturating_sub(1);
            }
            Arrow::Down => if state.edit_buffer.cy < state.edit_buffer.rows.len() as u32 {
                state.edit_buffer.cy += 1;
            },
        }


        let new_row_len = if state.edit_buffer.cy < state.edit_buffer.rows.len() as u32 {
            state.edit_buffer.rows[state.edit_buffer.cy as usize]
                .row
                .len() as u32
        } else {
            0
        };
        if state.edit_buffer.cx > new_row_len {
            state.edit_buffer.cx = new_row_len;
        }
    }
}

fn process_keypress() {
    static mut QUIT_TIMES: u32 = KILO_QUIT_TIMES;
    let key = read_key();

    match key {
        Byte(b'\r') => insert_newline(),
        Byte(c0) if c0 == CTRL_KEY!(b'q') => {
            if unsafe { STATE.as_mut() }
                .map(|st| st.edit_buffer.dirty)
                .unwrap_or(true) && unsafe { QUIT_TIMES > 0 }
            {
                set_status_message!(
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
            save();
        }
        Home => if let Some(state) = unsafe { STATE.as_mut() } {
            state.edit_buffer.cx = 0;
        },
        End => if let Some(state) = unsafe { STATE.as_mut() } {
            if state.edit_buffer.cy < state.edit_buffer.rows.len() as u32 {
                state.edit_buffer.cx = state.edit_buffer.rows[state.edit_buffer.cy as usize]
                    .row
                    .len() as u32;
            }
        },
        Byte(c0) if c0 == CTRL_KEY!(b'f') => {
            find();
        }
        Byte(BACKSPACE) | Delete | Byte(CTRL_H) => {
            match key {
                Delete => {
                    move_cursor(Arrow::Right);
                }
                _ => {}
            }
            del_char();
        }
        Page(page) => if let Some(state) = unsafe { STATE.as_mut() } {
            match page {
                Page::Up => {
                    state.edit_buffer.cy = state.edit_buffer.row_offset;
                }
                Page::Down => {
                    state.edit_buffer.cy = state.edit_buffer.row_offset + state.screen_rows - 1;
                    if state.edit_buffer.cy > state.edit_buffer.rows.len() as u32 {
                        state.edit_buffer.cy = state.edit_buffer.rows.len() as u32;
                    }
                }
            };


            let arrow = match page {
                Page::Up => Arrow::Up,
                Page::Down => Arrow::Down,
            };

            for _ in 0..state.screen_rows {
                move_cursor(arrow);
            }
        },
        Arrow(arrow) => {
            move_cursor(arrow);
        }
        Byte(c0) if c0 == CTRL_KEY!(b'l') || c0 == b'\x1b' => {}
        Byte(c0) if c0 == 0 => {
            return;
        }
        Byte(c0) => {
            insert_char(c0 as char);
        }
    }
    unsafe {
        QUIT_TIMES = KILO_QUIT_TIMES;
    }
}

/*** init ***/

fn init_editor() {
    let mut state: EditorState = Default::default();
    match get_window_size() {
        None => die("get_window_size"),
        Some((rows, cols)) => {
            //leave room for the status bar
            state.screen_rows = rows - 2;
            state.screen_cols = cols;
        }
    }
    unsafe {
        STATE = Some(state);
    }
}

fn main() {
    init_editor();

    let mut args = std::env::args();
    //skip binary name
    args.next();
    if let Some(filename) = args.next() {
        open(filename);
    }
    enable_raw_mode();

    set_status_message!("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    let mut buf = String::new();

    loop {
        refresh_screen(&mut buf);
        process_keypress();
    }
}
