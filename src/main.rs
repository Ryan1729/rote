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
use std::fmt;
use std::path::Path;

/*** defines ***/

const ROTE_VERSION: &'static str = env!("CARGO_PKG_VERSION");
const ROTE_TAB_STOP: usize = 4;
const ROTE_QUIT_TIMES: u32 = 3;
const BACKSPACE: u8 = 127;

macro_rules! CTRL_KEY {
    ($k :expr) => (($k) & 0b0001_1111)
}

macro_rules! p {
    ($expr: expr) => {
        if cfg!(debug_assertions) || cfg!(test) {
            println!("{:?}\n\n", $expr);
        }
    };
    ($($element:expr),+) => {
        if cfg!(debug_assertions) || cfg!(test) {
            let tuple = ($($element,)+);

            let s = format!("{:?}", tuple);

            p!(s)
        }
    }
}

macro_rules! c {
    ($expr: expr) => {
        if cfg!(debug_assertions) || cfg!(test) {
            if let Some(state) = unsafe { STATE.as_mut() } {
                let s = format!("{:?}\n\n", $expr);

                for line in s.lines() {
                    let at = state.console_state.rows.len() as u32;
                    insert_row(&mut state.console_state, at, line.to_owned());
                }
            }
        }
    };
    ($($element:expr),+) => {
        if cfg!(debug_assertions) || cfg!(test) {
            let tuple = ($($element,)+);

            let s = format!("{:?}", tuple);

            c!(s)
        }
    }
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
    ($state:expr, $format_str: expr) => {prompt!($state, $format_str, None)};
    ($state:expr, $format_str: expr, $callback: expr) => {{
      let mut buf = String::new();
      let mut display_buf = String::new();
      let mut result = None;

      let callback : Option<&Fn(&mut EditBufferState, &str, EditorKey)> = $callback;

      loop {
            set_status_message!($format_str, buf);
            refresh_screen($state, &mut display_buf);

            let key = read_key();

            match key {
                Byte(BACKSPACE) | Delete | Byte(CTRL_H) => {
                    buf.pop();
                }

                Byte(b'\x1b') => {
                    set_status_message!("");
                    if let Some(cb) = callback {
                        cb($state, &mut buf, key);
                    }
                    break;
                }
                Byte(b'\r') => {
                    if buf.len() != 0 {
                      set_status_message!("");
                      if let Some(cb) = callback {
                          cb($state, &mut buf, key);
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
                        cb($state, &mut buf, key);
                    }
                }
            }
      }

      result
  }}
}


#[derive(Clone, Copy, Debug)]
enum EditorKey {
    Byte(u8),
    Arrow(Arrow),
    CtrlArrow(Arrow),
    ShiftArrow(Arrow),
    Page(Page),
    Delete,
    Home,
    End,
}
use EditorKey::*;

impl Default for EditorKey {
    fn default() -> EditorKey {
        Byte(0)
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Section {
    Character((u32, u32)), //TODO represent range across lines
}
use Section::*;

#[derive(Clone, Debug, PartialEq)]
enum Edit {
    Insert(Section, String),
    Remove(Section, String),
}
use Edit::*;

#[derive(Clone, Copy, Debug)]
enum Arrow {
    Left,
    Right,
    Up,
    Down,
}

#[derive(Clone, Copy, Debug)]
enum Page {
    Up,
    Down,
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug, Default, PartialEq)]
struct Row {
    index: u32,
    row: String,
    render: String,
    highlight: Vec<EditorHighlight>,
    highlight_open_comment: bool,
}

impl Row {
    fn new(at: u32, s: String) -> Self {
        let s_capacity = s.capacity();

        let mut row = Row {
            index: at,
            row: s,
            render: String::with_capacity(s_capacity),
            highlight: Vec::with_capacity(s_capacity),
            highlight_open_comment: false,
        };

        update_row(&mut row);

        row
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
struct History {
    edits: Vec<Edit>,
    current: Option<u32>,
}

impl History {
    //this is kind of a cop-out to make property-based testing work better,
    //but I don't expect this to be any kind of bottleneck. If this causes
    //I don't know, branch prediction issues or something, then we can
    //change it.
    fn correct_current(&mut self) {
        if let Some(i) = self.current {
            let len = self.edits.len() as u32;
            if i >= len {
                self.current = if len == 0 { None } else { Some(len - 1) };
            }
        }
    }

    fn inc_current(&mut self) {
        self.correct_current();
        match self.current {
            None => {
                self.current = Some(0);
            }
            Some(i) => {
                self.current = Some(i + 1);
            }
        }
    }
    fn dec_current(&mut self) {
        self.correct_current();
        match self.current {
            None => {}
            Some(i) => if i == 0 {
                self.current = None;
            } else {
                self.current = Some(i - 1);
            },
        }
    }
    fn get_next(&mut self) -> Option<Edit> {
        self.correct_current();
        let index = match self.current {
            None => 0,
            Some(i) => i + 1,
        };

        self.edits.get(index as usize).map(Clone::clone)
    }
    fn get_current(&mut self) -> Option<Edit> {
        self.correct_current();
        self.current
            .and_then(|i| self.edits.get(i as usize))
            .map(Clone::clone)
    }

    fn remove_next(&mut self) -> Option<Edit> {
        let index = match self.current {
            None => 0,
            Some(i) => i + 1,
        } as usize;

        if index < self.edits.len() {
            Some(self.edits.remove(index))
        } else {
            None
        }
    }
    fn remove_current(&mut self) -> Option<Edit> {
        self.current.and_then(|i| {
            let index = i as usize;
            if index < self.edits.len() {
                self.dec_current();
                Some(self.edits.remove(index))
            } else {
                self.correct_current();
                None
            }
        })
    }
}

#[derive(Clone, Debug)]
pub struct EditBufferState {
    cx: u32,
    cy: u32,
    rx: u32,
    row_offset: u32,
    col_offset: u32,
    rows: Vec<Row>,
    dirty: bool,
    filename: Option<String>,
}

impl Default for EditBufferState {
    fn default() -> EditBufferState {
        EditBufferState {
            cx: Default::default(),
            cy: Default::default(),
            rx: Default::default(),
            row_offset: Default::default(),
            col_offset: Default::default(),
            //When the user opens a new file, we're pretty sure thay'll want at least one line.
            rows: vec![Default::default()],
            dirty: Default::default(),
            filename: Default::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum SectionType {
    NormalRow,
    Invalid,
}
use SectionType::*;

impl EditBufferState {
    fn section_type(&self, section: &Section) -> SectionType {
        match *section {
            Character((cx, cy)) => {
                let cy_usize = cy as usize;
                //one past the last character if each row is a valid position
                if cy_usize < self.rows.len() as _
                    && (cx as usize) <= char_len(&self.rows[cy_usize].row)
                {
                    NormalRow
                } else {
                    Invalid
                }
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
struct EditBuffer {
    state: EditBufferState,
    history: History,
}

struct EditorState {
    edit_buffer: EditBuffer,
    screen_cols: u32,
    screen_rows: u32,
    status_msg: String,
    status_msg_time: Instant,
    console_state: EditBufferState,
    show_console: bool,
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
            console_state: Default::default(),
            show_console: false,
            syntax: Default::default(),
            orig_termios: unsafe { std::mem::zeroed() },
        }
    }
}

impl fmt::Debug for EditorState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "EditorState {{
            edit_buffer: {:?},
            screen_rows: {:?},
            screen_cols: {:?},
            status_msg: {:?},
            status_msg_time: {:?},
            console_state: {:?},
            show_console: {:?},
            syntax: {:?},
            orig_termios: <termios>,
        }}",
            self.edit_buffer,
            self.screen_rows,
            self.screen_cols,
            self.status_msg,
            self.status_msg_time,
            self.console_state,
            self.show_console,
            self.syntax,
        )
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
        let mut seq = [0; 5];

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
                    } else if seq[2] == b';' {
                        match c {
                            b'1' => {
                                if stdin.read_exact(&mut seq[3..5]).is_err() {
                                    return Byte(b'\x1b');
                                }
                                match (seq[3], seq[4]) {
                                    (b'5', b'A') => {
                                        return CtrlArrow(Arrow::Up);
                                    }
                                    (b'5', b'B') => {
                                        return CtrlArrow(Arrow::Down);
                                    }
                                    (b'5', b'C') => {
                                        return CtrlArrow(Arrow::Right);
                                    }
                                    (b'5', b'D') => {
                                        return CtrlArrow(Arrow::Left);
                                    }
                                    (b'2', b'A') => {
                                        return ShiftArrow(Arrow::Up);
                                    }
                                    (b'2', b'B') => {
                                        return ShiftArrow(Arrow::Down);
                                    }
                                    (b'2', b'C') => {
                                        return ShiftArrow(Arrow::Right);
                                    }
                                    (b'2', b'D') => {
                                        return ShiftArrow(Arrow::Left);
                                    }
                                    _ => {}
                                }
                            }
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
    let render_char_len = char_len(&row.render);
    let extra_needed = render_char_len.saturating_sub(row.highlight.capacity());
    if extra_needed != 0 {
        row.highlight.reserve(extra_needed);
    }

    if let Some(state) = unsafe { STATE.as_mut() } {
        if let Some(ref syntax) = state.syntax {
            let mut prev_sep = true;
            let mut in_string = None;
            let mut in_comment = row.index > 0
                && state.edit_buffer.state.rows[(row.index - 1) as usize].highlight_open_comment;

            let mut char_indices = row.render.char_indices();

            'char_indices: while let Some((i, c)) = char_indices.next() {
                let prev_highlight = if i > 0 {
                    row.highlight[i - 1]
                } else {
                    EditorHighlight::Normal
                };

                if syntax.singleline_comment_start.len() > 0 && in_string.is_none() && !in_comment {
                    if row.render[i..].starts_with(syntax.singleline_comment_start) {
                        for _ in 0..row.render[i..].len() {
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
                            let one_past_comment_end = syntax.multiline_comment_end.len();
                            for j in 0..one_past_comment_end {
                                row.highlight.push(EditorHighlight::MultilineComment);
                                if j < one_past_comment_end - 1 {
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
                        let one_past_comment_start = syntax.multiline_comment_start.len();
                        for j in 0..one_past_comment_start {
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
                        let mut k_len = keyword.as_bytes().len();
                        let is_kw2 = keyword.ends_with('|');
                        if is_kw2 {
                            k_len -= 1;
                        }
                        let one_past_keyword = i + k_len;
                        if (&row.render[i..]).starts_with(&keyword[..k_len])
                            && row.render[one_past_keyword..]
                                .chars()
                                .next()
                                .map(is_separator)
                                .unwrap_or(false)
                        {
                            let mut k_char_len = char_len(keyword);
                            if is_kw2 {
                                k_char_len -= 1;
                            }
                            for j in 0..k_char_len {
                                row.highlight.push(if is_kw2 {
                                    EditorHighlight::Keyword2
                                } else {
                                    EditorHighlight::Keyword1
                                });
                                if j < k_char_len - 1 {
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
            if changed && row.index + 1 < state.edit_buffer.state.rows.len() as u32 {
                update_syntax(&mut state.edit_buffer.state.rows[(row.index + 1) as usize]);
            }
        } else {
            for _ in 0..render_char_len {
                row.highlight.push(EditorHighlight::Normal);
            }
        }
    } else {
        for _ in 0..render_char_len {
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
        if let Some(ref filename) = state.edit_buffer.state.filename {
            for s in HLDB.iter() {
                let mut i = 0;
                while let Some(ref file_match) = s.file_match[i] {
                    let is_ext = file_match.starts_with('.');
                    if (is_ext && filename.ends_with(file_match))
                        || (!is_ext && filename.contains(file_match))
                    {
                        state.syntax = Some(s.clone());

                        for row in state.edit_buffer.state.rows.iter_mut() {
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
            rx += (ROTE_TAB_STOP - 1) - (rx % ROTE_TAB_STOP);
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
            cur_rx += (ROTE_TAB_STOP - 1) - (cur_rx % ROTE_TAB_STOP);
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

    row.render = String::with_capacity(row.row.len() + tabs * (ROTE_TAB_STOP - 1));

    for c in row.row.chars() {
        if c == '\t' {
            tabs += 1;
            row.render.push(' ');
            while row.render.len() % ROTE_TAB_STOP != 0 {
                row.render.push(' ');
            }
        } else {
            row.render.push(c);
        }
    }

    update_syntax(row);
}

fn insert_row(state: &mut EditBufferState, at: u32, s: String) {
    if at > state.rows.len() as u32 {
        return;
    }

    state.rows.insert(at as usize, Row::new(at, s));

    for i in (at + 1) as usize..state.rows.len() {
        state.rows[i as usize].index += 1;
    }

    state.dirty = true;
}

fn del_row(state: &mut EditBufferState, cy: u32) {
    let cy_usize = cy as usize;
    if cy_usize >= state.rows.len() {
        return;
    }

    state.rows.remove(cy_usize);

    for i in cy_usize..state.rows.len() {
        state.rows[i].index -= 1;
    }

    state.dirty = true;
}

fn row_insert_char(row: &mut Row, cx: u32, c: char) {
    if let Some(i) = cx_to_byte_x(&row.row, cx) {
        row.row.insert(i, c);

        update_row(row);

        if let Some(state) = unsafe { STATE.as_mut() } {
            state.edit_buffer.state.dirty = true;
        }
    }
}

fn row_append_string(row: &mut Row, s: &str) {
    row.row.push_str(s);
    update_row(row);
    if let Some(state) = unsafe { STATE.as_mut() } {
        state.edit_buffer.state.dirty = true;
    }
}

fn row_del_char(row: &mut Row, cx: u32) {
    if let Some(i) = cx_to_byte_x(&row.row, cx) {
        row.row.remove(i);
        update_row(row);
        if let Some(state) = unsafe { STATE.as_mut() } {
            state.edit_buffer.state.dirty = true;
        }
    }
}

/*** editor operations ***/

fn insert_char(state: &mut EditBufferState, (cx, cy): (u32, u32), c: char) {
    state.cx = cx;
    state.cy = cy;
    insert_char_at_cursor(state, c)
}

fn insert_char_at_cursor(state: &mut EditBufferState, c: char) {
    if state.cy == state.rows.len() as u32 {
        let at = state.rows.len() as u32;
        insert_row(state, at, String::new());
    }

    row_insert_char(&mut state.rows[state.cy as usize], state.cx, c);
    state.cx += 1;
}

fn insert_newline(state: &mut EditBufferState, (cx, cy): (u32, u32)) {
    state.cx = cx;
    state.cy = cy;
    if state.cx == 0 {
        insert_row(state, cy, String::new());
    } else {
        let new_row = {
            let row = &mut state.rows[cy as usize];

            if let Some(byte_x) = cx_to_byte_x(&row.row, cx) {
                row.row.split_off(byte_x)
            } else {
                String::new()
            }
        };

        insert_row(state, cy + 1, new_row);
        update_row(&mut state.rows[cy as usize]);
    }
    state.cy += 1;
    state.cx = 0;
}

fn del_char(state: &mut EditBufferState, (cx, cy): (u32, u32)) {
    state.cx = cx;
    state.cy = cy;
    if state.cy == state.rows.len() as u32 {
        return;
    };

    let char_len = char_len(&state.rows[state.cy as usize].row) as u32;
    if state.cx < char_len {
        row_del_char(&mut state.rows[state.cy as usize], state.cx);
    } else {
        {
            let (before, after) = state.rows.split_at_mut(state.cy as usize + 1);

            match (before.last_mut(), after.first_mut()) {
                (Some(previous_row), Some(row)) => {
                    state.cx = char_len;
                    row_append_string(previous_row, &row.row);
                }
                // _ => die("del_char"),
                (a, b) => panic!("del_char {:?}", (a, b)),
            }
        }

        del_row(state, cy + 1);
    }
}

/*** file i/o ***/

fn rows_to_string() -> String {
    let mut buf = String::new();
    if let Some(state) = unsafe { STATE.as_mut() } {
        for row in state.edit_buffer.state.rows.iter() {
            buf.push_str(&row.row);
            buf.push('\n');
        }
    }
    buf
}

fn open<P: AsRef<Path>>(filename: P) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        state.edit_buffer.state.filename = Some(format!("{}", filename.as_ref().display()));

        select_syntax_highlight();

        if let Ok(file) = File::open(filename) {
            for res in BufReader::new(file).lines() {
                match res {
                    Ok(mut line) => {
                        while line.ends_with(is_newline) {
                            line.pop();
                        }
                        let at = state.edit_buffer.state.rows.len() as u32;
                        insert_row(&mut state.edit_buffer.state, at, line);
                    }
                    Err(e) => {
                        die(&e.to_string());
                    }
                }
            }
        } else {
            die("open");
        }
        state.edit_buffer.state.dirty = false;
    }
}

fn save() {
    if let Some(state) = unsafe { STATE.as_mut() } {
        if state.edit_buffer.state.filename.is_none() {
            state.edit_buffer.state.filename = prompt!(&mut state.edit_buffer.state, "Save as: {}");
            select_syntax_highlight();
        }

        if let Some(filename) = state.edit_buffer.state.filename.as_ref() {
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
                    state.edit_buffer.state.dirty = false;
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

fn find_callback(buffer_state: &mut EditBufferState, query: &str, key: EditorKey) {
    static mut LAST_MATCH: i32 = -1;
    static mut FORWARD: bool = true;

    static mut SAVED_HIGHLIGHT_LINE: u32 = 0;
    static mut SAVED_HIGHLIGHT: Option<Vec<EditorHighlight>> = None;

    unsafe {
        if let Some(ref highlight) = SAVED_HIGHLIGHT {
            buffer_state.rows[SAVED_HIGHLIGHT_LINE as usize]
                .highlight
                .copy_from_slice(highlight);

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

    unsafe {
        if LAST_MATCH == -1 {
            FORWARD = true;
        }
    }

    let mut current: i32 = unsafe { LAST_MATCH };
    let row_count = buffer_state.rows.len() as u32;
    for _ in 0..row_count {
        current += if unsafe { FORWARD } { 1 } else { -1 };
        if current == -1 {
            current = (row_count as i32) - 1;
        } else if current == row_count as _ {
            current = 0;
        }

        let row = &mut buffer_state.rows[current as usize];
        if let Some(index) = row.render.find(query) {
            unsafe {
                LAST_MATCH = current;
            }
            buffer_state.cy = current as u32;
            buffer_state.cx = row_rx_to_cx(row, index as u32);
            buffer_state.row_offset = row_count;

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

fn find(state: &mut EditBufferState) {
    let saved_cx = state.cx;
    let saved_cy = state.cy;
    let saved_col_offset = state.col_offset;
    let saved_row_offset = state.row_offset;

    if prompt!(
        state,
        "Search: {} (Use ESC/Arrows/Enter)",
        Some(&find_callback)
    ).is_none()
    {
        state.cx = saved_cx;
        state.cy = saved_cy;
        state.col_offset = saved_col_offset;
        state.row_offset = saved_row_offset;
    }
}

/*** output ***/

fn scroll(buffer_state: &mut EditBufferState) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        buffer_state.rx = 0;
        if buffer_state.cy < buffer_state.rows.len() as u32 {
            buffer_state.rx = row_cx_to_rx(
                &buffer_state.rows[buffer_state.cy as usize],
                buffer_state.cx,
            )
        }

        if buffer_state.cy < buffer_state.row_offset {
            buffer_state.row_offset = buffer_state.cy;
        }
        if buffer_state.cy >= buffer_state.row_offset + state.screen_rows {
            buffer_state.row_offset = buffer_state.cy - state.screen_rows + 1;
        }
        if buffer_state.rx < buffer_state.col_offset {
            buffer_state.col_offset = buffer_state.rx;
        }
        if buffer_state.rx >= buffer_state.col_offset + state.screen_cols {
            buffer_state.col_offset = buffer_state.rx - state.screen_cols + 1;
        }
    }
}

fn draw_rows(
    (screen_cols, screen_rows): (u32, u32),
    buffer_state: &EditBufferState,
    buf: &mut String,
) {
    for y in 0..screen_rows {
        let file_index = y + buffer_state.row_offset;
        if file_index >= buffer_state.rows.len() as u32 {
            if y == screen_rows / 3 && buffer_state.rows.len() <= 1
                && buffer_state
                    .rows
                    .first()
                    .map(|r| r.row.len() == 0)
                    .unwrap_or(false)
            {
                let mut welcome =
                    format!("Rote : Ryan's Own Text Editor -- version {}", ROTE_VERSION);
                let mut padding = (screen_cols as usize - welcome.len()) / 2;

                if padding > 0 {
                    buf.push('~');
                    padding -= 1;
                }
                for _ in 0..padding {
                    buf.push(' ');
                }

                welcome.truncate(screen_cols as _);
                buf.push_str(&welcome);
            } else {
                buf.push('~');
            }
        } else {
            let current_row = &buffer_state.rows[file_index as usize];
            let mut len = std::cmp::min(
                current_row
                    .render
                    .len()
                    .saturating_sub(buffer_state.col_offset as _),
                screen_cols as usize,
            );


            let mut current_colour = None;
            for (i, c) in current_row
                .render
                .chars()
                .skip(buffer_state.col_offset as _)
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

fn draw_status_bar(buf: &mut String) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        buf.push_str("\x1b[7m");

        let name = match &state.edit_buffer.state.filename {
            &Some(ref f_n) => f_n,
            &None => "[No Name]",
        };

        let status = format!(
            "{:.20} - {} lines {}",
            name,
            state.edit_buffer.state.rows.len(),
            if state.edit_buffer.state.dirty {
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
            state.edit_buffer.state.cy + 1,
            state.edit_buffer.state.rows.len()
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

fn refresh_screen(buffer_state: &mut EditBufferState, buf: &mut String) {
    scroll(buffer_state);
    buf.clear();

    buf.push_str("\x1b[?25l");
    buf.push_str("\x1b[H");

    if let Some(state) = unsafe { STATE.as_mut() } {
        draw_rows((state.screen_cols, state.screen_rows), &buffer_state, buf);
    }
    draw_status_bar(buf);
    draw_message_bar(buf);

    draw_cursor(&buffer_state, buf);

    buf.push_str("\x1b[?25h");

    let mut stdout = io::stdout();
    stdout.write(buf.as_bytes()).unwrap_or_default();
    stdout.flush().unwrap_or_default();
}

fn draw_cursor(buffer_state: &EditBufferState, buf: &mut String) {
    buf.push_str(&format!(
        "\x1b[{};{}H",
        (buffer_state.cy - buffer_state.row_offset) + 1,
        (buffer_state.rx - buffer_state.col_offset) + 1
    ));
}

/*** input ***/

fn move_cursor(state: &mut EditBufferState, arrow: Arrow) {
    match arrow {
        Arrow::Left => if state.cx != 0 {
            state.cx -= 1;
        } else if state.cy > 0 {
            state.cy -= 1;
            state.cx = state.rows[state.cy as usize].row.len() as u32;
        },
        Arrow::Right => {
            let row_len = if state.cy < state.rows.len() as u32 {
                Some(state.rows[state.cy as usize].row.len())
            } else {
                None
            };

            match row_len {
                Some(len) if (state.cx as usize) < len => {
                    state.cx += 1;
                }
                Some(len) if (state.cx as usize) == len => {
                    state.cy += 1;
                    state.cx = 0;
                }
                _ => {}
            }
        }
        Arrow::Up => {
            state.cy = state.cy.saturating_sub(1);
        }
        Arrow::Down => if state.cy < state.rows.len() as u32 {
            state.cy += 1;
        },
    }


    let new_row_len = if state.cy < state.rows.len() as u32 {
        state.rows[state.cy as usize].row.len() as u32
    } else {
        0
    };
    if state.cx > new_row_len {
        state.cx = new_row_len;
    }
}

fn jump_cursor(state: &mut EditBufferState, arrow: Arrow) {
    match arrow {
        Arrow::Left => {
            let mut seen_non_separator = false;

            loop {
                match get_previous_char(state) {
                    Some(c) if is_separator(c) => if seen_non_separator {
                        break;
                    } else {
                        move_cursor(state, arrow);
                    },
                    Some(_) => {
                        seen_non_separator = true;
                        move_cursor(state, arrow);
                    }
                    None => {
                        break;
                    }
                }
            }
        }
        Arrow::Right => {
            let mut seen_non_separator = false;

            loop {
                match get_current_char(state) {
                    Some(c) if is_separator(c) => if seen_non_separator {
                        break;
                    } else {
                        move_cursor(state, arrow);
                    },
                    Some(_) => {
                        seen_non_separator = true;
                        move_cursor(state, arrow);
                    }
                    None => {
                        break;
                    }
                }
            }
        }
        Arrow::Up => {
            let mut seen_empty_line = false;

            loop {
                match get_previous_line_len(state) {
                    Some(0) => {
                        seen_empty_line = true;
                        move_cursor(state, arrow);
                    }
                    Some(_) => if seen_empty_line {
                        break;
                    } else {
                        move_cursor(state, arrow);
                    },
                    None => {
                        break;
                    }
                }
            }
        }
        Arrow::Down => {
            let mut seen_empty_line = false;

            loop {
                match get_next_line_len(state) {
                    Some(0) => {
                        seen_empty_line = true;
                        move_cursor(state, arrow);
                    }
                    Some(_) => if seen_empty_line {
                        break;
                    } else {
                        move_cursor(state, arrow);
                    },
                    None => {
                        break;
                    }
                }
            }
        }
    }
}

fn get_previous_char(state: &EditBufferState) -> Option<char> {
    get_previous_character_containing_cx_cy(state).and_then(|(cx, cy)| {
        state.rows[cy as usize].row.chars().nth(cx as usize)
    })
}
fn get_previous_character_containing_cx_cy(state: &EditBufferState) -> Option<(u32, u32)> {
    if state.cx != 0 {
        Some((state.cx - 1, state.cy))
    } else if state.cy > 0 {
        let mut cy = state.cy - 1;
        loop {
            let len = state.rows[cy as usize].row.len() as u32;
            if len > 0 {
                return Some((len - 1, cy));
            } else if cy == 0 {
                return None;
            }
            cy -= 1;
        }
    } else {
        None
    }
}

fn get_current_char(state: &EditBufferState) -> Option<char> {
    let char_on_this_line = state.rows[state.cy as usize]
        .row
        .chars()
        .nth(state.cx as usize);
    if char_on_this_line.is_some() {
        char_on_this_line
    } else {
        get_next_character_containing_cx_cy(state).and_then(|(cx, cy)| {
            state.rows[cy as usize].row.chars().nth(cx as usize)
        })
    }
}

fn get_previous_line_len(state: &EditBufferState) -> Option<u32> {
    if state.cy != 0 {
        state
            .rows
            .get(state.cy as usize - 1)
            .map(|row| row.row.len() as u32)
    } else {
        None
    }
}
fn get_next_line_len(state: &EditBufferState) -> Option<u32> {
    state
        .rows
        .get(state.cy as usize + 1)
        .map(|row| row.row.len() as u32)
}

fn get_next_character_containing_cx_cy(state: &EditBufferState) -> Option<(u32, u32)> {
    let mut cy = state.cy + 1;
    loop {
        if let Some(row) = state.rows.get(cy as usize) {
            if row.row.len() > 0 {
                return Some((0, cy));
            }
        } else {
            return None;
        }
        cy += 1;
    }
}

//handle normal editor operations
fn process_editor_keypress() {
    static mut QUIT_TIMES: u32 = ROTE_QUIT_TIMES;
    let key = read_key();

    let mut possible_edit = None;

    match key {
        Byte(b'\r') => if let Some(state) = unsafe { STATE.as_mut() } {
            possible_edit = Some(Insert(
                Character((state.edit_buffer.state.cx, state.edit_buffer.state.cy)),
                "\r".to_owned(),
            ));
        },
        //on my keyboard/terminal emulator this results from ctrl-5
        Byte(29) => if let Some(state) = unsafe { STATE.as_mut() } {
            state.show_console = !state.show_console;
        },
        Byte(c0) if c0 == CTRL_KEY!(b'q') => {
            if unsafe { STATE.as_mut() }
                .map(|st| st.edit_buffer.state.dirty)
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
            state.edit_buffer.state.cx = 0;
        },
        End => if let Some(state) = unsafe { STATE.as_mut() } {
            if state.edit_buffer.state.cy < state.edit_buffer.state.rows.len() as u32 {
                state.edit_buffer.state.cx = state.edit_buffer.state.rows
                    [state.edit_buffer.state.cy as usize]
                    .row
                    .len() as u32;
            }
        },
        Byte(c0) if c0 == CTRL_KEY!(b'f') => if let Some(state) = unsafe { STATE.as_mut() } {
            find(&mut state.edit_buffer.state);
        },
        Byte(c0) if c0 == CTRL_KEY!(b'z') => if let Some(state) = unsafe { STATE.as_mut() } {
            undo(&mut state.edit_buffer);
        },
        Byte(c0) if c0 == CTRL_KEY!(b'y') || c0 == CTRL_KEY!(b'Z') => {
            if let Some(state) = unsafe { STATE.as_mut() } {
                redo(&mut state.edit_buffer);
            }
        }
        Byte(BACKSPACE) | Delete | Byte(CTRL_H) => if let Some(state) = unsafe { STATE.as_mut() } {
            match key {
                Byte(BACKSPACE) | Byte(CTRL_H) => {
                    move_cursor(&mut state.edit_buffer.state, Arrow::Left);
                }
                _ => {}
            }


            if let Some(row) = state
                .edit_buffer
                .state
                .rows
                .get(state.edit_buffer.state.cy as usize)
            {
                let cx = state.edit_buffer.state.cx as usize;
                let current_char = row.row.chars().nth(cx);

                let current_char_str = if let Some(c) = current_char {
                    c.to_string()
                } else {
                    '\n'.to_string()
                };

                possible_edit = Some(Remove(
                    Character((state.edit_buffer.state.cx, state.edit_buffer.state.cy)),
                    current_char_str.to_owned(),
                ));
            }
        },
        Page(page) => if let Some(state) = unsafe { STATE.as_mut() } {
            match page {
                Page::Up => {
                    state.edit_buffer.state.cy = state.edit_buffer.state.row_offset;
                }
                Page::Down => {
                    state.edit_buffer.state.cy =
                        state.edit_buffer.state.row_offset + state.screen_rows - 1;
                    if state.edit_buffer.state.cy > state.edit_buffer.state.rows.len() as u32 {
                        state.edit_buffer.state.cy = state.edit_buffer.state.rows.len() as u32;
                    }
                }
            };


            let arrow = match page {
                Page::Up => Arrow::Up,
                Page::Down => Arrow::Down,
            };

            for _ in 0..state.screen_rows {
                move_cursor(&mut state.edit_buffer.state, arrow);
            }
        },
        Arrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            move_cursor(&mut state.edit_buffer.state, arrow);
        },
        CtrlArrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            jump_cursor(&mut state.edit_buffer.state, arrow);
        },
        ShiftArrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            c!(arrow);
            move_cursor(&mut state.edit_buffer.state, arrow);
        },
        Byte(c0) if c0 == CTRL_KEY!(b'l') || c0 == b'\x1b' => {}
        Byte(c0) if c0 == 0 => {
            return;
        }
        Byte(c0) => if let Some(state) = unsafe { STATE.as_mut() } {
            possible_edit = Some(Insert(
                Character((state.edit_buffer.state.cx, state.edit_buffer.state.cy)),
                (c0 as char).to_string(),
            ))
        },
    }

    if let Some(edit) = possible_edit {
        if let Some(state) = unsafe { STATE.as_mut() } {
            perform_edit(&mut state.edit_buffer, &edit);
        }
    }

    unsafe {
        QUIT_TIMES = ROTE_QUIT_TIMES;
    }
}

//handle  operations for the editor's console
fn process_console_keypress() {
    let key = read_key();

    match key {
        //on my keyboard/terminal emulator this results from ctrl-5
        Byte(29) => if let Some(state) = unsafe { STATE.as_mut() } {
            state.show_console = !state.show_console;
        },
        Home => if let Some(state) = unsafe { STATE.as_mut() } {
            state.console_state.cx = 0;
        },
        End => if let Some(state) = unsafe { STATE.as_mut() } {
            if state.console_state.cy < state.console_state.rows.len() as u32 {
                state.console_state.cx = state.console_state.rows[state.console_state.cy as usize]
                    .row
                    .len() as u32;
            }
        },
        Byte(c0) if c0 == CTRL_KEY!(b'f') => if let Some(state) = unsafe { STATE.as_mut() } {
            find(&mut state.console_state);
        },
        Page(page) => if let Some(state) = unsafe { STATE.as_mut() } {
            match page {
                Page::Up => {
                    state.console_state.cy = state.console_state.row_offset;
                }
                Page::Down => {
                    state.console_state.cy = state.console_state.row_offset + state.screen_rows - 1;
                    if state.console_state.cy > state.console_state.rows.len() as u32 {
                        state.console_state.cy = state.console_state.rows.len() as u32;
                    }
                }
            };


            let arrow = match page {
                Page::Up => Arrow::Up,
                Page::Down => Arrow::Down,
            };

            for _ in 0..state.screen_rows {
                move_cursor(&mut state.console_state, arrow);
            }
        },
        Arrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            move_cursor(&mut state.console_state, arrow);
        },
        CtrlArrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            jump_cursor(&mut state.console_state, arrow);
        },
        ShiftArrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            move_cursor(&mut state.console_state, arrow);
        },
        Byte(c0) if c0 == CTRL_KEY!(b'l') => {}
        Byte(c0) if c0 == 0 => {
            return;
        }
        _ => if let Some(state) = unsafe { STATE.as_mut() } {
            state.show_console = false;
        },
    }
}

fn cx_to_byte_x(s: &String, cx: u32) -> Option<usize> {
    let cx_usize = cx as usize;
    let char_len = char_len(s);
    if cx_usize == char_len {
        Some(s.len())
    } else if cx_usize < char_len {
        s.char_indices().nth(cx as usize).map(|(i, _)| i)
    } else {
        None
    }
}

fn char_len(s: &str) -> usize {
    s.chars().count()
}

fn perform_edit(edit_buffer: &mut EditBuffer, edit: &Edit) -> EditOutcome {
    let outcome = no_history_perform_edit(&mut edit_buffer.state, edit);

    if let Changed = outcome {
        if edit_buffer
            .history
            .get_next()
            .map(|e| edit != &e)
            .unwrap_or(true)
        {
            //Here's the bit where we throw away history...
            //It would be cool to be able to keep it...
            if let Some(i) = edit_buffer.history.current {
                edit_buffer.history.edits.truncate(1 + i as usize);
            }
            edit_buffer.history.edits.push(edit.clone());

            edit_buffer.history.current = Some(edit_buffer.history.edits.len() as u32 - 1);
        } else {
            edit_buffer.history.inc_current();
        }
    }

    outcome
}

fn unperform_edit(edit_buffer: &mut EditBuffer, edit: &Edit) -> EditOutcome {
    let outcome = no_history_unperform_edit(&mut edit_buffer.state, edit);
    if let Changed = outcome {
        edit_buffer.history.dec_current();
    }
    outcome
}

fn perform_insert(state: &mut EditBufferState, section: &Section, s: &str) -> EditOutcome {
    match *section {
        Character(coord) => {
            if s.len() == 0 {
                return Unchanged;
            }

            let mut chars = s.chars().rev();
            while let Some(c) = chars.next() {
                if is_newline(c) {
                    insert_newline(state, coord);
                } else {
                    insert_char(state, coord, c);
                }
            }
            Changed
        }
    }
}

fn is_newline(c: char) -> bool {
    c == '\n' || c == '\r'
}

fn rows_match(rows: &Vec<Row>, cy: u32, s: &str) -> bool {
    let cy_usize = cy as usize;
    if cy_usize < rows.len() {
        let mut iter = rows[cy_usize..].iter().zip(s.split(is_newline)).peekable();
        while let Some((row, sub_s)) = iter.next() {
            if iter.peek().is_some() {
                if !(row.row == sub_s) {
                    return false;
                }
            } else {
                if !row.row.starts_with(sub_s) {
                    return false;
                }
            }
        }

        true
    } else {
        false
    }
}

fn perform_remove(state: &mut EditBufferState, section: &Section, s: &str) -> EditOutcome {
    match *section {
        Character((cx, cy)) => {
            let mut should_delete = false;

            {
                if s.starts_with('\n') || s.starts_with('\r') {
                    let at_end = if let Some(row) = state.rows.get(cy as usize).map(|row| &row.row)
                    {
                        cx == char_len(row) as u32
                    } else {
                        false
                    };

                    if at_end && rows_match(&state.rows, cy + 1, &s[1..]) {
                        should_delete = true;
                    }
                } else if let Some(row) = state.rows.get(cy as usize).map(|row| &row.row) {
                    if let Some(byte_x) = cx_to_byte_x(row, cx) {
                        let row_remains = &row[byte_x..];

                        if let Some(i) = s.find(is_newline) {
                            if row_remains.starts_with(&s[..i]) {
                                if s.len() <= row_remains.len()
                                    //this indexing into s assumes that a newline is a single byte.
                                    || rows_match(&state.rows, cy + 1, &s[i + 1 ..])
                                {
                                    should_delete = true;
                                }
                            }
                        } else {
                            if row_remains.starts_with(s) {
                                should_delete = true;
                            }
                        };
                    } else {
                        state.cx = row.len() as u32;
                    }
                }
            }

            if should_delete {
                for _ in 0..char_len(s) {
                    del_char(state, (cx, cy))
                }
                Changed
            } else {
                Unchanged
            }
        }
    }
}

fn no_history_perform_edit(state: &mut EditBufferState, edit: &Edit) -> EditOutcome {
    match *edit {
        Insert(ref section, ref s) if NormalRow == state.section_type(section) => {
            perform_insert(state, section, s)
        }
        Remove(ref section, ref s) if NormalRow == state.section_type(section) => {
            perform_remove(state, section, s)
        }
        Remove(_, _) | Insert(_, _) => Unchanged,
    }
}

fn no_history_unperform_edit(state: &mut EditBufferState, edit: &Edit) -> EditOutcome {
    match *edit {
        Insert(ref section, ref s) if NormalRow == state.section_type(section) => {
            perform_remove(state, section, s)
        }
        Remove(ref section, ref s) if NormalRow == state.section_type(section) => {
            perform_insert(state, section, s)
        }
        Remove(_, _) | Insert(_, _) => Unchanged,
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum EditOutcome {
    Changed,
    Unchanged,
}
use EditOutcome::*;

fn oldest(edit_buffer: &mut EditBuffer) -> EditOutcome {
    let mut outcome = Unchanged;

    while let Changed = undo(edit_buffer) {
        outcome = Changed;
    }

    outcome
}

fn latest(edit_buffer: &mut EditBuffer) -> EditOutcome {
    let mut outcome = Unchanged;

    while let Changed = redo(edit_buffer) {
        outcome = Changed;
    }

    outcome
}

fn redo(edit_buffer: &mut EditBuffer) -> EditOutcome {
    if let Some(edit) = edit_buffer.history.get_next() {
        let outcome = no_history_perform_edit(&mut edit_buffer.state, &edit);

        if let Changed = outcome {
            edit_buffer.history.inc_current();
        } else {
            edit_buffer.history.remove_next();
        }

        Changed
    } else {
        Unchanged
    }
}

fn undo(edit_buffer: &mut EditBuffer) -> EditOutcome {
    if let Some(edit) = edit_buffer.history.get_current() {
        let outcome = no_history_unperform_edit(&mut edit_buffer.state, &edit);

        if let Changed = outcome {
            edit_buffer.history.dec_current();
        } else {
            edit_buffer.history.remove_current();
        }

        Changed
    } else {
        Unchanged
    }
}


#[cfg(test)]
#[macro_use]
extern crate quickcheck;

#[cfg(test)]
extern crate rand;

#[cfg(test)]
mod test_helpers {
    use super::*;
    pub fn edit_buffer_isomorphism(e_b1: &EditBufferState, e_b2: &EditBufferState) -> bool {
        edit_buffer_weak_isomorphism(e_b1, e_b2) && e_b1.cx == e_b2.cx && e_b1.cy == e_b2.cy
            && e_b1.rx == e_b2.rx && e_b1.row_offset == e_b2.row_offset
            && e_b1.col_offset == e_b2.col_offset && e_b1.dirty == e_b2.dirty
    }

    pub fn edit_buffer_weak_isomorphism(e_b1: &EditBufferState, e_b2: &EditBufferState) -> bool {
        e_b1.filename == e_b2.filename
            && e_b1.rows.iter().map(|r| &r.row).collect::<Vec<_>>()
                == e_b2.rows.iter().map(|r| &r.row).collect::<Vec<_>>()
    }

    pub fn must_edit_buffer_isomorphism(e_b1: &EditBufferState, e_b2: &EditBufferState) -> bool {
        assert_eq!(e_b1.filename, e_b2.filename);
        assert_eq!(e_b1.cx, e_b2.cx);
        assert_eq!(e_b1.cy, e_b2.cy);
        assert_eq!(e_b1.rx, e_b2.rx);
        assert_eq!(e_b1.row_offset, e_b2.row_offset);
        assert_eq!(e_b1.col_offset, e_b2.col_offset);
        assert_eq!(e_b1.rows, e_b2.rows);
        assert_eq!(e_b1.dirty, e_b2.dirty);

        true
    }

    pub fn must_edit_buffer_weak_isomorphism(
        e_b1: &EditBufferState,
        e_b2: &EditBufferState,
    ) -> bool {
        assert_eq!(e_b1.filename, e_b2.filename);
        assert_eq!(
            e_b1.rows.iter().map(|r| &r.row).collect::<Vec<_>>(),
            e_b2.rows.iter().map(|r| &r.row).collect::<Vec<_>>()
        );

        true
    }
}

#[cfg(test)]
mod edit_actions {
    use super::*;
    use super::test_helpers::{edit_buffer_isomorphism, edit_buffer_weak_isomorphism,
                              must_edit_buffer_isomorphism, must_edit_buffer_weak_isomorphism};
    use quickcheck::{Arbitrary, Gen, StdGen};
    use std::ops::Deref;

    #[derive(Clone, Debug)]
    //quickcheck Arbitrary adaptor that forces the size to be 1
    pub struct One<T>(pub T);

    impl<T> Deref for One<T> {
        type Target = T;
        fn deref(&self) -> &T {
            &self.0
        }
    }

    impl<T> Arbitrary for One<T>
    where
        T: Arbitrary,
    {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            One(T::arbitrary(&mut StdGen::new(g, 1)))
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            p!("shrink: Box::new((**self).shrink().map(One))");
            Box::new((**self).shrink().map(One))
        }
    }

    macro_rules! a {
        ($gen:expr) => {
            Arbitrary::arbitrary($gen)
        }
    }

    impl Arbitrary for EditBuffer {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Self {
            p!("Arbitrary for EditBuffer");
            EditBuffer {
                state: a!(g),
                history: a!(g),
            }
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            p!("shrink: EditBuffer");

            Box::new(
                (self.state.to_owned(), self.history.to_owned())
                    .shrink()
                    .map(|(state, history)| EditBuffer { state, history }),
            )
        }
    }

    impl Arbitrary for Row {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Self {
            p!("Arbitrary for Row");
            Row::new(0, a!(g))
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            p!("shrink: quickcheck::single_shrinker(self.clone())");
            quickcheck::single_shrinker(self.clone())
        }
    }

    impl Arbitrary for History {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Self {
            p!("Arbitrary for History");
            let edits: Vec<_> = a!(g);

            let current = if g.gen() || edits.len() == 0 {
                None
            } else {
                Some(g.gen_range(0, edits.len()) as u32)
            };
            if let Some(cur) = current {
                assert!((cur as usize) < edits.len());
            }

            History { edits, current }
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            p!("shrink: let len = self.edits.len();");
            Box::new(self.edits.shrink().map(|new_edits| {
                let current = if new_edits.len() == 0 {
                    None
                } else {
                    Some(new_edits.len() as u32 / 2)
                };

                History {
                    edits: new_edits,
                    current,
                }
            }))
        }
    }

    impl Arbitrary for EditBufferState {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Self {
            p!("Arbitrary for EditBufferState");
            let row_count: u32 = 1;
            // {
            //     let s = g.size();
            //     if s == 0 {
            //         0
            //     } else {
            //         g.gen_range(0, s as u32)
            //     }
            // };

            let mut rows = Vec::new();
            for i in 0..row_count {
                let mut s: String = a!(g);
                while s.len() > 5 {
                    s.pop();
                }
                rows.push(Row::new(i, s));
            }

            EditBufferState {
                cx: g.gen(),
                cy: g.gen(),
                rx: g.gen(),
                row_offset: g.gen(),
                col_offset: g.gen(),
                rows,
                dirty: g.gen(),
                filename: a!(g),
            }
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            p!("shrink: struct EShrink {");
            struct EShrink {
                e: EditBufferState,
            }

            impl Iterator for EShrink {
                type Item = EditBufferState;

                fn next(&mut self) -> Option<EditBufferState> {
                    if let Some(filename) = self.e.filename.shrink().next() {
                        self.e.filename = filename;
                    }

                    if let Some(cx) = self.e.cx.shrink().next() {
                        self.e.cx = cx;
                    }

                    if let Some(cy) = self.e.cy.shrink().next() {
                        self.e.cy = cy;
                    }

                    if let Some(rx) = self.e.rx.shrink().next() {
                        self.e.rx = rx;
                    }

                    if let Some(row_offset) = self.e.row_offset.shrink().next() {
                        self.e.row_offset = row_offset;
                    }

                    if let Some(col_offset) = self.e.col_offset.shrink().next() {
                        self.e.col_offset = col_offset;
                    }

                    if self.e.rows.len() <= 1 {
                        None
                    } else {
                        self.e.rows.pop();
                        Some(self.e.clone())
                    }
                }
            }

            Box::new(EShrink { e: self.clone() })
        }
    }

    impl Arbitrary for Section {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Self {
            let size = (g.size() as u32) / 4;
            if size == 0 {
                Character((0, 0))
            } else {
                Character((g.gen_range(0, size), g.gen_range(0, size)))
            }
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            match *self {
                Character(coord) => Box::new(coord.shrink().map(Character)),
            }
        }
    }

    impl Arbitrary for Edit {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Self {
            match g.gen_range(0, 2) {
                1 => Insert(Section::arbitrary(g), String::arbitrary(g)),
                _ => Remove(Section::arbitrary(g), String::arbitrary(g)),
            }
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            match *self {
                Insert(ref section, ref string) => Box::new(
                    (section.to_owned(), string.to_owned())
                        .shrink()
                        .map(|(se, st)| Insert(se.clone(), st.clone())),
                ),
                Remove(ref section, ref string) => Box::new(
                    (section.to_owned(), string.to_owned())
                        .shrink()
                        .map(|(se, st)| Remove(se.clone(), st.clone())),
                ),
            }
        }
    }

    quickcheck! {
        fn single_char_undo_redo(edit_buffer_: EditBuffer, edits: Vec<One<Edit>>) -> bool {
            let mut edit_buffer = edit_buffer_.clone();

            for edit in edits.iter() {
                perform_edit(&mut edit_buffer, edit);
            }

            latest(&mut edit_buffer);

            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return true;
            }

            while let Changed = undo(&mut edit_buffer) {
                if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                    return true;
                }
            }

            must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
            must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

            false
        }

        fn undo_redo(edit_buffer_: EditBuffer, edits: Vec<Edit>) -> bool {

            let mut edit_buffer = edit_buffer_.clone();

            for edit in edits.iter() {
                perform_edit(&mut edit_buffer, edit);
            }

            latest(&mut edit_buffer);

            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return true;
            }

            while let Changed = undo(&mut edit_buffer) {
                if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                    return true;
                }
            }

            must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
            must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);



            false
        }

        fn redo_undo(edit_buffer_: EditBuffer, edits: Vec<Edit>) -> bool {

            let mut edit_buffer = edit_buffer_.clone();

            for edit in edits.iter() {
                perform_edit(&mut edit_buffer, edit);
            }

            oldest(&mut edit_buffer);

            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return true;
            }

            while let Changed = redo(&mut edit_buffer) {
                if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                    return true;
                }
            }

            must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
            must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);



            false
        }
    }
}

#[cfg(test)]
mod edit_actions_unit {
    use super::*;
    use super::EditorHighlight::*;
    use super::test_helpers::{edit_buffer_isomorphism, edit_buffer_weak_isomorphism,
                              must_edit_buffer_isomorphism, must_edit_buffer_weak_isomorphism};


    #[test]
    fn remove_at_end() {
        let mut edit_buffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "\n".to_string()),
        );
        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 1)), "qweqwe".to_string()),
        );
        perform_edit(
            &mut edit_buffer,
            &Remove(Character((5, 1)), "e".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["".to_string(), "qweqw".to_string()]
        );
    }

    #[test]
    fn valid_between_two_invalid() {
        let mut edit_buffer_ = EditBuffer {
            state: EditBufferState {
                cx: 0,
                cy: 0,
                rx: 0,
                row_offset: 0,
                col_offset: 0,
                rows: vec![
                    Row {
                        index: 0,
                        row: "".to_string(),
                        render: "".to_string(),
                        highlight: vec![],
                        highlight_open_comment: false,
                    },
                ],
                dirty: false,
                filename: None,
            },
            history: History {
                edits: vec![
                    Remove(Character((0, 0)), "B".to_string()),
                    Insert(Character((0, 0)), "A".to_string()),
                    Remove(Character((0, 0)), "B".to_string()),
                ],
                current: Some(0),
            },
        };

        let mut edit_buffer = edit_buffer_.clone();

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn weird_history() {
        let mut edit_buffer_ = EditBuffer {
            state: EditBufferState {
                cx: 0,
                cy: 0,
                rx: 0,
                row_offset: 0,
                col_offset: 0,
                rows: vec![
                    Row {
                        index: 0,
                        row: "1".to_string(),
                        render: "1".to_string(),
                        highlight: vec![Normal],
                        highlight_open_comment: false,
                    },
                ],
                dirty: false,
                filename: None,
            },
            history: History {
                edits: vec![Remove(Character((0, 0)), "".to_string())],
                current: None,
            },
        };

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((1, 0)), "2".to_string()),
        );

        latest(&mut edit_buffer);


        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }


        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn newline_carriage_return_then_undo() {
        let mut edit_buffer_: EditBuffer = Default::default();

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "\n\r".to_string()),
        );

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn carriage_return_newline_then_undo() {
        let mut edit_buffer_: EditBuffer = Default::default();

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "\r\n".to_string()),
        );

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }


        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn c_index_byte_index_confusion() {
        let mut edit_buffer_ = EditBuffer {
            state: EditBufferState {
                cx: 0,
                cy: 0,
                rx: 0,
                row_offset: 0,
                col_offset: 0,
                rows: vec![
                    Row {
                        index: 0,
                        row: "˓".to_string(),
                        render: "˓".to_string(),
                        highlight: vec![Normal],
                        highlight_open_comment: false,
                    },
                ],
                dirty: false,
                filename: None,
            },
            history: History {
                edits: vec![Insert(Character((2, 0)), "\n".to_string())],
                current: None,
            },
        };

        let mut edit_buffer = edit_buffer_.clone();

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }


    #[test]
    fn undo_initial_newline_paste() {
        let mut edit_buffer_: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer_,
            &Insert(Character((0, 0)), "4".to_string()),
        );

        edit_buffer_.history = History {
            edits: vec![Insert(Character((1, 0)), "\n2".to_string())],
            current: None,
        };

        let mut edit_buffer = edit_buffer_.clone();

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn undo_final_newline_paste() {
        let mut edit_buffer_: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer_,
            &Insert(Character((0, 0)), "2".to_string()),
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "4\n".to_string()),
        );

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn undo_internal_newline_paste() {
        let mut edit_buffer_: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer_,
            &Insert(Character((0, 0)), "3".to_string()),
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "1\n2\n".to_string()),
        );

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn handle_invalid_current_index() {
        let mut edit_buffer_ = EditBuffer {
            state: EditBufferState {
                cx: 0,
                cy: 0,
                rx: 0,
                row_offset: 0,
                col_offset: 0,
                rows: vec![
                    Row {
                        index: 0,
                        row: "^]8".to_string(),
                        render: "^]8".to_string(),
                        highlight: vec![Normal, Normal, Normal],
                        highlight_open_comment: false,
                    },
                ],
                dirty: true,
                filename: None,
            },
            history: History {
                edits: vec![],
                current: Some(19),
            },
        };

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "\u{0}".to_string()),
        );

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn troublesome_unicode() {
        let mut edit_buffer_: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer_,
            &Insert(Character((0, 0)), "/˓^".to_string()),
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((2, 0)), "/˓^".to_string()),
        );

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    fn get_hello_world() -> EditBuffer {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "Hello\nWorld".to_string()),
        );

        edit_buffer
    }

    #[test]
    fn insert_multiple_lines() {
        let edit_buffer = get_hello_world();

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["Hello".to_string(), "World".to_string()]
        );
    }

    #[test]
    fn remove_zero_zero() {
        let mut edit_buffer: EditBuffer = get_hello_world();

        perform_edit(
            &mut edit_buffer,
            &Remove(Character((0, 0)), "Hello".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["".to_string(), "World".to_string()]
        );
    }

    #[test]
    fn remove_one_zero() {
        let mut edit_buffer: EditBuffer = get_hello_world();

        perform_edit(
            &mut edit_buffer,
            &Remove(Character((1, 0)), "ello".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["H".to_string(), "World".to_string()]
        );
    }

    #[test]
    fn remove_four_zero() {
        let mut edit_buffer: EditBuffer = get_hello_world();

        perform_edit(
            &mut edit_buffer,
            &Remove(Character((4, 0)), "o".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["Hell".to_string(), "World".to_string()]
        );
    }

    #[test]
    fn remove_five_zero() {
        let mut edit_buffer: EditBuffer = get_hello_world();

        perform_edit(
            &mut edit_buffer,
            &Remove(Character((5, 0)), "\n".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["HelloWorld".to_string()]
        );
    }

    #[test]
    fn remove_newline_zero_zero() {
        let edit_buffer_: EditBuffer = get_hello_world();
        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Remove(Character((0, 0)), "\n".to_string()),
        );

        assert_eq!(edit_buffer.history.edits, edit_buffer_.history.edits);
        assert_eq!(edit_buffer.history.current, edit_buffer_.history.current);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);
    }

    #[test]
    fn insert_ascii() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), 'A'.to_string()),
        );

        if let Some(row) = edit_buffer.state.rows.first() {
            assert_eq!(row.row, 'A'.to_string())
        } else {
            panic!("No row!")
        }
    }

    #[test]
    fn insert_unicode() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), '\u{203B}'.to_string()),
        );

        if let Some(row) = edit_buffer.state.rows.first() {
            assert_eq!(row.row, '\u{203B}'.to_string())
        } else {
            panic!("No row!")
        }
    }

    #[test]
    fn insert_multiple_unicode() {
        let mut edit_buffer: EditBuffer = Default::default();

        let multiple = "\"⁉ᗆ쥔￼";

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), multiple.to_string()),
        );

        if let Some(row) = edit_buffer.state.rows.first() {
            assert_eq!(row.row, multiple.to_string())
        } else {
            panic!("No row!")
        }
    }

    #[test]
    fn non_matching_remove() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "Hello".to_string()),
        );
        perform_edit(
            &mut edit_buffer,
            &Remove(Character((0, 0)), " World!".to_string()),
        );

        if let Some(row) = edit_buffer.state.rows.first() {
            assert_eq!(row.row, "Hello".to_string())
        } else {
            panic!("No row!")
        }
    }

    #[test]
    fn undo_non_matching_remove() {
        let mut edit_buffer_: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer_,
            &Insert(Character((0, 0)), "123".to_string()),
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Remove(Character((2, 0)), "123".to_string()),
        );

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        assert_eq!(
            edit_buffer.history.edits,
            vec![Insert(Character((0, 0)), "123".to_string())]
        );
        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn undo_non_matching_remove_one_to_the_right() {
        let mut edit_buffer_: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer_,
            &Insert(Character((0, 0)), "123".to_string()),
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Remove(Character((1, 0)), "123".to_string()),
        );

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        assert_eq!(
            edit_buffer.history.edits,
            vec![Insert(Character((0, 0)), "123".to_string())]
        );
        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn undo_matching_remove() {
        let mut edit_buffer_: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer_,
            &Insert(Character((0, 0)), "123".to_string()),
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Remove(Character((0, 0)), "123".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .iter()
                .map(|r| r.row.clone())
                .collect::<Vec<_>>(),
            vec!["".to_string()]
        );


        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        assert_eq!(
            edit_buffer.history.edits,
            vec![
                Insert(Character((0, 0)), "123".to_string()),
                Remove(Character((0, 0)), "123".to_string()),
            ]
        );
        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
    }

    #[test]
    fn add_linebreak_at_start() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "Hello".to_string()),
        );

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "\r".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["".to_string(), "Hello".to_string()]
        )
    }

    #[test]
    fn paste_string_containing_linebreak_at_start() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "World".to_string()),
        );
        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "Hello\r".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["Hello".to_string(), "World".to_string()]
        )
    }

    #[test]
    fn remove_from_bad_location() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 7)), "Hello".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["".to_string()]
        )
    }

    #[test]
    fn undo_removal_on_second() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "Hello".to_string()),
        );

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((5, 0)), "\nWorld".to_string()),
        );

        perform_edit(
            &mut edit_buffer,
            &Remove(Character((0, 1)), "Worl".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .iter()
                .map(|r| r.row.clone())
                .collect::<Vec<_>>(),
            vec!["Hello".to_string(), "d".to_string()]
        );

        undo(&mut edit_buffer);


        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["Hello".to_string(), "World".to_string()]
        );
    }

    #[test]
    fn undo_removal_on_first_line() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "Hello".to_string()),
        );

        perform_edit(
            &mut edit_buffer,
            &Remove(Character((0, 0)), "Hell".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .iter()
                .map(|r| r.row.clone())
                .collect::<Vec<_>>(),
            vec!["o".to_string()]
        );

        undo(&mut edit_buffer);


        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["Hello".to_string()]
        );
    }

    #[test]
    fn undo_line_addition() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "Hello".to_string()),
        );
        perform_edit(
            &mut edit_buffer,
            &Insert(Character((3, 0)), "\n".to_string()),
        );

        undo(&mut edit_buffer);

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["Hello".to_string()]
        )
    }

    #[test]
    fn undo_line_addition_at_beginning() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "Hello".to_string()),
        );
        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "\n".to_string()),
        );

        undo(&mut edit_buffer);

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["Hello".to_string()]
        )
    }

    #[test]
    fn undo_line_addition_at_end() {
        let mut edit_buffer: EditBuffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 0)), "Hello".to_string()),
        );
        perform_edit(
            &mut edit_buffer,
            &Insert(Character((5, 0)), "\n".to_string()),
        );

        undo(&mut edit_buffer);


        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["Hello".to_string()]
        )
    }

    #[test]
    fn cannot_make_row_without_newline() {
        let blank_history: History = Default::default();
        let mut edit_buffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 1)), "A".to_string()),
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["".to_string()]
        );
        assert_eq!(edit_buffer.history, blank_history);
    }

    #[test]
    fn cannot_make_row_past_last_row() {
        let mut edit_buffer_ = EditBuffer {
            state: EditBufferState {
                cx: 0,
                cy: 0,
                rx: 0,
                row_offset: 0,
                col_offset: 0,
                rows: vec![
                    Row {
                        index: 0,
                        row: "".to_string(),
                        render: "".to_string(),
                        highlight: vec![],
                        highlight_open_comment: false,
                    },
                ],
                dirty: true,
                filename: None,
            },
            history: History {
                edits: vec![],
                current: None,
            },
        };

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Insert(Character((0, 1)), "\n".to_string()),
        );

        latest(&mut edit_buffer);

        if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
            return;
        }

        while let Changed = undo(&mut edit_buffer) {
            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        must_edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state);
        must_edit_buffer_isomorphism(&edit_buffer.state, &edit_buffer_.state);

        assert!(false)
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
        if let Some(state) = unsafe { STATE.as_mut() } {
            if state.show_console {
                refresh_screen(&mut state.console_state, &mut buf);
                process_console_keypress();
            } else {
                refresh_screen(&mut state.edit_buffer.state, &mut buf);
                process_editor_keypress();
            }
        } else {
            //allow quitting if we have no edit_buffers
            process_editor_keypress();
        }
    }
}
