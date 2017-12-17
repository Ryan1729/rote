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
use std::cmp::{min, Ord, Ordering};
use std::ops::Index;

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
            refresh_screen($state, &mut display_buf, Default::default());

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
    ShiftHome,
    End,
    ShiftEnd,
}
use EditorKey::*;

impl Default for EditorKey {
    fn default() -> EditorKey {
        Byte(0)
    }
}

fn split_at_ends_mut<T>(slice: &mut [T], index: usize) -> Option<(&mut T, &mut T)> {
    let (before, after) = slice.split_at_mut(index);

    match (before.last_mut(), after.first_mut()) {
        (Some(before_end), Some(after_end)) => Some((before_end, after_end)),
        _ => None,
    }
}

mod selection {
    use ::*;

    //The reason this is in a module and the reason `_new_only` exists is to enforce
    //using `new` to construct `Selection`s, maintaining the fact that earlier is before
    //later.
    #[derive(Clone, Debug, Default, PartialEq, Eq)]
    pub struct Selection {
        pub earlier: (u32, u32),
        pub later: (u32, u32),
        pub cursor_side: CursorSide,
        _new_only: (),
    }

    impl Selection {
        pub fn new(
            (edge_x1, edge_y1): (u32, u32),
            (edge_x2, edge_y2): (u32, u32),
            cursor_side: CursorSide,
        ) -> Self {
            let earlier;
            let later;
            if edge_y1 < edge_y2 || (edge_y1 == edge_y2 && edge_x1 < edge_x2) {
                earlier = (edge_x1, edge_y1);
                later = (edge_x2, edge_y2);
            } else {
                earlier = (edge_x2, edge_y2);
                later = (edge_x1, edge_y1);
            }

            Selection {
                earlier,
                later,
                cursor_side,
                _new_only: (),
            }
        }
        pub fn new_empty(coord: (u32, u32)) -> Self {
            Selection::new(coord, coord, CursorSide::Late)
        }
        pub fn get_next_char_string(&self, rows: &Vec<Row>) -> String {
            let mut s = String::new();
            let (cx, cy) = self.later;

            if let Some(c) = rows[cy as usize].row.chars().nth(cx as usize) {
                s.push(c);
            }

            s
        }
        pub fn get_selected_string(&self, rows: &Vec<Row>) -> Option<String> {
            let mut s = String::new();
            let mut cx = self.earlier.0;
            //we want to distinguish empty selfs, from invalid selfs.
            let mut added_any = false;
            if self.earlier == self.later {
                if let Some(row) = rows.get(self.earlier.1 as usize) {
                    let char_len = char_len(&row.row) as u32;
                    added_any = self.earlier.0 <= char_len;
                }
            } else if self.earlier.1 == self.later.1 {
                let (cx, cy) = self.earlier;
                if let Some(row) = rows.get(cy as usize).map(|row| &row.row) {
                    match (cx_to_byte_x(row, cx), cx_to_byte_x(row, self.later.0)) {
                        (Some(start), Some(end)) => {
                            if start != end {
                                s.push_str(&row[start..end]);
                            }

                            added_any = true;
                        }
                        _ => {}
                    }
                }
            } else {
                let mut not_first_line = false;
                for cy in self.earlier.1..(self.later.1 + 1) {
                    if not_first_line {
                        s.push('\n');
                        added_any = true;
                    }

                    if let Some(row) = rows.get(cy as usize).map(|row| &row.row) {
                        let end_cx = if cy >= self.later.1 {
                            self.later.0
                        } else {
                            row.len() as u32
                        };

                        match (cx_to_byte_x(row, cx), cx_to_byte_x(row, end_cx)) {
                            (Some(start), Some(end)) => {
                                if start != end {
                                    s.push_str(&row[start..end]);
                                }

                                added_any = true;
                            }
                            _ => {}
                        }

                        cx = 0;
                    } else {
                        return None;
                    }

                    not_first_line = true;
                }
            }
            if added_any {
                Some(s)
            } else {
                None
            }
        }
        pub fn get_edge(&self) -> (u32, u32) {
            match self.cursor_side {
                CursorSide::Early => self.earlier,
                CursorSide::Late => self.later,
            }
        }
        pub fn is_empty(&self) -> bool {
            self.earlier == self.later
        }
        pub fn set_pos(&mut self, coord: (u32, u32)) {
            self.earlier = coord;
            self.later = coord;
        }

        pub fn contract_left(&mut self, rows: &Vec<Row>) {
            if self.later.0 != 0 {
                self.later.0 -= 1;
            } else if self.later.1 > 0 {
                let cy = self.later.1 - 1;
                let cx = rows[cy as usize].row.len() as u32;
                self.later = (cx, cy);
            } else {
                //at the top left already!
            }
            if self.is_empty() {
                self.cursor_side = CursorSide::Early;
            }
        }
        pub fn contract_right(&mut self, rows: &Vec<Row>) {
            let row_len = if self.earlier.1 < rows.len() as u32 {
                Some(rows[self.earlier.1 as usize].row.len())
            } else {
                None
            };

            match row_len {
                Some(len) if (self.earlier.0 as usize) < len => {
                    self.earlier.0 += 1;
                }
                Some(len) if (self.earlier.0 as usize) == len => {
                    self.earlier = (0, self.earlier.1 + 1);
                }
                _ => {}
            }

            if self.is_empty() {
                self.cursor_side = CursorSide::Late;
            }
        }
        pub fn contract_up(&mut self, rows: &Vec<Row>) {
            let cy = self.later.1.saturating_sub(1);

            let new_row_len = if cy < rows.len() as u32 {
                rows[cy as usize].row.len() as u32
            } else {
                0
            };

            let old_earlier = self.earlier;

            let moved_part = (min(self.later.0, new_row_len), cy);

            if moved_part.1 <= old_earlier.1 && moved_part.0 < old_earlier.0 {
                self.earlier = moved_part;
                self.later = old_earlier;
            } else {
                self.later = moved_part;
            }
        }
        pub fn contract_down(&mut self, rows: &Vec<Row>) {
            if self.earlier.1 < rows.len() as u32 {
                let cy = self.earlier.1.saturating_add(1);

                let new_row_len = if cy < rows.len() as u32 {
                    rows[cy as usize].row.len() as u32
                } else {
                    0
                };

                let old_later = self.later;

                let moved_part = (min(self.earlier.0, new_row_len), cy);

                if moved_part.1 <= old_later.1 && moved_part.0 < old_later.0 {
                    self.later = moved_part;
                    self.earlier = old_later;
                } else {
                    self.earlier = moved_part;
                }
            }
        }

        pub fn extend_left(&mut self, rows: &Vec<Row>) {
            if self.is_empty() {
                self.cursor_side = CursorSide::Early;
            }
            if self.earlier.0 != 0 {
                self.earlier.0 -= 1;
            } else if self.earlier.1 > 0 {
                let cy = self.earlier.1 - 1;
                let cx = rows[cy as usize].row.len() as u32;
                self.earlier = (cx, cy);
            } else {
                //at the top left already!
            }
        }
        pub fn extend_right(&mut self, rows: &Vec<Row>) {
            if self.is_empty() {
                self.cursor_side = CursorSide::Late;
            }

            let row_len = if self.later.1 < rows.len() as u32 {
                Some(rows[self.later.1 as usize].row.len())
            } else {
                None
            };

            match row_len {
                Some(len) if (self.later.0 as usize) < len => {
                    self.later.0 += 1;
                }
                Some(len) if (self.later.0 as usize) == len => {
                    self.later = (0, self.later.1 + 1);
                }
                _ => {}
            }
        }
        pub fn extend_up(&mut self, rows: &Vec<Row>) {
            let cy = self.earlier.1.saturating_sub(1);

            let new_row_len = if cy < rows.len() as u32 {
                rows[cy as usize].row.len() as u32
            } else {
                0
            };

            self.earlier = (min(self.earlier.0, new_row_len), cy);
        }
        pub fn extend_down(&mut self, rows: &Vec<Row>) {
            if self.later.1 < rows.len() as u32 {
                let cy = self.later.1.saturating_add(1);

                let new_row_len = if cy < rows.len() as u32 {
                    rows[cy as usize].row.len() as u32
                } else {
                    0
                };

                self.later = (min(self.later.0, new_row_len), cy);
            }
        }

        pub fn move_left(&mut self, rows: &Vec<Row>) {
            if self.is_empty() {
                if self.earlier.0 != 0 {
                    let pos = (self.earlier.0 - 1, self.earlier.1);
                    self.set_pos(pos);
                } else if self.earlier.1 > 0 {
                    let cy = self.earlier.1 - 1;
                    let cx = rows[cy as usize].row.len() as u32;
                    self.set_pos((cx, cy));
                } else {
                    //at the top left already!
                }
            } else {
                *self = Selection::new(self.earlier, self.earlier, CursorSide::Early);
            }
        }
        pub fn move_right(&mut self, rows: &Vec<Row>) {
            if self.is_empty() {
                let row_len = if self.earlier.1 < rows.len() as u32 {
                    Some(rows[self.earlier.1 as usize].row.len())
                } else {
                    None
                };

                match row_len {
                    Some(len) if (self.earlier.0 as usize) < len => {
                        let pos = (self.earlier.0 + 1, self.earlier.1);
                        self.set_pos(pos);
                    }
                    Some(len) if (self.earlier.0 as usize) == len => {
                        let pos = (0, self.earlier.1 + 1);
                        self.set_pos(pos);
                    }
                    _ => {}
                }
            } else {
                *self = Selection::new(self.later, self.later, CursorSide::Late);
            }
        }
        pub fn move_up(&mut self, rows: &Vec<Row>) {
            let cy = self.earlier.1.saturating_sub(1);

            let new_row_len = if cy < rows.len() as u32 {
                rows[cy as usize].row.len() as u32
            } else {
                0
            };

            let pos = (min(self.earlier.0, new_row_len), cy);
            self.set_pos(pos);
        }
        pub fn move_down(&mut self, rows: &Vec<Row>) {
            if self.later.1 < rows.len() as u32 {
                let cy = self.earlier.1.saturating_add(1);

                let new_row_len = if cy < rows.len() as u32 {
                    rows[cy as usize].row.len() as u32
                } else {
                    0
                };

                let pos = (min(self.earlier.0, new_row_len), cy);
                self.set_pos(pos);
            }
        }

        fn get_previous_char(&self, rows: &Vec<Row>) -> Option<char> {
            self.get_previous_character_containing_cx_cy(rows)
                .and_then(|(cx, cy)| rows[cy as usize].row.chars().nth(cx as usize))
        }
        fn get_previous_character_containing_cx_cy(&self, rows: &Vec<Row>) -> Option<(u32, u32)> {
            if self.earlier.0 != 0 {
                Some((self.earlier.0 - 1, self.earlier.1))
            } else if self.earlier.1 > 0 {
                let mut cy = self.earlier.1 - 1;
                loop {
                    let len = rows[cy as usize].row.len() as u32;
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

        pub fn get_current_char(&self, rows: &Vec<Row>) -> Option<char> {
            let char_on_this_line = rows[self.later.1 as usize]
                .row
                .chars()
                .nth(self.later.0 as usize);
            if char_on_this_line.is_some() {
                char_on_this_line
            } else {
                self.get_next_character_containing_cx_cy(rows)
                    .and_then(|(cx, cy)| rows[cy as usize].row.chars().nth(cx as usize))
            }
        }
        fn get_next_character_containing_cx_cy(&self, rows: &Vec<Row>) -> Option<(u32, u32)> {
            let mut cy = self.later.1 + 1;
            loop {
                if let Some(row) = rows.get(cy as usize) {
                    if row.row.len() > 0 {
                        return Some((0, cy));
                    }
                } else {
                    return None;
                }
                cy += 1;
            }
        }
        pub fn get_previous_line_len(&self, rows: &Vec<Row>) -> Option<u32> {
            if self.earlier.1 != 0 {
                rows.get(self.earlier.1 as usize - 1)
                    .map(|row| row.row.len() as u32)
            } else {
                None
            }
        }
        pub fn get_next_line_len(&self, rows: &Vec<Row>) -> Option<u32> {
            rows.get(self.later.1 as usize + 1)
                .map(|row| row.row.len() as u32)
        }
    }

    impl Ord for Selection {
        fn cmp(&self, other: &Selection) -> Ordering {
            self.earlier.1.cmp(&other.earlier.1).then_with(|| {
                self.earlier.0.cmp(&other.earlier.0).then_with(|| {
                    self.later
                        .1
                        .cmp(&other.later.1)
                        .then_with(|| self.later.0.cmp(&other.later.0))
                })
            })
        }
    }

    impl PartialOrd for Selection {
        fn partial_cmp(&self, other: &Selection) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum CursorSide {
        Early,
        Late,
    }

    impl Default for CursorSide {
        fn default() -> Self {
            CursorSide::Early
        }
    }

    //The reason this is in a module and the reason `selewctions` is private is to enforce
    //using `insert` to add new selections so that there will never be overlapping selections,
    //since they will all be merged by calls to `compact`. Also this allows maintaining at least one
    //selection at all times
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Selections {
        selections: Vec<Selection>,
        main_index: usize,
    }


    impl Selections {
        pub fn set_cx(&mut self, cx: u32) {
            let cy = self.get_main_selection().get_edge().1;
            self.set_cx_cy((cx, cy));
        }
        pub fn set_cy(&mut self, cy: u32) {
            let cx = self.get_main_selection().get_edge().0;
            self.set_cx_cy((cx, cy));
        }
        pub fn set_cx_cy(&mut self, coords: (u32, u32)) {
            self.selections[self.main_index] = Selection::new_empty(coords);
        }

        pub fn get_main_selection(&self) -> &Selection {
            &self.selections[self.main_index]
        }
        pub fn clear_to_main(&mut self) {
            let main_selection = self.selections.swap_remove(self.main_index);
            self.selections.clear();

            self.selections.push(main_selection);
            self.main_index = 0;
        }
        pub fn len(&self) -> usize {
            self.selections.len()
        }
        pub fn insert(&mut self, sel: Selection) {
            self.selections.push(sel);
            self.compact();
        }
        pub fn get_next_char_strings(&self, rows: &Vec<Row>) -> Vec<String> {
            self.selections
                .iter()
                .map(|sel| sel.get_next_char_string(rows))
                .collect()
        }
        pub fn get_selected_strings(&self, rows: &Vec<Row>) -> Vec<String> {
            self.selections
                .iter()
                .filter_map(|sel| sel.get_selected_string(rows))
                .collect()
        }
        pub fn new(sel: Selection) -> Self {
            Selections {
                selections: vec![sel],
                main_index: 0,
            }
        }

        pub fn move_to_home(&mut self) {
            for sel in self.selections.iter_mut() {
                let line_home = (0, sel.earlier.1);

                *sel = Selection::new(line_home, line_home, CursorSide::Late);
            }

            self.compact();
        }
        pub fn select_to_home(&mut self) {
            for sel in self.selections.iter_mut() {
                let line_home = (0, sel.earlier.1);

                match sel.cursor_side {
                    CursorSide::Early => {
                        *sel = Selection::new(line_home, sel.later, CursorSide::Early);
                    }
                    CursorSide::Late => {
                        *sel = Selection::new(line_home, sel.earlier, CursorSide::Early);
                    }
                }
            }

            self.compact();
        }

        pub fn move_to_end(&mut self, rows: &Vec<Row>) {
            for sel in self.selections.iter_mut() {
                let mut line_end = get_line_end(sel.later, rows);

                *sel = Selection::new(line_end, line_end, CursorSide::Late);
            }

            self.compact();
        }
        pub fn select_to_end(&mut self, rows: &Vec<Row>) {
            for sel in self.selections.iter_mut() {
                let mut line_end = get_line_end(sel.later, rows);

                match sel.cursor_side {
                    CursorSide::Early => {
                        *sel = Selection::new(sel.later, line_end, CursorSide::Late);
                    }
                    CursorSide::Late => {
                        *sel = Selection::new(sel.earlier, line_end, CursorSide::Late);
                    }
                }
            }

            self.compact();
        }

        pub fn move_cursor(&mut self, rows: &Vec<Row>, arrow: Arrow) {
            match arrow {
                Arrow::Left => for sel in self.selections.iter_mut() {
                    sel.move_left(rows);
                },
                Arrow::Right => for sel in self.selections.iter_mut() {
                    sel.move_right(rows);
                },
                Arrow::Up => for sel in self.selections.iter_mut() {
                    sel.move_up(rows);
                },
                Arrow::Down => for sel in self.selections.iter_mut() {
                    sel.move_down(rows);
                },
            }

            self.compact();
        }

        pub fn jump_cursor(&mut self, rows: &Vec<Row>, arrow: Arrow) {
            match arrow {
                Arrow::Left => for sel in self.selections.iter_mut() {
                    let mut seen_non_separator = false;

                    loop {
                        match sel.get_previous_char(rows) {
                            Some(c) if is_separator(c) => if seen_non_separator {
                                break;
                            } else {
                                sel.move_left(rows);
                            },
                            Some(_) => {
                                seen_non_separator = true;
                                sel.move_left(rows);
                            }
                            None => {
                                break;
                            }
                        }
                    }
                },
                Arrow::Right => for sel in self.selections.iter_mut() {
                    let mut seen_non_separator = false;

                    loop {
                        match sel.get_current_char(rows) {
                            Some(c) if is_separator(c) => if seen_non_separator {
                                break;
                            } else {
                                sel.move_right(rows);
                            },
                            Some(_) => {
                                seen_non_separator = true;
                                sel.move_right(rows);
                            }
                            None => {
                                break;
                            }
                        }
                    }
                },
                Arrow::Up => for sel in self.selections.iter_mut() {
                    let mut seen_empty_line = false;

                    loop {
                        match sel.get_previous_line_len(rows) {
                            Some(0) => {
                                seen_empty_line = true;
                                sel.move_up(rows);
                            }
                            Some(_) => if seen_empty_line {
                                break;
                            } else {
                                sel.move_up(rows);
                            },
                            None => {
                                break;
                            }
                        }
                    }
                },
                Arrow::Down => for sel in self.selections.iter_mut() {
                    let mut seen_empty_line = false;

                    loop {
                        match sel.get_next_line_len(rows) {
                            Some(0) => {
                                seen_empty_line = true;
                                sel.move_down(rows);
                            }
                            Some(_) => if seen_empty_line {
                                break;
                            } else {
                                sel.move_down(rows);
                            },
                            None => {
                                break;
                            }
                        }
                    }
                },
            }

            self.compact();
        }

        pub fn adjust_selection(&mut self, rows: &Vec<Row>, arrow: Arrow) {
            match arrow {
                Arrow::Left => for sel in self.selections.iter_mut() {
                    match (sel.is_empty(), sel.cursor_side) {
                        (true, _) | (false, CursorSide::Early) => {
                            sel.extend_left(rows);
                        }
                        (false, CursorSide::Late) => {
                            sel.contract_left(rows);
                        }
                    }
                },
                Arrow::Right => for sel in self.selections.iter_mut() {
                    match (sel.is_empty(), sel.cursor_side) {
                        (true, _) | (false, CursorSide::Late) => {
                            sel.extend_right(rows);
                        }
                        (false, CursorSide::Early) => {
                            sel.contract_right(rows);
                        }
                    }
                },
                Arrow::Up => for sel in self.selections.iter_mut() {
                    match (sel.is_empty(), sel.cursor_side) {
                        (true, _) | (false, CursorSide::Early) => {
                            sel.extend_up(rows);
                        }
                        (false, CursorSide::Late) => {
                            sel.contract_up(rows);
                        }
                    }
                },
                Arrow::Down => for sel in self.selections.iter_mut() {
                    match (sel.is_empty(), sel.cursor_side) {
                        (true, _) | (false, CursorSide::Late) => {
                            sel.extend_down(rows);
                        }
                        (false, CursorSide::Early) => {
                            sel.contract_down(rows);
                        }
                    }
                },
            }

            self.compact();
        }

        pub fn get_cursor_positions(&self) -> Vec<(u32, u32)> {
            self.selections
                .iter()
                .map(|sel| match sel.cursor_side {
                    CursorSide::Early => sel.earlier,
                    CursorSide::Late => sel.later,
                })
                .collect()
        }

        pub fn get_selection_transitions(&self) -> Vec<(u32, u32)> {
            let mut result = Vec::with_capacity(self.selections.len() * 2);

            for sel in self.selections.iter() {
                if !sel.is_empty() {
                    result.push(sel.earlier);
                    result.push(sel.later);
                }
            }

            result
        }

        fn compact(&mut self) {
            self.selections.sort();

            let mut to_remove = Vec::new();

            for i in 0..self.selections.len() {
                if i + 1 == self.selections.len() {
                    break;
                }

                match split_at_ends_mut(&mut self.selections, i + 1) {
                    Some((current, next)) => if current.later.1 > next.earlier.1
                        || (current.later.1 == next.earlier.1 && current.later.0 > next.earlier.0)
                    {
                        *next = Selection::new(current.earlier, next.later, next.cursor_side);
                        to_remove.push(i);
                    },
                    None => {}
                }
            }

            for &i in to_remove.iter().rev() {
                self.selections.remove(i);
            }
        }
    }

    impl Index<usize> for Selections {
        type Output = Selection;

        fn index(&self, i: usize) -> &Selection {
            &self.selections[i]
        }
    }

    impl Default for Selections {
        fn default() -> Self {
            let selections = vec![Default::default()];

            Selections {
                selections,
                main_index: 0,
            }
        }
    }


    fn get_line_end(coord_on_line: (u32, u32), rows: &Vec<Row>) -> (u32, u32) {
        let mut line_end = coord_on_line;
        let cy = line_end.1 as usize;
        if cy < rows.len() {
            line_end.0 = rows[cy].row.len() as u32;
        }
        line_end
    }
}

use selection::{Selection, Selections};

#[derive(Clone, Debug, PartialEq)]
struct Edit {
    selections: Selections,
    past: Vec<String>,
    future: Vec<String>,
}

impl Edit {
    fn new(state: &EditBufferState, future: String) -> Self {
        let selections = state.selections.clone();

        let past = selections.get_selected_strings(&state.rows);
        let future = vec![future; past.len()];

        Edit {
            selections,
            past,
            future,
        }
    }

    fn len(&self) -> usize {
        min(
            self.selections.len(),
            min(self.past.len(), self.future.len()),
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
//only dows anything if `HL_HIGHLIGHT_STRINGS` is set.
const HL_HIGHLIGHT_SINGLE_QUOTE_PREFIX: u32 = 1 << 2;

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
pub struct Row {
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Cleanliness {
    Clean,
    Dirty,
}
use Cleanliness::*;

impl Default for Cleanliness {
    fn default() -> Cleanliness {
        Clean
    }
}

#[derive(Clone, Debug)]
pub struct EditBufferState {
    rx: u32,
    row_offset: u32,
    col_offset: u32,
    rows: Vec<Row>,
    filename: Option<String>,
    selections: Selections,
}

impl EditBufferState {
    fn get_cx(&self) -> u32 {
        self.get_cx_cy().0
    }
    fn get_cy(&self) -> u32 {
        self.get_cx_cy().1
    }
    fn get_cx_cy(&self) -> (u32, u32) {
        self.selections.get_main_selection().get_edge()
    }

    fn set_cx(&mut self, cx: u32) {
        self.selections.set_cx(cx)
    }
    fn set_cy(&mut self, cy: u32) {
        self.selections.set_cy(cy)
    }
    fn set_cx_cy(&mut self, coords: (u32, u32)) {
        self.selections.set_cx_cy(coords)
    }

    fn get_main_selection(&self) -> &selection::Selection {
        self.selections.get_main_selection()
    }

    fn move_cursor(&mut self, arrow: Arrow) {
        self.selections.move_cursor(&self.rows, arrow);
    }

    fn jump_cursor(&mut self, arrow: Arrow) {
        self.selections.jump_cursor(&self.rows, arrow);
    }

    fn adjust_selection(&mut self, arrow: Arrow) {
        self.selections.adjust_selection(&self.rows, arrow);
    }

    fn get_selection_transitions(&self) -> Vec<(u32, u32)> {
        self.selections.get_selection_transitions()
    }
}

impl Default for EditBufferState {
    fn default() -> EditBufferState {
        EditBufferState {
            rx: Default::default(),
            row_offset: Default::default(),
            col_offset: Default::default(),
            //When the user opens a new file, we're pretty sure thay'll want at least one line.
            rows: vec![Default::default()],
            filename: Default::default(),
            selections: Default::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum SelectionType {
    Valid,
    Invalid,
}
use SelectionType::*;

impl EditBufferState {
    fn selection_type(&self, selection: &selection::Selection) -> SelectionType {
        if selection.get_selected_string(&self.rows).is_some() {
            //Should there be a multi-row `SectionType`?
            //Only if we need to react differently than Valid
            Valid
        } else {
            Invalid
        }
    }
}

#[derive(Clone, Debug, Default)]
struct EditBuffer {
    state: EditBufferState,
    history: History,
    saved_history_position: Option<u32>,
}

impl EditBuffer {
    fn cleanliness(&self) -> Cleanliness {
        if self.history.current == self.saved_history_position {
            Clean
        } else {
            Dirty
        }
    }
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

const HLDB: [EditorSyntax; 2] = [
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
    EditorSyntax {
        file_type: "rust",
        file_match: [Some(".rs"), None, None, None, None, None, None, None],
        singleline_comment_start: "//",
        multiline_comment_start: "/*",
        multiline_comment_end: "*/",
        flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS | HL_HIGHLIGHT_SINGLE_QUOTE_PREFIX,
        keywords1: [
            Some("_"),
            Some("abstract"),
            Some("alignof"),
            Some("as"),
            Some("become"),
            Some("box"),
            Some("break"),
            Some("const"),
            Some("continue"),
            Some("crate"),
            Some("do"),
            Some("else"),
            Some("enum"),
            Some("extern"),
            Some("false|"),
            Some("final"),
            Some("fn"),
            Some("for"),
            Some("if"),
            Some("impl"),
            Some("in"),
            Some("let"),
            Some("loop"),
            Some("macro"),
            Some("match"),
            Some("mod"),
            Some("move"),
            Some("mut"),
            Some("offsetof"),
            Some("override"),
            Some("priv"),
            Some("proc"),
        ],
        keywords2: [
            Some("pub"),
            Some("pure"),
            Some("ref"),
            Some("return"),
            Some("Self"),
            Some("self"),
            Some("sizeof"),
            Some("static"),
            Some("struct"),
            Some("super"),
            Some("trait"),
            Some("true|"),
            Some("type"),
            Some("typeof"),
            Some("unsafe"),
            Some("unsized"),
            Some("use"),
            Some("virtual"),
            Some("where"),
            Some("while"),
            Some("yield"),
            Some("bool|"),
            Some("char|"),
            Some("str|"),
            Some("i8|"),
            Some("i16|"),
            Some("i32|"),
            Some("i64|"),
            Some("u8|"),
            Some("u16|"),
            Some("u32|"),
            Some("u64|"),
        ],
        keywords3: [
            Some("isize|"),
            Some("usize|"),
            Some("f32|"),
            Some("f64|"),
            Some("Option|"),
            Some("Result|"),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
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
                    } else if seq[2] == b'J' {
                        match c {
                            b'2' => return ShiftHome,
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
                b'K' => {
                    return ShiftEnd;
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
                && state
                    .edit_buffer
                    .state
                    .rows
                    .get((row.index - 1) as usize)
                    .map(|r| r.highlight_open_comment)
                    .unwrap_or(false);

            let mut char_indices = row.render.char_indices();

            'char_indices: while let Some((i, c)) = char_indices.next() {
                let prev_highlight = row.highlight
                    .last()
                    .map(|&h| h)
                    .unwrap_or(EditorHighlight::Normal);

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

                            if c == '\'' && syntax.flags & HL_HIGHLIGHT_SINGLE_QUOTE_PREFIX != 0 {
                                let mut loop_count: u8 = 0;

                                for ch in row.render[i + 1..].chars() {
                                    loop_count += 1;
                                    //Assuming that an escape means we are in a char literal
                                    //works almost all the time and is much simpler, so we'll
                                    //do that for now.
                                    if ch == '\'' || ch == '\\' {
                                        continue 'char_indices;
                                    } else if loop_count > 1 {
                                        //We're fairly sure we're not in a character literal.
                                        break;
                                    }
                                }
                                in_string = None;
                                //if we're in a quote prefix, we still want keyword highlighting
                                prev_sep = true;
                            }

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
                        break;
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
}

fn row_insert_char(row: &mut Row, cx: u32, c: char) {
    if let Some(i) = cx_to_byte_x(&row.row, cx) {
        row.row.insert(i, c);

        update_row(row);
    }
}

fn row_append_string(row: &mut Row, s: &str) {
    row.row.push_str(s);
    update_row(row);
}

fn row_del_char(row: &mut Row, cx: u32) {
    if let Some(i) = cx_to_byte_x(&row.row, cx) {
        row.row.remove(i);
        update_row(row);
    }
}

/*** editor operations ***/

fn insert_char(state: &mut EditBufferState, (cx, cy): (u32, u32), c: char) {
    state.set_cx_cy((cx, cy));
    insert_char_at_cursor(state, c)
}

fn insert_char_at_cursor(state: &mut EditBufferState, c: char) {
    let (cx, cy) = state.get_cx_cy();
    if cy == state.rows.len() as u32 {
        let at = state.rows.len() as u32;
        insert_row(state, at, String::new());
    }

    row_insert_char(&mut state.rows[cy as usize], cx, c);
    state.set_cx(cx + 1);
}

fn insert_newline(state: &mut EditBufferState, (cx, cy): (u32, u32)) {
    state.set_cx_cy((cx, cy));
    if cx == 0 {
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
    state.set_cx_cy((0, cy + 1));
}

fn del_char(state: &mut EditBufferState, (cx, cy): (u32, u32)) {
    state.set_cx_cy((cx, cy));
    if cy == state.rows.len() as u32 {
        return;
    };

    let char_len = char_len(&state.rows[cy as usize].row) as u32;
    if cx < char_len {
        row_del_char(&mut state.rows[cy as usize], cx);
    } else {
        match split_at_ends_mut(&mut state.rows, cy as usize + 1) {
            Some((previous_row, row)) => {
                state.selections.set_cx(char_len);
                row_append_string(previous_row, &row.row);
            }
            None => die("del_char"),
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
                    state.edit_buffer.saved_history_position = state.edit_buffer.history.current;
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

            buffer_state
                .selections
                .set_cx_cy((row_rx_to_cx(row, index as u32), current as u32));
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
    let (saved_cx, saved_cy) = state.get_cx_cy();
    let saved_col_offset = state.col_offset;
    let saved_row_offset = state.row_offset;

    if prompt!(
        state,
        "Search: {} (Use ESC/Arrows/Enter)",
        Some(&find_callback)
    ).is_none()
    {
        state.set_cx_cy((saved_cx, saved_cy));
        state.col_offset = saved_col_offset;
        state.row_offset = saved_row_offset;
    }
}

/*** output ***/

fn scroll(buffer_state: &mut EditBufferState) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        buffer_state.rx = 0;
        let (cx, cy) = buffer_state.get_cx_cy();
        if cy < buffer_state.rows.len() as u32 {
            buffer_state.rx = row_cx_to_rx(&buffer_state.rows[cy as usize], cx)
        }

        if cy < buffer_state.row_offset {
            buffer_state.row_offset = cy;
        }
        if cy >= buffer_state.row_offset + state.screen_rows {
            buffer_state.row_offset = cy - state.screen_rows + 1;
        }
        if buffer_state.rx < buffer_state.col_offset {
            buffer_state.col_offset = buffer_state.rx;
        }
        if buffer_state.rx >= buffer_state.col_offset + state.screen_cols {
            buffer_state.col_offset = buffer_state.rx - state.screen_cols + 1;
        }
    }
}

fn is_in_this_row(transitions: &Vec<(u32, u32)>, cy: u32) -> bool {
    transitions.last().map(|t| t.1 == cy).unwrap_or(false)
}

fn draw_rows(
    (screen_cols, screen_rows): (u32, u32),
    buffer_state: &EditBufferState,
    buf: &mut String,
) {
    let mut in_selection = false;
    let mut transitions = buffer_state.get_selection_transitions();
    transitions.reverse();

    if buffer_state.filename.is_some() && transitions.len() != 0 {
        c!(transitions);
    }

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
            let right_edge = min(
                char_len(&current_row.render).saturating_sub(buffer_state.col_offset as _),
                screen_cols as usize,
            ) + buffer_state.col_offset as usize;

            let mut in_transition_row = is_in_this_row(&transitions, file_index);

            if buffer_state.filename.is_some() && transitions.len() != 0 {
                c!(y, in_transition_row);
            }

            let mut current_transition: (u32, u32) = (0, 0);
            if in_transition_row {
                current_transition = transitions.pop().unwrap_or((0, 0));
            }

            let mut current_colour = None;
            for (ci, c) in current_row
                .render
                .char_indices()
                .skip(buffer_state.col_offset as _)
            {
                if ci >= right_edge {
                    break;
                }

                if in_transition_row {
                    if buffer_state.filename.is_some() && transitions.len() != 0 {
                        c!("==?", current_transition.0, ci as u32);
                    }
                    if current_transition.0 == ci as u32 {
                        in_selection = !in_selection;
                        c!("toggle to", in_selection, (ci, y));
                        if is_in_this_row(&transitions, file_index) {
                            current_transition = transitions.pop().unwrap_or((0, 0));
                        } else {
                            if buffer_state.filename.is_some() && transitions.len() != 0 {
                                c!("set to false");
                            }
                            in_transition_row = false;
                        }
                    }
                }

                if in_selection {
                    buf.push_str("\x1b[7m");
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
                    match current_row.highlight[ci] {
                        EditorHighlight::Normal => if current_colour.is_some() {
                            buf.push_str("\x1b[39m");
                            current_colour = None;
                        },
                        _ => {
                            let colour = syntax_to_color(current_row.highlight[ci]);
                            if Some(colour) != current_colour {
                                current_colour = Some(colour);
                                buf.push_str(&format!("\x1b[{}m", colour));
                            }
                        }
                    }
                    buf.push(c);
                }

                if in_selection {
                    buf.push_str("\x1b[m");
                }
            }

            if in_transition_row {
                if buffer_state.filename.is_some() && transitions.len() != 0 {
                    c!(
                        "",
                        "==?",
                        current_transition.0,
                        current_row.render.len() as u32
                    );
                }
                if current_transition.0 == current_row.render.len() as u32 {
                    in_selection = !in_selection;
                    c!("", "toggle to", in_selection, (current_row.render.len(), y));
                    if is_in_this_row(&transitions, file_index) {
                        current_transition = transitions.pop().unwrap_or((0, 0));
                    } else {
                        if buffer_state.filename.is_some() && transitions.len() != 0 {
                            c!("", "set to false");
                        }
                        in_transition_row = false;
                    }
                }
            }

            buf.push_str("\x1b[39m");
        }

        buf.push_str("\x1b[K");

        buf.push_str("\r\n");
    }
}

fn draw_status_bar(buffer_state: &mut EditBufferState, buf: &mut String, cleanliness: Cleanliness) {
    if let Some(state) = unsafe { STATE.as_mut() } {
        buf.push_str("\x1b[7m");

        let name = match &buffer_state.filename {
            &Some(ref f_n) => f_n,
            &None => "[No Name]",
        };

        let status = format!(
            "{:.20} - {} lines {}",
            name,
            buffer_state.rows.len(),
            if Dirty == cleanliness {
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
            buffer_state.get_cy() + 1,
            buffer_state.rows.len()
        );

        buf.push_str(&status);

        let screen_cols = state.screen_cols as usize;
        let mut len = min(status.len(), screen_cols);
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
        let msglen = min(state.status_msg.len(), state.screen_cols as usize);

        if msglen > 0
            && Instant::now().duration_since(state.status_msg_time) < Duration::from_secs(5)
        {
            buf.push_str(&state.status_msg[..msglen]);
        }
    }
}

fn refresh_screen(buffer_state: &mut EditBufferState, buf: &mut String, cleanliness: Cleanliness) {
    scroll(buffer_state);
    buf.clear();

    buf.push_str("\x1b[?25l");
    buf.push_str("\x1b[H");

    if let Some(state) = unsafe { STATE.as_mut() } {
        draw_rows((state.screen_cols, state.screen_rows), &buffer_state, buf);
    }
    draw_status_bar(buffer_state, buf, cleanliness);
    draw_message_bar(buf);

    draw_cursors(&buffer_state, buf);

    buf.push_str("\x1b[?25h");

    let mut stdout = io::stdout();
    stdout.write(buf.as_bytes()).unwrap_or_default();
    stdout.flush().unwrap_or_default();
}

fn draw_cursors(buffer_state: &EditBufferState, buf: &mut String) {
    for &(cx, cy) in buffer_state.selections.get_cursor_positions().iter() {
        if let Some(ref row) = buffer_state.rows.get(cy as usize) {
            buf.push_str(&format!(
                "\x1b[{};{}H",
                (cy - buffer_state.row_offset) + 1,
                (row_cx_to_rx(row, cx) - buffer_state.col_offset) + 1
            ));
        }
    }
}

/*** input ***/
fn move_to_end(state: &mut EditBufferState) {
    let cy = state.get_cy() as usize;
    if cy < state.rows.len() {
        let cx = state.rows[cy].row.len() as u32;

        state.set_cx(cx);
    }
}

fn page_up_or_down(state: &mut EditBufferState, screen_rows: u32, page: Page) {
    let cy = match page {
        Page::Up => state.row_offset,
        Page::Down => {
            let new_cy = (state.row_offset + screen_rows).saturating_sub(1);

            min(new_cy, state.rows.len() as u32)
        }
    };

    state.set_cy(cy);

    let arrow = match page {
        Page::Up => Arrow::Up,
        Page::Down => Arrow::Down,
    };

    for _ in 0..screen_rows {
        state.move_cursor(arrow);
    }
}

//handle normal editor operations
fn process_editor_keypress() {
    static mut QUIT_TIMES: u32 = ROTE_QUIT_TIMES;
    let key = read_key();

    let mut possible_edit = None;

    match key {
        Byte(b'\r') => if let Some(state) = unsafe { STATE.as_mut() } {
            possible_edit = Some(Edit::new(&state.edit_buffer.state, "\r".to_owned()));
        },
        //on my keyboard/terminal emulator this results from ctrl-5
        Byte(29) => if let Some(state) = unsafe { STATE.as_mut() } {
            state.show_console = !state.show_console;
        },
        Byte(c0) if c0 == CTRL_KEY!(b'q') => {
            if unsafe { STATE.as_mut() }
                .map(|st| st.edit_buffer.cleanliness() == Dirty)
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
            state.edit_buffer.state.selections.move_to_home();
        },
        ShiftHome => if let Some(state) = unsafe { STATE.as_mut() } {
            state.edit_buffer.state.selections.select_to_home();
        },
        End => if let Some(state) = unsafe { STATE.as_mut() } {
            state
                .edit_buffer
                .state
                .selections
                .move_to_end(&state.edit_buffer.state.rows);
        },
        ShiftEnd => if let Some(state) = unsafe { STATE.as_mut() } {
            state
                .edit_buffer
                .state
                .selections
                .select_to_end(&state.edit_buffer.state.rows);
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
                    EditBufferState::move_cursor(&mut state.edit_buffer.state, Arrow::Left);
                }
                _ => {}
            }

            let past = state
                .edit_buffer
                .state
                .selections
                .get_next_char_strings(&state.edit_buffer.state.rows);


            let mut future = Vec::with_capacity(past.len());
            for _ in 0..past.len() {
                future.push(String::new());
            }

            possible_edit = Some(Edit {
                selections: state.edit_buffer.state.selections.clone(),
                past,
                future,
            });
        },
        Page(page) => if let Some(state) = unsafe { STATE.as_mut() } {
            page_up_or_down(&mut state.edit_buffer.state, state.screen_rows, page);
        },
        Arrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            state.edit_buffer.state.move_cursor(arrow);
        },
        CtrlArrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            state.edit_buffer.state.jump_cursor(arrow);
        },
        ShiftArrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            state.edit_buffer.state.adjust_selection(arrow);
        },
        Byte(c0) if c0 == CTRL_KEY!(b'l') || c0 == b'\x1b' => {}
        Byte(c0) if c0 == 0 => {
            return;
        }
        Byte(c0) => if let Some(state) = unsafe { STATE.as_mut() } {
            possible_edit = Some(Edit::new(
                &state.edit_buffer.state,
                (c0 as char).to_string(),
            ));
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

//handle operations for the editor's console
fn process_console_keypress() {
    let key = read_key();

    match key {
        //on my keyboard/terminal emulator this results from ctrl-5
        Byte(29) => if let Some(state) = unsafe { STATE.as_mut() } {
            state.show_console = !state.show_console;
        },
        Home => if let Some(state) = unsafe { STATE.as_mut() } {
            state.console_state.set_cx(0);
        },
        End => if let Some(state) = unsafe { STATE.as_mut() } {
            move_to_end(&mut state.console_state);
        },
        Byte(c0) if c0 == CTRL_KEY!(b'f') => if let Some(state) = unsafe { STATE.as_mut() } {
            find(&mut state.console_state);
        },
        Page(page) => if let Some(state) = unsafe { STATE.as_mut() } {
            page_up_or_down(&mut state.console_state, state.screen_rows, page);
        },
        Arrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            state.console_state.move_cursor(arrow);
        },
        CtrlArrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            state.console_state.jump_cursor(arrow);
        },
        ShiftArrow(arrow) => if let Some(state) = unsafe { STATE.as_mut() } {
            state.console_state.move_cursor(arrow);
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

        edit_buffer.state.selections.clear_to_main();
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

fn perform_insert(
    state: &mut EditBufferState,
    &selection::Selection { earlier, later, .. }: &selection::Selection,
    (past_str, future_str): (&str, &str),
) -> EditOutcome {
    let outcome = if past_str.len() == 0 {
        Changed
    } else {
        remove_string(state, earlier, past_str)
    };

    if let Changed = outcome {
        insert_string(state, earlier, future_str)
    } else {
        Unchanged
    }
}

fn insert_string(state: &mut EditBufferState, coord: (u32, u32), s: &str) -> EditOutcome {
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

fn remove_string(state: &mut EditBufferState, (cx, cy): (u32, u32), s: &str) -> EditOutcome {
    let mut should_delete = false;

    {
        if s.starts_with('\n') || s.starts_with('\r') {
            let at_end = if let Some(row) = state.rows.get(cy as usize).map(|row| &row.row) {
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
                state.selections.set_cx(row.len() as u32);
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

fn no_history_perform_edit(state: &mut EditBufferState, edit: &Edit) -> EditOutcome {
    let mut outcome = Unchanged;

    for i in 0..edit.len() {
        let selection = &edit.selections[i];
        let past = &edit.past[i];
        let future = &edit.future[i];
        if Valid == state.selection_type(selection) {
            let current_outcome = perform_insert(state, selection, (past, future));
            if outcome == Unchanged {
                outcome = current_outcome;
            }
        }
    }

    outcome
}

fn no_history_unperform_edit(state: &mut EditBufferState, edit: &Edit) -> EditOutcome {
    let mut outcome = Unchanged;

    for i in 0..edit.len() {
        let selection = &edit.selections[i];
        let past = &edit.past[i];
        let future = &edit.future[i];
        if Valid == state.selection_type(selection) {
            let current_outcome = perform_insert(state, selection, (future, past));
            if outcome == Unchanged {
                outcome = current_outcome;
            }
        }
    }

    outcome
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
            && e_b1.col_offset == e_b2.col_offset
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
    use std::string::String;
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
            let history: History = a!(g);
            let saved_history_position = if g.gen() || history.edits.len() == 0 {
                None
            } else {
                Some(g.gen_range(0, history.edits.len()) as u32)
            };
            EditBuffer {
                state: a!(g),
                history,
                saved_history_position,
            }
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            Box::new(
                (
                    self.state.to_owned(),
                    self.history.to_owned(),
                    self.saved_history_position,
                ).shrink()
                    .map(|(state, history, saved_history_position)| {
                        EditBuffer {
                            state,
                            history,
                            saved_history_position,
                        }
                    }),
            )
        }
    }

    impl Arbitrary for Selection {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Self {
            Selection::new(a!(g), a!(g))
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            Box::new(
                (self.earlier, self.later)
                    .shrink()
                    .map(|(e, l)| Selection::new(e, l)),
            )
        }
    }

    impl Arbitrary for Row {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Self {
            Row::new(0, a!(g))
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            quickcheck::single_shrinker(self.clone())
        }
    }

    impl Arbitrary for History {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Self {
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
            let row_count: u32 = 1;
            // {
            //     let s = g.size();
            //     if s == 0 {
            //         0
            //     } else {
            //         g.gen_range(0, s as u32)
            //     }
            // };

            let row_length = 5;

            let mut rows = Vec::new();
            for i in 0..row_count {
                let mut s: String = a!(g);
                while s.len() > row_length {
                    s.pop();
                }
                rows.push(Row::new(i, s));
            }

            let rows_length = rows.len() as u32;

            let selection = Some(Selection::new(
                (
                    g.gen_range(0, row_length as u32),
                    g.gen_range(0, rows_length),
                ),
                (
                    g.gen_range(0, row_length as u32),
                    g.gen_range(0, rows_length),
                ),
            ));

            EditBufferState {
                cx: g.gen(),
                cy: g.gen(),
                rx: g.gen(),
                row_offset: g.gen(),
                col_offset: g.gen(),
                rows,
                filename: a!(g),
                selection,
            }
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
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

    impl Arbitrary for Edit {
        fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Self {
            Edit {
                selection: a!(g),
                past: a!(g),
                future: a!(g),
            }
        }

        fn shrink(&self) -> Box<Iterator<Item = Self>> {
            Box::new(
                (self.selection, self.past.to_owned(), self.future.to_owned())
                    .shrink()
                    .map(|(selection, past, future)| {
                        Edit {
                            selection,
                            past,
                            future,
                        }
                    }),
            )
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
    use std::string::String;
    use super::*;
    use super::EditorHighlight::*;
    use super::test_helpers::{edit_buffer_isomorphism, edit_buffer_weak_isomorphism,
                              must_edit_buffer_isomorphism, must_edit_buffer_weak_isomorphism};


    #[test]
    fn test_index_overrun() {
        let mut edit_buffer_ = EditBuffer {
            state: EditBufferState {
                cx: 909008773,
                cy: 3607610137,
                rx: 2909560752,
                row_offset: 2946809625,
                col_offset: 2659313325,
                rows: vec![
                    Row {
                        index: 0,
                        row: "\u{94}Z7".to_string(),
                        render: "\u{94}Z7".to_string(),
                        highlight: vec![Normal, Normal, Normal],
                        highlight_open_comment: false,
                    },
                ],
                filename: None,
                selection: None,
            },
            history: History {
                edits: vec![],
                current: None,
            },
            saved_history_position: None,
        };

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new((0, 0), (0, 2)),
                past: String::new(),
                future: String::new(),
            },
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
    fn remove_at_end() {
        let mut edit_buffer = Default::default();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "\n".to_string(),
            },
        );
        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 1)),
                past: String::new(),
                future: "qweqwe".to_string(),
            },
        );
        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((5, 1)),
                past: "e".to_string(),
                future: String::new(),
            },
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
        let edit_buffer_ = EditBuffer {
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
                ..Default::default()
            },
            history: History {
                edits: vec![
                    Edit {
                        selection: Selection::new_empty((0, 0)),
                        past: String::new(),
                        future: "B".to_string(),
                    },
                    Edit {
                        selection: Selection::new_empty((0, 0)),
                        past: String::new(),
                        future: "A".to_string(),
                    },
                    Edit {
                        selection: Selection::new_empty((0, 0)),
                        past: String::new(),
                        future: "B".to_string(),
                    },
                ],
                current: Some(0),
            },
            saved_history_position: None,
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
        let edit_buffer_ = EditBuffer {
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

                ..Default::default()
            },
            history: History {
                edits: vec![
                    Edit {
                        selection: Selection::new_empty((0, 0)),
                        past: String::new(),
                        future: String::new(),
                    },
                ],
                current: None,
            },
            saved_history_position: None,
        };

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((1, 0)),
                past: String::new(),
                future: "2".to_string(),
            },
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
        let edit_buffer_: EditBuffer = Default::default();

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "\n\r".to_string(),
            },
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
        let edit_buffer_: EditBuffer = Default::default();

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "\r\n".to_string(),
            },
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
        let edit_buffer_ = EditBuffer {
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
                ..Default::default()
            },
            history: History {
                edits: vec![
                    Edit {
                        selection: Selection::new_empty((2, 0)),
                        past: String::new(),
                        future: "\n".to_string(),
                    },
                ],
                current: None,
            },
            saved_history_position: None,
        };

        let mut edit_buffer = edit_buffer_.clone();

        latest(&mut edit_buffer);

        assert_eq!(
            edit_buffer
                .state
                .rows
                .iter()
                .map(|r| &r.row)
                .collect::<Vec<_>>(),
            vec!["˓"]
        );

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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "4".to_string(),
            },
        );

        edit_buffer_.history = History {
            edits: vec![
                Edit {
                    selection: Selection::new_empty((1, 0)),
                    past: String::new(),
                    future: "\n2".to_string(),
                },
            ],
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "2".to_string(),
            },
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "4\n".to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "3".to_string(),
            },
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "1\n2\n".to_string(),
            },
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
        let edit_buffer_ = EditBuffer {
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
                ..Default::default()
            },
            history: History {
                edits: vec![],
                current: Some(19),
            },
            saved_history_position: None,
        };

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "\u{0}".to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "/˓^".to_string(),
            },
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((2, 0)),
                past: String::new(),
                future: "/˓^".to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "Hello\nWorld".to_string(),
            },
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
    fn remove_multiple_selected_lines() {
        let mut edit_buffer = get_hello_world();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new((1, 0), (2, 1)),
                past: "ello\nWo".to_string(),
                future: String::new(),
            },
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["Hrld".to_string()]
        );
    }

    #[test]
    fn remove_multiple_selected_lines_and_insert() {
        let mut edit_buffer = get_hello_world();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new((1, 0), (2, 1)),
                past: "ello\nWo".to_string(),
                future: "u".to_string(),
            },
        );

        assert_eq!(
            edit_buffer
                .state
                .rows
                .into_iter()
                .map(|r| r.row)
                .collect::<Vec<_>>(),
            vec!["Hurld".to_string()]
        );
    }

    #[test]
    fn remove_zero_zero() {
        let mut edit_buffer: EditBuffer = get_hello_world();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: "Hello".to_string(),
                future: String::new(),
            },
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
            &Edit {
                selection: Selection::new_empty((1, 0)),
                past: "ello".to_string(),
                future: String::new(),
            },
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
            &Edit {
                selection: Selection::new_empty((4, 0)),
                past: "o".to_string(),
                future: String::new(),
            },
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
            &Edit {
                selection: Selection::new_empty((5, 0)),
                past: "\n".to_string(),
                future: String::new(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: "\n".to_string(),
                future: String::new(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: 'A'.to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: '\u{203B}'.to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: multiple.to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "Hello".to_string(),
            },
        );
        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: " World!".to_string(),
                future: String::new(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "123".to_string(),
            },
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((2, 0)),
                past: "123".to_string(),
                future: String::new(),
            },
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
                Edit {
                    selection: Selection::new_empty((0, 0)),
                    past: String::new(),
                    future: "123".to_string(),
                },
            ]
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "123".to_string(),
            },
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((1, 0)),
                past: "123".to_string(),
                future: String::new(),
            },
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
                Edit {
                    selection: Selection::new_empty((0, 0)),
                    past: String::new(),
                    future: "123".to_string(),
                },
            ]
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "123".to_string(),
            },
        );

        assert_eq!(
            edit_buffer_.history.edits,
            vec![
                Edit {
                    selection: Selection::new_empty((0, 0)),
                    past: String::new(),
                    future: "123".to_string(),
                },
            ]
        );

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: "123".to_string(),
                future: String::new(),
            },
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
            assert_eq!(
                edit_buffer.history.edits,
                vec![
                    Edit {
                        selection: Selection::new_empty((0, 0)),
                        past: String::new(),
                        future: "123".to_string(),
                    },
                    Edit {
                        selection: Selection::new_empty((0, 0)),
                        past: "123".to_string(),
                        future: String::new(),
                    },
                ]
            );

            if edit_buffer_weak_isomorphism(&edit_buffer.state, &edit_buffer_.state) {
                return;
            }
        }

        assert_eq!(
            edit_buffer.history.edits,
            vec![
                Edit {
                    selection: Selection::new_empty((0, 0)),
                    past: String::new(),
                    future: "123".to_string(),
                },
                Edit {
                    selection: Selection::new_empty((0, 0)),
                    past: "123".to_string(),
                    future: String::new(),
                },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "Hello".to_string(),
            },
        );

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "\r".to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "World".to_string(),
            },
        );
        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "Hello\r".to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 7)),
                past: "Hello".to_string(),
                future: String::new(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "Hello".to_string(),
            },
        );

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((5, 0)),
                past: String::new(),
                future: "\nWorld".to_string(),
            },
        );

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 1)),
                past: "Worl".to_string(),
                future: String::new(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "Hello".to_string(),
            },
        );

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: "Hell".to_string(),
                future: String::new(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "Hello".to_string(),
            },
        );
        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((3, 0)),
                past: String::new(),
                future: "\n".to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "Hello".to_string(),
            },
        );
        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "\n".to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 0)),
                past: String::new(),
                future: "Hello".to_string(),
            },
        );
        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((5, 0)),
                past: String::new(),
                future: "\n".to_string(),
            },
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
            &Edit {
                selection: Selection::new_empty((0, 1)),
                past: String::new(),
                future: "A".to_string(),
            },
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
        let edit_buffer_ = EditBuffer {
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
                ..Default::default()
            },
            history: History {
                edits: vec![],
                current: None,
            },
            saved_history_position: None,
        };

        let mut edit_buffer = edit_buffer_.clone();

        perform_edit(
            &mut edit_buffer,
            &Edit {
                selection: Selection::new_empty((0, 1)),
                past: String::new(),
                future: "\n".to_string(),
            },
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
                refresh_screen(&mut state.console_state, &mut buf, Default::default());
                process_console_keypress();
            } else {
                let cleanliness = state.edit_buffer.cleanliness();
                refresh_screen(&mut state.edit_buffer.state, &mut buf, cleanliness);
                process_editor_keypress();
            }
        } else {
            //allow quitting if we have no edit_buffers
            process_editor_keypress();
        }
    }
}
