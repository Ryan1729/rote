extern crate libc;

/*** includes ***/

use libc::{iscntrl, perror, tcgetattr, tcsetattr, termios, CS8, BRKINT, ECHO, ICANON, ICRNL,
           IEXTEN, INPCK, ISIG, ISTRIP, IXON, OPOST, TCSAFLUSH, VMIN, VTIME};
use std::io::{self, ErrorKind, Read};
use std::os::unix::io::AsRawFd;
use std::ffi::CString;

/*** data ***/

// This is a reasonably nice way to have a "uninitialized/zeroed" global,
// given what is stable in Rust 1.21.0
static mut ORIG_TERMIOS: Option<termios> = None;

/*** terminal ***/

fn die(s: &str) {
    if let Ok(c_s) = CString::new(s) {
        unsafe { perror(c_s.as_ptr()) };
    }
    std::process::exit(1);
}

fn disable_raw_mode() {
    if let Some(orig_termios) = unsafe { ORIG_TERMIOS.as_mut() } {
        unsafe {
            if tcsetattr(
                io::stdin().as_raw_fd(),
                TCSAFLUSH,
                orig_termios as *mut termios,
            ) == -1
            {
                die("tcsetattr");
            }
        }
    }
}

fn enable_raw_mode() {
    unsafe {
        ORIG_TERMIOS = Some(std::mem::zeroed());
        if let Some(orig_termios) = ORIG_TERMIOS.as_mut() {
            let stdin_fileno = io::stdin().as_raw_fd();

            if tcgetattr(stdin_fileno, orig_termios as *mut termios) == -1 {
                die("tcgetattr");
            }

            let mut raw = *orig_termios;

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

/*** init ***/

fn main() {
    enable_raw_mode();

    let mut stdin = io::stdin();

    loop {
        let mut buffer = [0; 1];
        stdin
            .read_exact(&mut buffer)
            .or_else(|e| if e.kind() == ErrorKind::UnexpectedEof {
                buffer[0] = 0;
                Ok(())
            } else {
                Err(e)
            })
            .unwrap();

        let c = buffer[0] as char;

        if unsafe { iscntrl(c as _) } != 0 {
            print!("{}\r\n", c as u8);
        } else {
            print!("{} ('{}')\r\n", c as u8, c);
        }

        if c == 'q' {
            break;
        }
    }

    disable_raw_mode();
}
