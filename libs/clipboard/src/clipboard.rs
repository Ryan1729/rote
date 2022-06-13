/*
Copyright 2016 Avraham Weinstock

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#[cfg(all(unix, not(any(target_os="macos", target_os="android", target_os="emscripten"))))]
extern crate x11_clipboard as x11_clipboard_crate;

#[cfg(windows)]
extern crate clipboard_win;

#[cfg(target_os="macos")]
#[macro_use]
extern crate objc;
#[cfg(target_os="macos")]
extern crate objc_id;
#[cfg(target_os="macos")]
extern crate objc_foundation;

mod common;
use crate::common::ClipboardProvider;

#[cfg(all(unix, not(any(target_os="macos", target_os="android", target_os="emscripten"))))]
mod x11_clipboard;

#[cfg(windows)]
mod windows_clipboard;

#[cfg(target_os="macos")]
mod osx_clipboard;

mod nop_clipboard;

#[cfg(all(unix, not(any(target_os="macos", target_os="android", target_os="emscripten"))))]
type ClipboardContext = x11_clipboard::Context;
#[cfg(windows)]
type ClipboardContext = windows_clipboard::Context;
#[cfg(target_os="macos")]
type ClipboardContext = osx_clipboard::Context;
#[cfg(target_os="android")]
type ClipboardContext = nop_clipboard::Context; // TODO: implement AndroidClipboardContext (see #52)
#[cfg(not(any(unix, windows, target_os="macos", target_os="android", target_os="emscripten")))]
type ClipboardContext = nop_clipboard::Context;

use std::error::Error;

/// This enum exists so we can do dynamic dispatch on `ClipboardProvider` 
/// instances even though the trait requires `Sized`. The reason  we want to do
/// that, is so that if we try to run this on a platform where 
/// `ClipboardContext::new` returns an `Err` we can continue operation, just 
/// without system clipboard support.
pub enum Clipboard {
    System(ClipboardContext),
    Fallback(nop_clipboard::Context),
}

impl core::fmt::Debug for Clipboard {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use Clipboard::*;
        write!(f, "{}", match self {
            System(_) => {
                "System(_)"
            },
            Fallback(_) => {
                "Fallback(_)"
            },
        })
    }
}

#[must_use]
pub fn get_clipboard() -> Clipboard {
    let result: Result<
        ClipboardContext,
        nop_clipboard::Context,
    > = ClipboardContext::new().map_err(|err| {
        eprintln!("System clipboard not supported. {}", err);
        nop_clipboard::Context
    });

    match result {
        Ok(ctx) => Clipboard::System(ctx),
        Err(ctx) => Clipboard::Fallback(ctx),
    }
}

impl Clipboard {
    /// # Errors
    /// Returns the errors that the platform specific implementation does.
    pub fn get(&mut self) -> Result<String, Box<dyn Error>> {
        self.get_contents()
    }

    /// # Errors
    /// Returns the errors that the platform specific implementation does.
    pub fn set(&mut self, s: String) -> Result<(), Box<dyn Error>> {
        self.set_contents(s)
    }
}

impl ClipboardProvider for Clipboard {
    fn new() -> Result<Self, Box<dyn Error>> {
        Ok(get_clipboard())
    }
    fn get_contents(&mut self) -> Result<String, Box<dyn Error>> {
        match self {
            Clipboard::System(ctx) => ctx.get_contents(),
            Clipboard::Fallback(ctx) => ctx.get_contents(),
        }
    }
    fn set_contents(&mut self, s: String) -> Result<(), Box<dyn Error>> {
        match self {
            Clipboard::System(ctx) => ctx.set_contents(s),
            Clipboard::Fallback(ctx) => ctx.set_contents(s),
        }
    }
}


#[test]
fn test_clipboard() {
    let mut ctx = ClipboardContext::new().unwrap();
    ctx.set_contents("some string".to_owned()).unwrap();
    assert!(ctx.get_contents().unwrap() == "some string");
}
