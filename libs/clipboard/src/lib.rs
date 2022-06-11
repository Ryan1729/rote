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
pub use crate::common::ClipboardProvider;

#[cfg(all(unix, not(any(target_os="macos", target_os="android", target_os="emscripten"))))]
pub mod x11_clipboard;

#[cfg(windows)]
pub mod windows_clipboard;

#[cfg(target_os="macos")]
pub mod osx_clipboard;

pub mod nop_clipboard;

#[cfg(all(unix, not(any(target_os="macos", target_os="android", target_os="emscripten"))))]
pub type ClipboardContext = x11_clipboard::X11ClipboardContext;
#[cfg(windows)]
pub type ClipboardContext = windows_clipboard::WindowsClipboardContext;
#[cfg(target_os="macos")]
pub type ClipboardContext = osx_clipboard::OSXClipboardContext;
#[cfg(target_os="android")]
pub type ClipboardContext = nop_clipboard::NopClipboardContext; // TODO: implement AndroidClipboardContext (see #52)
#[cfg(not(any(unix, windows, target_os="macos", target_os="android", target_os="emscripten")))]
pub type ClipboardContext = nop_clipboard::NopClipboardContext;

pub mod clipboard_layer {
    use crate as clipboard;
    pub use crate::common::ClipboardProvider;
    use std::error::Error;

    /// This enum exists so we can do dynamic dispatch on `ClipboardProvider` instances even though
    /// the trait requires `Sized`. The reason  we want to do that, is so that if we try to run this
    /// on a platform where `clipboard::ClipboardContext::new` retirns an `Err` we can continue
    /// operation, just without system clipboard support.
    pub enum Clipboard {
        System(clipboard::ClipboardContext),
        Fallback(clipboard::nop_clipboard::NopClipboardContext),
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

    impl clipboard::ClipboardProvider for Clipboard {
        fn new() -> Result<Self, Box<dyn Error>> {
            let result: Result<
                clipboard::ClipboardContext,
                clipboard::nop_clipboard::NopClipboardContext,
            > = clipboard::ClipboardContext::new().map_err(|err| {
                eprintln!("System clipboard not supported. {}", err);
                // `NopClipboardContext::new` always returns an `Ok`
                clipboard::nop_clipboard::NopClipboardContext::new().unwrap()
            });

            let output = match result {
                Ok(ctx) => Clipboard::System(ctx),
                Err(ctx) => Clipboard::Fallback(ctx),
            };

            // `get_clipboard` currently relies on this neer returning `Err`.
            Ok(output)
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

    pub fn get_clipboard() -> Clipboard {
        // As you can see in the implementation of the `new` method, it always returns `Ok`
        Clipboard::new().unwrap()
    }
}

#[test]
fn test_clipboard() {
    let mut ctx = ClipboardContext::new().unwrap();
    ctx.set_contents("some string".to_owned()).unwrap();
    assert!(ctx.get_contents().unwrap() == "some string");
}
