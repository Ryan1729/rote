## TODO

* basic Undo/Redo (no persistent storage)
  * write unimplemented interface for this
    * ctrl-z for undo
    * ctrl-shift-z and ctrl-y for redo
  * property-based test
    * generate sequence of undo/redo actions and figure out what state should be, and make sure that it is correct
  * struct containing `VecDeque` which keeps track of total bytes used
  and automatically pops things off the end when storage would exceed a limit.

* start allowing multiple cursors to be manipulated
  * Ctrl-click to add cursors
  * Allow manipulating a single cursor on its own
    * store `cursor_index` per buffer
      * in `editor` or in `text_buffer`?
      * reflect which cursor is the current one in the view
        * colour change? shape change? both?
        * what if that one, and only that one, blinks?
    * add `Input` variant to increment or decrement it.
      * wrapping around to keep the index valid is implied. The platform layer should not need to store this
      * What should the keyboard shortcut be?
    * add `Input` variant to move only the current cursor
      * What should the keyboard shortcut be?
        * hold alt so both ctrl and shift are free to use?
        * a "current cursor mode"?


* Cut, Copy, Paste, using the system clipboard

* Store clipboard history (no persistent storage)
  * Use (parameterized version of?) Undo/Redo struct to store

* Saving and loading files.
  * How easy is it to pop open the system file chooser? Can we get file choosing done with that alone for now?

* Persistence for Undo/Redo and clipboard history, per file.
  * If a file in the history does not exist any more, and a file with a name which is a short Levenstein distance away is opened, prompt to use the old history and change the key in the stored file.
    * should we just hash the content instead?
    * "Never prompt for this file pair" seems extraneous. Is there a case where we ever not want it checked?

* figure out why `#[check_or_no_panic]` seems to always report a panic in `panic_safe_rope`
