## TODO


* merge overlapping cursors, including those with overlapping highlights
  * can we write a test that demonstrates the need for this?
    * why doesn't `multicursor_insertion_should_preserve_this_equality` fail given this is not done yet?
      * `multicursor_insertion_should_preserve_this_equality` does not do everything we want. What we actually want to preserve is that when an insertion happens, each of the inserted characters actually makes it into the buffer and remain after the insertion is done. A way to test this would be to generate characters that do not include a special character and then call `buffer.insert('<special char>')` and then count the occurrences
      of the special character and make sure there is one for each cursor.
        * eventually we will want to insert incrementing numbers at each cursor. Maybe we could implement that and then test that all the numbers are inserted into a buffer with no numbers. That could give us better test failure messages.




* start allowing multiple cursors to be manipulated
  * Ctrl-click to add cursors
    * double Ctrl-click to select word with new cursor
  * Ctrl-D to select word and find next instance of word and select it and place a cursor there.
  * Allow manipulating a single cursor on its own
    * store `cursor_index` per buffer
      * in `editor` or in `text_buffer`?
      * reflect which cursor is the current one in the view
        * colour change? shape change? both?
        * what if that one, and only that one, blinks?
    * add `Input` variant to increment or decrement it.
      * wrapping around to keep the index valid is implied. The platform layer should not need to store this
      * What should the keyboard shortcut be?
        * given we use a alt below, would tapping alt work?
        * maybe alt and page up/down?
    * add `Input` variant to move only the current cursor
      * What should the keyboard shortcut be?
        * hold alt so both ctrl and shift are free to use?
        * a "current cursor mode"?

* scroll into view of cursor if it is moved.
  * how does this work when multiple cursors are moved at once?
    * We need to decide which cursor to follow. Wait until more multi cursor
    stuff is implemented, to see what our choices are.
      * most recently created or moved on it's own seem like the obvious choices,
      but we may end up needing the concept of a `current_cursor` anyway for
      other reasons .

* Ctrl-a select all

* make Undo/Redo history into struct containing `VecDeque` which keeps track of total bytes used and automatically pops things off the end when storage would exceed a limit.
  * add max size parameter so we can test with small sizes
  * test:
    * let `n` be the maximum number of things that it can hold.
    * make a new one and add `n` things to it, assert they are all there.
    * add another item. Assert that only the expected on is missing
  * make sure some kind of undo/redo tests with limited history are performed. We want to ensure that you can undo back to a point just before the history was deleted for, and get back to the final state by redoing.
  * Why is undo/redo so slow?
    * Actually, wait is the index being different than the length causing a bug? We probably need to clear old moves if the length is greater than the index.
    * probably we should just wait until we make a limited bytes used history to bother fixing this?

* Store clipboard history (no persistent storage)
  * Use (parameterized version of?) Undo/Redo struct to store

* Saving and loading files.
  * How easy is it to pop open the system file chooser? Can we get file choosing done with that alone for now?

* Persistence for Undo/Redo and clipboard history, per file.
  * If a file in the history does not exist any more, and a file with a name which is a short Levenstein distance away is opened, prompt to use the old history and change the key in the stored file.
    * should we just hash the content instead?
    * "Never prompt for this file pair" seems extraneous. Is there a case where we ever not want it checked?

* figure out why `#[check_or_no_panic]` seems to always report a panic in `panic_safe_rope`

* Measure the timings and perceived latency without the weird `"time-render"` stuff again. A quick check is not showing a perceived difference anymore
  * see https://gamedev.stackexchange.com/a/173730
