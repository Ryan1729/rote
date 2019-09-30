## TODO

* if a file was already opened, switch to that buffer instead of opening a new version of that file.
  * Store the path on each buffer.
    * how should we handle scratch buffers?
      * ```
        enum BufferName {
            Path(Pathbuf),
            Scratch(u32),
        }
        ```
* Display file tabs that allow the user to switch between open buffers
  * show file path on tabs.
  * Do we want side-by-side visible buffers?
    * Eventually yes. Having the same buffer visible with two different scroll positions is desirable
      * should the cursor positions be separate?
        * that is not necessary. The primary use case for two at the same time would be to look at one of them and type in the other.
      * Do we want interior screens or separate windows?
        * Let's try separate windows. That way we don't need to re-invent the OS's window management.
          * Seems like each of the N windows will need to be able to tell whether they are the last window somehow so we can trigger stuff on close. But I guess we could trigger that stuff on each window close?
          * how should the processes be laid out? One editor processes and several UI/window threads?
            * Seems reasonable at the moment. The editor should not need to care about whether there are multiple windows, but it does need to know that two buffers are the same
  * Ctrl-t to make a new tab
  * Ctrl-shift-t to restore last tab
    * We would want a fixed buffer of history like for undo/redo and clipboard history.

* Do we want a way for the editor to show little pop-up messages? Something like "File \"blah.txt\" opened" or "Not implemented"? They would fade away automatically after a period of time.
  * If so, then we would want a way to see the last several messages. Which means we'd want another limited history buffer.
  * Eventually I think we will want these since we plan to attempt integrating external programs through LSP or something similar. And they are going to crash or otherwise complain at some point.

* Saving and loading files.
  * saving new files to a new place on disk with ctrl-s
  * saving pre-existing files to disk on ctrl-s
    * ctrl-shift-s to save pre-existing file as a new file



* Ctrl-d to select word and find next instance of word and select it and place a cursor there.

* Ctrl-p open a list of open files, with a search box.
  * is it okay for this to take over the whole screen?

* Ctrl-Alt-t to transpose characters like in the `bash` prompt, (where Ctrl-t is used).
  * we want Ctrl-T as a keyboard shortcut for making a new tab.
  * I guess doing this at every cursor make sense.
  * What does this do to selections?
    * "nothing" (aka leaving them alone) seem sensible here.


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

* Persistence for Undo/Redo and clipboard history, per file.
  * If a file in the history does not exist any more, and a file with a name which is a short Levenstein distance away is opened, prompt to use the old history and change the key in the stored file.
    * should we just hash the content instead?
    * "Never prompt for this file pair" seems extraneous. Is there a case where we ever not want it checked?

* handle tab key properly
  * Remaining Features:
    * what should pasting a tab do? insert four spaces instead?
      * Would it be worth it to have a "raw input" mode or window where you can insert Tab characters and all the Ctrl+Letter sequences?
        * What would be the case where you would need that?
    * There's this thread talking about "real tabs" helping visually impaired users. Do we want to do something about that? https://www.reddit.com/r/javascript/comments/c8drjo/nobody_talks_about_the_real_reason_to_use_tabs/
      * Well right now we are in a complexity local minimum regarding mouse positioning which relies on all characters being the same width. If we want to move away from there then I think we should be getting more out of that, like ligature support.
      * Also, why do the characters used in editor need to be the same as what is on disk? We could just make conversion easier if we ever have any visually impaired users (which at this point implies myself getting further visual impairments beyond needing glasses.)

* Have tab edits be collected together for the purposes of undo
  * options
    * Undo/redo fence
    * stuff it into one edit
    * make `Edit` contain a `Vec` of edits

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

* figure out why `#[check_or_no_panic]` seems to always report a panic in `panic_safe_rope`
  * current suspicion: Allocating memory can always panic.

* Measure the timings and perceived latency without the weird `"time-render"` stuff again. A quick check is not showing a perceived difference anymore
  * see https://gamedev.stackexchange.com/a/173730
