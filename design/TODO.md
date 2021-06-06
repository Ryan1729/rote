## TODO

* Prove the perf issues are not related to stray logs by making all logs go through an l! macro which also tracks how many bytes were logged in a way that we can show while the app is running.
    * ln! for logging with a newline may make sense.


* Make an unsaved change to a file survive multiple restarts without needing to touch
    it each time.
    * maybe make accidentally closed tabs recoverable?
        * or add confirmation dialog on closing them?
            * maybe have a tab-saftey we can turn off temporairly for when we want to
            close multiple tabs?
    * an intial testing procedure:
        * type into a new scratch buffer
        * close the program
        * open the program (check if typed stuff is stil there)
        * close the program again
        * open the program again (check if typed stuff is stil there)
    * a second testing procedure
        the previous procedure in a non-scratch buffer. Say "text/not_checked_in.txt"

* Write a test that ensures that undo/redo produces the expected spans

* make auto-tab-scroll happen when a new tab is created
    * fix auto-scroll drifting as the amount of tabs increases.
        * a specific undesirable case:
            * If you mash Ctrl-t, then eventually the new tab is invisible

* fix the CRLF/\r\n display issues, or at least figure out a performant way to display control pictures
    * Specifically, the thing where comments ending in CRLF (or at least something weird at the ends) 
    make the lines not line up

* make auto-tab-scroll happen when a tab is switched to with the keyboard

* when a tab is switched to, any fullscreen menus should be hidden
    * currently this is not true if keyboard shortcuts are used. We should make sure
    the menus will be hidden when we do the numeric tab jumping, as well.

* make some way to jump to tab a given tab, say by making numbers appear on tabs when 
holding down modifiers
    * We'll probably want to make "making the current tab visible when changing to it"
     work, before doing this.

* on windows, consider closing the console after flags are handled, unless a debug flag
    is passed to keep it open.

* as part of making the state of the editor more observable, when holding down 
modifier keys, indicate what pressing non-modifiers will do.
    * Where should this go on the screen? Are we realy going to want a bunch of text
    to appear over top of everything when we tap the ctrl key?
        * we could make it fade in, after say an entire second of holding it.
    * Maybe do the numeric tab jumping one first, and see how we feel about this afterwards.

* if multiple things are copied with multiple cursors then if they are pasted with the same number of cursors then 
    they should be pasted separately
    * given the numbers are selected by three cursors represented by "|"
        1|  copy then paste should be 11 not 1123 
        2|                            22     2123 
        3|                            33     3123

* Make Esc pick only one of the mulitple cursors to keep and remove that one's selection if there is one.

* It seems like the editor thread slows down sometimes. Possibly when there are many
files open, or just when the editor has been open a long time
    * We've started measuring and displaying how long it took the last call to 
    `editor::update_and_render` to run. Do we need more visualization than that?
        * If we do, the next obvious step would be a bar chart of the last N view
        renders, where the x axis is the Input variant, and the y axis is duration
        statistics like maximum, mean, median and mode.

* I find myself doubting whether putting the keyboard menu selection in the ui::Id was a good idea.
    * if nothing else, it seems like it would be nice to have the scroll state of the command menu stick around,
        given we have a way to reset the scroll, similarly to the text buffer itself.
        * why not the same keyboard shortcut? There is precedent for that in the tab scroll.
    * So, add fields to the ui::State for the file-switcher state and the command menu state.

* get both command menu and file switcher menus scrolling
    * file switcher 90% works, but the code is ugly.

* make Ctrl-D show the new cursors
    * First it should loop around properly.
    * We want to show whichever cursor was added this time, so I guess add a parameter to `try_to_show_cursors_on`?

* make Ctrl-D match cursor's direction with the initial selection
    * "[abc|]" repesents a selection of "abc" with the cursor at the larger of the two positions
    * current: [abc|] abc abc => [abc|] [|abc] abc
    * desired: [abc|] abc abc => [abc|] [abc|] abc
    * also desired: [|abc] abc abc => [|abc] [|abc] abc

* Ctrl-E to toggle single line comments
  * could probably reuse tab insertion/deletion code.

* prompt to reload files when they are changed on disk by an external program
    * for example, `git`.
    * I guess on linux we want to use `inotify`? Is there a cross-platform lib for this?

* add code folding
    * Ctrl-. to collapse largest uncollapsed region by cursor(s)
    * Ctrl-Shift-. to uncollapse smallest uncollapsed region by cursor(s)
    * maybe Logo-(Shift-). to do full collapse/uncollapse?
    * also a way to do it with mouse?

* fix Ctrl-Left/Right acting weird around unicode characters
    * for example, try using Ctrl-Left/Right on the folowing line
        * U+2261 ≡ "IDENTICAL TO"
        * as of this writing, placing the cursor after the "L" then pressing Ctrl-Left does puts you *after* the I.
    * possible proptest: for any set of characters and cursor position Ctrl-Left then Ctrl-Right then Ctrl-Left should put you in the same spot you were in after the first Ctrl-Left
        * also with Left and Right swapped
    * possible proptest: given you are not on either the first or final line, then Ctrl-Left should always move you at least one character back and Ctrl-Right should move you one character Right.

* report the time the editor thread took to render the previous view. Maybe a rolling average too?
    * how do linux load averages work? Would something like that make sense here?
    * What about a tiny line chart?
        * that doesn't fit into our current text and rectangles setup. 
            * What about a histogram? 
            * Or maybe just draw tall thin rects

* fix slowness that shows up when selecting things with the mouse
    * seems to have appeared when the parsing started so we could probably just start caching
    previous trees, or actually complete the TODO to edit the tree with each keyboard edit.

* fix Ctrl-F not looping properly
    * repro
        * scroll to bottom of multi-screen-length file
        * search for something that is known to be near the top.
        * observe that you are not jumped to the beginning of the file.

* Add a more advanced high-level code manipulation: Extract function
    * user perspective:
        * select an expression
        * press the key combo (Ctrl-Alt-M is traditional)
        * the selected code is converted to a function call to a new function with a default name. Say "funcName"
        * the "funcName" function is created with the appropriate parameters, with a good guess at good names.
        * the user's cursor is jumped to the name of the function but the call is also selected so you can rename it by just typing
    * steps to approach a working system (these can become tests):
        * select some code. press key combo
            * return something different if there main expression is selected/can be inferred from selection
        * given a selected expression, produce function call
            * example that takes 0 params and returns nothing
            * examples that take n params and return nothing
            * examples that take 0 params and return m things
            * examples that take n params and return m things
        * given a selected expression, produce function denifition
            * example that takes 0 params and returns nothing
            * examples that take n params and return nothing
            * examples that take 0 params and return m things
            * examples that take n params and return m things
        * given a selected expression, produce function call and definition and place them appropriately
            * in the outermost scope of the innermost module
        * do the above but also set/check that the cursors are in the right spots.

* refresh search spans on tab-in/out 
    * Should be all edits really; are there other ones we missed? We should only need to put the refresh in one place.
    * Is this fixed now?

* work on fleshing out requirements and implementing this list of features that are enabled by having a parser
    * list
        * extract to variable
        * extract function/method
        * jump to blah
        * error location highlighting/jump
        * add import for thing I just started using in this scope
        * find duplicated code
        * move
        * macro snippet
    * we'll add fleshed out requirements above this in the file

* prevent "No buffer selected" when re-opening already opened file
    * seems fixed?
    * this still happens but only after extended usage apparently.
        * Okay, we can try running a proptest over the relevant code.
    * the Ctrl-P menu is also capable of provoking this FWIW
    * I suspect an issue with the generational indexes based on nothing but intuition.
    * Does it make sense to make a proptest that claims that you can never get to the "No buffer selected" state,
        and then make the relevant generator avoid any cases that really cannot happen? That would hopefully 
        identify the problem, at least.
    * We've now done some corrections to the generational indexes. This may actually be fixed.

* soft focus follows mouse on menus?
    * if the cursor is on the main text when, for example, the find menu is up, then the main text should be scrolled, not the find box.

* Ctrl, and up and down arrow keys to swap the current line with the one above it.
    * If mulktiple lines are selected, swap all of them
    * maybe Ctrl-Alt-Up/Down to swap pased on parse?
        * move smallest entire node that encompasses selection

* Ctrl-shift-f to open a within current project folder search
  * implies some way to know what the project is. Options:
    * custom file format that specifies the paths. Open one of those at startup
    * parse each programming languages files' to figure this out where possible
    * just like ask the user to open one or more project dirs and then keep track of those
      * maybe with file format mentioned above.
  * fallback to open files search if there is no project info
    * Ctrl-alt-f for always open files search?

* add markdown highlighting/parse-enabled features
    * https://github.com/ikatyang/tree-sitter-markdown

* draw an underline below matching braces, parens, brackets when a cursor is next to them.
  * draw a different thing (dotted line?) if there is no matching brace found.
  * jump to matching brace?

* when tabbing in (and out I guess?) insert, (/remove?) extra tab strs automatically depending on the surrounding text.
    * given these lines
    ```
            {
    to tab
            }
    ```
    selecting `to tab` and pressing tab should result in something like this:
    ```
            {
                to tab
            }
    ```
    since that is the likely desired result.
    This might also be acceptable and closer to correct in more cases
    ```
            {
            to tab
            }
    ```
    The following is a case where we would expect the second behaviour
    ```
        {
            a = 1;
    to tab
            a += 1;
        }
    ```
    That is, we want 
    ```
        {
            a = 1;
            to tab
            a += 1;
        }
    ```
    not
    ```
        {
            a = 1;
                to tab
            a += 1;
        }
    ```

* similar logic to the above auto-tabbing should be used to add in tabs when enter is pressed.

* Do some compile-time profiling so I can see what is taking so long to compile and either pull that into a crate (meaning it is compiled less often) or change it in some way to make it compile faster

* have some way to see what whitespace, (including line endings) is in a file.
    * see if we can get conditionally transforming ascii into the control pictures block fast enough
        * if not fast enough to do repeatedly and still scrolling, then at least 
            we could do it once and stick it in a scratch buffer.
    * a built-in hex dump viewer?
    * would conditionally changing the font be better? That way the layout doesn't get messed up.
        * that is, switch to a font with the control pictures glyphs in the ascii slots
        * how hard would moving the glyphs be to do at runtime?
            * having our own program to do that would be preferable to having to go find a font editor
                if we end up wanting more changes later.

* make it such that buffers are not considered edited if their contents matches what is on disk
    * do we want a hash here?
        * we would only want that as an optimization. 
        * let's try without one and see if we run into problems in practice
    * We've added the hash but I'm not positive there aren't still issues here.

* PageUp/Down?
    * maybe these could be jump to matching brace?

* update highlights after edits change the text

* visual feedback on copy
    * as in, copy-paste
    * so say have the selection rectangle shrink towards the cursor?
    * display what is in the copy buffer somewhere

* make internal states clearer
  * whether buffer is selected or not (is cursor blinking enough?)
    * changing the background colour seems annoying
      * we want it to be dark most of the time, and making it brighter emphasizes it too much
    * thin outline? Maybe just an underline?
    * a little icon in the top right?
    * decrease letter/cursor alpha?
      * this is sort of cheating the colour restriction thing, but if we imagine someone being colour-blind then stuff that is out of focus becoming unintelligible doesn't seem like an enormous issue?
      * But, we'd still want to be able to read the text for find and replace.
  * If we really want to do this completely, we could write the following test:
    * test steps:
      * generate an editor state
      * run update and render on the state with an effective no-op and keep the view
      * transform the view _back into_ an editor state
      * check that we all of the differences between the old state and new state
        are acceptable omissions of the state. For instance, not all of the text
        needs to be there.
      * for bonus points ensure the view that is rendered again is identical.
    * the above steps ensure that all the required info from the state is present
    in the view.

* should the cursor blinking be (a fast approximation of) a sine wave instead?

* get line wrapping/text clipping working better
  * re-enable using `WrapInRect`
  * the cursor positions are off
  * is there anything else to this?

* embark on the journey to get multiple `VisibleBuffers` working
  * separate out `gl_layer` into re-usable crate
  * experiment with multiple windows a bit in a separate repo, just to make sure there are no surprises there.
    so we should have things that are shared between the n windows and things that are not.
  * allow multiple different windows
    * each window should have a view onto the same set of open files
      * so, changes in a file that has two different views visible should update immediately
      * refactor to place the responsibility to know which `TextBuffer`(s) should be selected to be on the "client" AKA the platform layer.
        * This will make having multiple windows much nicer, and it allows the client to do things the editor did not expect.
        * Concretely, this means making all the `Input` variants that care about which buffer is used require an additional `BufferId` parameter. Also, the `current_buffer_id` field on the editor `State` will be removed.
          * does it make sense to allow all these "endpoints" to take multiple `BufferId`s?
            * we can make the `out_rx.try_recv()` bit into a loop over, say `0..16` that keeps the latest view and which breaks out of the loop on `TryRecvError::Empty`
              * how should we handle `TryRecvError::Disconnected`? An error message maybe? See item talking about "little pop-up messages".
    * should we make the find and replace its own window?
    * keyboard shortcut to detach current tab into its own window
    * make sure typing works
    * allow dragging tabs outside the window to detach them.
    * allow dragging tabs inside a window to reattach them.

* allow switching between search modes with mouse

* replace for all find modes

* allow opening a new file through the OS GUI
    * if rote is already running then should it open a separate instance or open the file in the running instance?
        * we would like it to work as a git commit/rebase TODO file editor, so maybe we'd want a cli switch to decide?

* Display file tabs that allow the user to switch between open buffers
  * truncate tab names that are too long  with `...`
  * Do we want side-by-side visible buffers?
    * Eventually yes. Having the same buffer visible with two different scroll positions is desirable
      * should the cursor positions be separate (if they are the same buffer)?
        * that is not necessary. The primary use case for two at the same time would be to look at one of them and type in the other.
      * Do we want interior screens or separate windows?
        * Let's try separate windows. That way we don't need to re-invent the OS's window management.
          * Seems like each of the N windows will need to be able to tell whether they are the last window somehow so we can trigger stuff on close. But I guess we could trigger that stuff on each window close?
          * how should the processes be laid out? One editor processes and several UI/window threads?
            * Seems reasonable at the moment. The editor should not need to care about whether there are multiple windows, but it does need to know that two buffers are the same
  * closable tabs
    * button on tabs
    * closing multiple tabs
      * right click menu like in browsers?
  * Ctrl-shift-t to restore last tab
    * We would want a fixed buffer of history like for undo/redo and clipboard history.


* don't allow scrolling emptyness into view?
  * for tabs?
  * for edit text?

* Do we want a way for the editor to show little pop-up messages? Something like "File \"blah.txt\" opened" or "Not implemented"? They would fade away automatically after a period of time.
  * If so, then we would want a way to see the last several messages. Which means we'd want another limited history buffer.
  * Eventually I think we will want these since we plan to attempt integrating external programs through LSP or something similar. And they are going to crash or otherwise complain at some point.
  * go through `wimp.rs` for places that could show an error message but don't. Say everywhere there is an unused `Result` or an `if let Some(...)`

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
    * "Never prompt for this file pair" seems  extraneous. Is there a case where we ever not want it checked?

* handle tab key properly
  * Remaining Features:
    * what should pasting a tab do? insert four spaces instead?
      * Would it be worth it to have a "raw input" mode or window where you can insert Tab characters and all the Ctrl+Letter sequences?
        * What would be the case where you would need that?
    * There's this thread talking about "real tabs" helping visually impaired users. Do we want to do something about that? https://www.reddit.com/r/javascript/comments/c8drjo/nobody_talks_about_the_real_reason_to_use_tabs/
      * Well right now we are in a complexity local minimum regarding mouse positioning which relies on all characters being the same width. If we want to move away from there then I think we should be getting more out of that, like ligature support.
      * Also, why do the characters used in editor need to be the same as what is on disk? We could just make conversion easier if we ever have any visually impaired users (which at this point implies myself getting further visual impairments beyond needing glasses.)

* develop an opinion on where the cursors should be when undoing and redoing. Then, ensure that is the case.

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

* As of this writing there is a bug that seems to only occur after the app is open for a long time, where
the cursor position is way off of where it should be.

* figure out why `#[check_or_no_panic]` seems to always report a panic in `panic_safe_rope`
  * current suspicion: Allocating memory can always panic.  

* Measure the timings and perceived latency without the weird `"time-render"` stuff again. A quick check is not showing a perceived difference anymore
  * see https://gamedev.stackexchange.com/a/173730

* search the code for TODOs
