## TODO

* fix recenty introduced mouse bugs
    * happened when we changed the way the modifier keys were checked.

* try to integrate tree-sitter
    * get a parser working
    * have the text be coloured based on the parse
        * show scope nesting level?

* do parsing and highlighting selectively
    * default based on file extension
    * allow cycling through options for current tab with keyboard shortcut
        * use Alt-L maybe?

* Ctrl-E to toggle single line comments
  * could probably reuse tab insertion/deletion code.            

* put all keyboard responses into a menu so that any command can be dispatched with the mouse or the keyboard.
    * keyboard responses should be defined in a single place where they end up in the menu and wired up to
        the keyboard
        * would setting up a space to put the menu population code be a good first step?
    * a list of labeled buttons with the keyboard control for each shown
        * make keyboard shortcut manadatory for each action

* Make Esc pick only one of the mulitple cursors to keep and remove that one's selection if there is one.

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

* let the user know how many search results there are somehow
    * The need for this manifests most when there are 0 results, which currently results in no action rather than an explicit indication of that fact.

* when a tab is switched to, any fullscreen menus should be hidden

* soft focus follows mouse on menus?
    * if the cursor is on the main text when, for example, the find menu is up, then the main text should be scrolled, not the find box.

* open file menu relative to the current tab's path if any

* make Ctrl-D show the new cursors

* make Ctrl-D match cursor's direction with the initial selection
    * "[abc|]" repesents a selection of "abc" with the cursor at the larger of the two positions
    * current: [abc|] abc abc => [abc|] [|abc] abc
    * desired: [abc|] abc abc => [abc|] [abc|] abc
    * also desired: [|abc] abc abc => [|abc] [|abc] abc

* Ctrl-shift-f to open a within current project folder search
  * implies some way to know what the project is. Options:
    * custom file format that specifies the paths. Open one of those at startup
    * parse each programming languages files' to figure this out where possible
    * just like ask the user to open one or more project dirs and then keep track of those
      * maybe with file format mentioned above.
  * fallback to open files search if there is no project info
    * Ctrl-alt-f for always open files search?

* draw an underline below matching braces, parens, brackets when a cursor is next to them.
  * draw a different thing (dotted line?) if there is no matching brace found.
  * jump to matching brace?

* prevent "No buffer selected" when re-opening already opened file
    * seems fixed?
    * this still happens but only after extended usage apparently.
        * Okay, we can try running a proptest over the relevant code.
    * the Ctrl-P menu is also capable of provoking this FWIW

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
        
    

* Do some compile-time profiling so I can see what is taking so long to compile and either pull that into a crate (meaning it is compiled less often) or change it in some way to make it compile faster

* make it such that buffers are not considered edited if their contents matches what is on disk
    * do we want a hash here?
        * we would only want that as an optimization. 
        * let's try without one and see if we run into problems in practice

* PageUp/Down?
    * maybe these could be jump to matching brace?

* scrollable file search results list

* update highlights after edits change the text

* visual feedback on copy
    * as in, copy-paste
    * so say have the selection rectangle shrink towards the cursor?

* make auto-tab-scroll happen when a new tab is created
  * fix auto-scroll drifting as the amount of tabs increases.

* make internal states clearer
  * whether buffer is selected or not (is cursor blinking enough?)
    * changing the background colour seems annoying
      * we want it to be dark most of the time, and making it brighter emphasizes it too much
    * thin outline? Maybe just an underline?
    * a little icon in the top right?
    * decrease letter/cursor alpha?
      * this is sort of cheating the colour restriction thing, but if we imagine someone being colour-blind then stuff that is out of focus becoming unintelligible doesn't seem like an enormous issue?
      * But, we'd still want to be able to read the text for find and replace.

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

* figure out why `#[check_or_no_panic]` seems to always report a panic in `panic_safe_rope`
  * current suspicion: Allocating memory can always panic.  

* Measure the timings and perceived latency without the weird `"time-render"` stuff again. A quick check is not showing a perceived difference anymore
  * see https://gamedev.stackexchange.com/a/173730

* search the code for TODOs
