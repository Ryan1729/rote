## TODO

* fix ctrl-f menu being too slow

Once everything above this line is done we can start bootstrapping, (using this editor to edit itself.)

----------------------------

* handle hidpi properly
  * add `--hidpi-override` arg
  * make sure 1.0, 1.5, and 2.0 work.

* Do some compile-time profiling so I can see what is taking so ling to compile and either pull that into a crate (meaning it is compiled less often) or change it in some way to make it compile faster

* Ctrl-E to toggle single line comments
  * could probably reuse tab insertion/deletion code.

* decide whether it would be better to start with a simple shelling out to the compiler to get error messages, or if we should start with trying to integrate RLS
  * the criteria are:
    * would RLS mean we wouldn't need the separate shelling out?
      * seems like it?
    * how easy would it be to get the most important info out of it?
    * if we can avoid total work by starting with RLS, and it's not more then say 2x the work to get the initial parts we want working with RLS, then it seems worth it ta go with RLS first.
      * example: if shelling out takes 100 units of work, and RLS takes 1000 in total, but 150 to get error reporting, then shelling out first costs 1100 overall, but we get error reporting 50 units sooner. whereas if we do RLS that's only 1000 units overall, but we have to wait 50 units longer for in-editor error reporting. If we assume that a unit of waiting is the same cost as a unit of saved work, (they are both proxies for time spent right? so maybe that costing makes sense,) then RLS seems like the better deal.
  * This page brings up some problems: https://www.reddit.com/r/vim/comments/b3yzq4/a_lsp_client_maintainers_view_of_the_lsp_protocol/ that may shift the calculus towards just not doing RLS integration at all. The offsets needing to be converted to utf-16 just to be converted back is annoying (RLS has switched to using utf-16 since that reddit post was written.)
    * what are the features that need language integration that I actually use in environments that
    have them?
      * extract to variable
      * extract function/method
      * jump to blah
      * error location highlighting/jump
      * add import for thing I just started using in this scope
      * find duplicated code
      * move
      * macro snippet
    * To be honest, I like the intellij rust plugin's behaviour better than what I get in Atom currently. Can we just do what they do?
      * intellij-community is under apache 2.0 and the rust plugin is under MIT so maybe?
        * not all of the above list of features are in RLS or intellij-community. So we're going to need to live without at least some of them. The question at this point seems to com down to: How easy is it to build intellij-community? Because if that is a reasonable process, then we could dissect that down to just the minimum needed to replicate the features from that list, (everything but the duplicated code one I think?) and then just re-implement that myself, using their code as an example.
        * result of attempting this: The build process was reasonable-ish. I actually got it working. But, I decided to stop before finishing the teardown, once it got to the ppint where I could discern the following: A lot of the code seems very special case, (at least the "fixes" seem to be doing custom string mangling,) and the AST API doesn't seem particularly worth directly copying. It's very straight forward in some ways, (for example an If Expression consists of the three parts you would expect which are themselves Expressions) but other parts like each PsiElement (PSI stands for Program Structure Interface,) extending a "UserDataHolder" apparently to be more garbage collection friendly make trying to directly port parts of it unappealing. Basically, I feel like if I just solved the problem myself, I would have an as good or better solution, since I don't see any real secret sauce/deep experience in there that I would want to take advantage of.
      * Can we use rust's TokenStream type to help with this?
        * it gives very poor errors (no messages at all apparently?), and sometimes panics, so no.
      * Okay, what if we use [`tree-sitter`](http://tree-sitter.github.io/tree-sitter/)? It seems to be designed specifically for my use case, and there is already a rust grammar made for it. And since `tree-sitter` is written in rust it is likely to be maintained.
        * The documentation is a little light, but I think the process is that I can take the generated `parser.c` and `scanner.c` from [`tree-sitter-rust`](https://github.com/tree-sitter/tree-sitter-rust) and just stick them into my source tree, then using the [`tree-sitter` rust bindings](https://github.com/tree-sitter/tree-sitter/tree/master/lib/binding_rust) I can set up a build.rs and afterwards just pretend `tree-sitter` is a rust library. Then, updating to the newest version of `tree-sitter` would just be as easy as editing the Cargo.toml and overwriting the c files with new versions. We'll see if I misunderstood something when I try it I guess.

* draw an underline below matching braces, parens, brackets when a cursor is next to them.
  * draw a different thing (dotted line?) if there is no matching brace found.
  * jump to matching brace?

* scrollable search results list

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

* Ctrl-shift-f to open a within current project folder search
  * implies some way to know what the project is. Options:
    * custom file format that specifies the paths. Open one of those at startup
    * parse each programming languages files' to figure this out where possible
    * just like ask the user to open one or more project dirs and then keep track of those
      * maybe with file format mentioned above.
  * fallback to open files search if there is no project info
    * Ctrl-alt-f for always open files search?

* allow switching between search modes with mouse

* replace for all find modes

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
