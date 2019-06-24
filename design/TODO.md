## TODO

* basic Undo/Redo (no persistent storage)
  * write unimplemented interface for this
    * ctrl-z for undo
    * ctrl-shift-z and ctrl-y for redo
  * property-based test
    * generate sequence of undo/redo actions and figure out what state should be, and make sure that it is correct
  * struct containing `VecDeque` which keeps track of total bytes used
  and automatically pops things off the end when storage would exceed a limit.


* Cut, Copy, Paste, using the system clipboard

* Store clipboard history (no persistent storage)
  * Use (parameterized version of?) Undo/Redo struct to store

* Saving and loading files.
  * How easy is it to pop open the system file chooser? Can we get file choosing done with that alone for now?

* Persistence for Undo/Redo and clipboard history, per file.
