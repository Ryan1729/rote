# rote - Ryan's Own Text Editor

# NOT EVEN CLOSE TO FUNCTIONAL (... yet?)

I'm starting this project over, this time as a graphical editor from the start instead of a terminal based one.

## Plan
* implment my shortlist of useful keyboard shortcuts
* generally make it a usable terminal editor
  - list of previously opened files
  - keymap?
  - drop down menu?
* abstract out terminal specific portions to a struct of function pointers
* implement another, non-terminal frontend
* add plugin interface based on loading dynamic library files
  - explain that adding different versions of the same library will need a restart because of POSIX
  - allow a single plugin file to perform multiple things. Users should be able to everything they
    want with one plugin file. For good or for ill that allows a plugin that runs scripts to exist.

