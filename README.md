# rote - Ryan's Own Text Editor

# CURRENTLY SHELVED

For various reasons I am not feeling the same level of editor related frustration that was one of the motivators of this project. I now have a better idea of how much work a text editor is. Some things are easier than I thought coming in, others are harder. We'll see if I eventually build up enough motivation to return to this, but no promises.

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
* make it windows compatible
  - remove `std::os::unix::io::AsRawFd` if still present
  - by default convert `\\r\\n` to `\\n` on file load and back on file save, allow overridng
