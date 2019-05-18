## High Level Goals

The editor should:
* (optionally) visually respond to all user input. If the cursor didn't do something when a key was pressed then there should an unambiguous indication why.
  * this turns out to involve a lot of tracking and passing things around. I can see why this isn't usually done. I'm not sure whether that's a reason to not do it though.
* be fast, or at least fast enough.
* not crash/panic to the extent reasonable, and if it must do something like that it should never lose data more than say a minute old.
