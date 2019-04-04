## TODO

* Write a crate that exports macros that use re-exports from https://github.com/TyOverby/flame and possibly https://github.com/llogiq/flamer to profile the code that is causing the slowdown described below.
  Using re-exports allows us to only have to use one import in each of the crates that use the macros, and using macros allows us to insert `flame` drop guards in the correct functions. An alternative would be to use `flamer` attributes in the straight-forward way but if I want to make it enabled with a `feature` argument to `cargo` that would require wiring features through each crate. Given the current restrictions on `flamer` and crates like it, (without inner attributes, that is, `#![...]`, each `fn` must be annotated separately), I don't see much difference between using the attributes and just adding a one-line macro inside the functions themselves. When/if the restrictions are lifted, then there is a clear advantage, but until then a `flame!()` macro seems just as good as `#[flame]`.

* Add a visible cursor, realizing that I will want to have more that one later
  * fix selecting the cursor position with the mouse taking *noticeably* longer the further down the page you go.
    * if it needs to be slower the further down you go, then it should be fast enough we don't notice.
  * make it so if you click on the right half of a character the cursor moves tot he position after the letter.
  * if you click out of bounds on a line, then move the cursor to the closest valid spot on that line.
  * fix weird behavior where deleting the last character takes longer than the other ones
      * is this memory alignment related?
* Try to implement a version of character ranges that uses the range operator (..)
