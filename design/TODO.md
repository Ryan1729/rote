## TODO

* Draw the status line background more efficiently
  * Get z working properly so we can insert the status line anywhere in the array and have it display correctly.
  * Add a vertex that just draws a single box across the bottom instead of a bunch of overlapping ones like we do now.

* Add a visible cursor, realizing that I will want to have more that one later
  * fix inserting a newline and re-rendering taking noticeably longer than a keystroke
  * fix selecting the cursor position with the mouse taking *noticeably* longer the further down the page you go.
    * if it needs to be slower the further down you go, then it should be fast enough we don't notice.
  * fix weird behavior where deleting the last character takes longer than the other ones
      * is this memory alignment related?
