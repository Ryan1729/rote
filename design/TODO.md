## TODO

* Add a visible cursor, realizing that I will want to have more that one later
  * fix inserting a newline and re-rendering taking noticeably longer than a keystroke
  * fix selecting the cursor position with the mouse taking *noticeably* longer the further down the page you go.
    * if it needs to be slower the further down you go, then it should be fast enough we don't notice.
  * fix weird behavior where deleting the last character takes longer than the other ones
      * is this memory alignment related?
