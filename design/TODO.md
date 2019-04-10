## TODO

* fix bug where if you add a newline in the middle of a line then move down and then to the end of the line, then press delete twice, everything after the place where you pressed enter is deleted
  * Actually start writing tests for this stuff!

* Add a visible cursor, realizing that I will want to have more that one later
  * fix selecting the cursor position with the mouse taking *noticeably* longer the further down the page you go.
    * if it needs to be slower the further down you go, then it should be fast enough we don't notice.
  * fix weird behavior where deleting the last character takes longer than the other ones
      * is this memory alignment related?
