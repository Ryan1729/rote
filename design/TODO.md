## TODO

* fix weird newline bug
  * does switching back to "official" `unicode-segmentation` fix it?
  * Have a way to show whitespace characters

* Add a visible cursor, realizing that I will want to have more that one later
  * fix selecting the cursor position with the mouse taking *noticeably* longer the further down the page you go.
    * if it needs to be slower the further down you go, then it should be fast enough we don't notice.
  * make it so if you click on the right half of a character the cursor moves tot he position after the letter.
  * if you click out of bounds on a line, then move the cursor to the closest valid spot on that line.
  * fix weird behavior where deleting the last character takes longer than the other ones
      * is this memory alignment related?
* Try to implement a version of character ranges that uses the range operator (..)
