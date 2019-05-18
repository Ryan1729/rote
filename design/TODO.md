## TODO

* We need to be able to see spaces and newline characters.
  * Should we edit the font, or render over top of the characters? Do we care about being able to not see spaces and newlines if we want to? How long does it take to change fonts?

* Make moving the cursor deep in the file faster. In particular, the cursor moving after clicking on a position is too slow.
  * I don't currently see a way to do this for the clicking case, besides caching byte offsets.
    * We could scan backwards or forwards from the current gap point to do up and down movement, but clicking speed would be unaffected.
  * If we are going to cache byte offsets, we might as well get the most bang for our buck. I think the best way to do that is to
  save the byte offsets of the middle of the buffer, the one-quarter mark and the three-quarters mark and so own.
  The natural way to store this is in a binary tree, which we can store in an array with some slightly clever indexing.
    * Should the tree grow as the file grows?
    * Seems like this could have a lot of edge cases. Should we maybe try to write a bunch of tests for it first?
      * property we want to test: after any number of random edit operations, the cached byte indexes are:
        * valid character indexes
        * in order
        * optimal (optionally?)
      * We can write a slow version to calculate the proper answer and check that after random edit operations we get the same answer.

* Add a visible cursor, realizing that I will want to have more that one later
  * fix inserting a newline and re-rendering taking noticeably longer than a keystroke
  * fix selecting the cursor position with the mouse taking *noticeably* longer the further down the page you go.
    * if it needs to be slower the further down you go, then it should be fast enough we don't notice.
  * fix weird behavior where deleting the last character takes longer than the other ones
      * is this memory alignment related?
