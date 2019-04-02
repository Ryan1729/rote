## TODO

* Add a visible cursor, realizing that I will want to have more that one later
  * fix weird behavior where the cursor can end up in places where both sides of the cursor are `None` or the characters are misaligned even though the insert and delete links up with the cursor's appearance
  * fix weird behavior where deleting the last character takes longer than the other ones
      * is this memory alignment related?
* Try to implement a version of character ranges that uses the range operator (..)
