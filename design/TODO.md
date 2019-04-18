## TODO

* Draw the status line background more efficiently
  * Get z working properly so we can insert the status line anywhere in the array and have it display correctly.
  * Add a vertex that just draws a single box across the bottom instead of a bunch of overlapping ones like we do now.
  * Since we seems to be getting some texture lookup issues which is producing unwanted transparency
  (I currently think that we are sampling outside of the filled area) let's just add an override alpha parameter to the vertex type
  `max(texture(font_tex, f_tex_pos).r, override_alpha)` seems like the least branchy way to write this.

* Add a visible cursor, realizing that I will want to have more that one later
  * fix inserting a newline and re-rendering taking noticeably longer than a keystroke
  * fix selecting the cursor position with the mouse taking *noticeably* longer the further down the page you go.
    * if it needs to be slower the further down you go, then it should be fast enough we don't notice.
  * fix weird behavior where deleting the last character takes longer than the other ones
      * is this memory alignment related?
