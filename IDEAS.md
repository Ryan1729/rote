# Potential Future Ideas

## Undo Tree

Usually, undo and redo is implemented as a stack, meaning that if a user makes some edits then
undoes some of them, then makes another edit, the edits that were undone are inaccessible,
(in practice most of the edits are still in RAM, only the oldest undone action would have
been overwitten). Undo treews are an alternative whihc keeps "alternate histories" around for longer.
They can be a little more confusing to use however, and visualization of the tree is often helpful.

### Implementation Details

We store the edits performed instead of the entire text buffer in case the user is editing an enormous
file. We also don't only store the text diffs because we want to be able to undo/redo things like
selections and cursor movements. (Redoing a search might be interesting..., maybe store the cursor jump
and not say, the query used?)

```rust
struct UndoTree {
    nodes: Vec<UndoNode>,
}

struct UndoNode {
    key: EditorKey,
    parent: u16,
}
```

Each node has a short pointer to its parent. Newer nodes are added to the end of the tree so the
age of nodes is stored implicitly, making identifying the (chronologically) previous node trivial.

If we only shrink the list at powers of two then we can copy the top half over the bottom half
and just set the apropriate bit to zero to fix all the parent pointers. Note that some nodes
of the old tree may not be connected to the root. These orphaned nodes should be ignored.
(should we mark them as orphaned? The tree needs to be traversed when performing an undo redo anyway, but marking orphans could eleiminate an important amount of traversals)

## Non-semantic folding
Someitmes the structure of the code doesn't allow folding away the part you want to fold away.
What if you could place marks at arbitrary locations and fold everything between them?
