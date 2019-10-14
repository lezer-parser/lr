## 0.4.1 (2019-10-14)

### Bug fixes

Fix an infinite loop where error recovery keeps finding zero-length tokens and imagining it's making progress.

## 0.4.0 (2019-09-10)

### Bug fixes

Don't rely on additional data stored in the parse table during recovery (shrinking the parse tables).

Fix a crash that could occur when starting a nested parse when there were multiple active stacks.

Fix an issue where error nodes would sometimes not be merged.

Don't reuse cached tokens for states that have a different token group.

### Breaking changes

The on-disk parse table format changed again.

## 0.3.0 (2019-08-22)

### Bug fixes

Don't treat reused nodes as if they are error terms when merging errors.

Add badness penalty for forced reductions at end of input.

Fix several infinite loops around forced reductions.

Don't move error nodes out of reduces.

### New features

Add a `Parser.withProps` method for extending a parser with new node props.

### Breaking changes

Emits lezer-tree 0.3.0 style trees with `NodeType` type objects.

`Parser.deserialize`'s interface changed (now taking an object rather than a long list of parameters).

## 0.2.0 (2019-08-02)

### Bug fixes

Don't include lezer-tree inline in `dist/index.js`.

### New features

The output tree now uses tags, rather than term names, to identify its nodes.

Export `Tag` data structure from lezer-tree.

Support per-grammar global tag suffixes in `Parser.deserialize`.

### Breaking changes

Grammars no longer have ids.

Removes export of `allocateGrammarID` and `TagMap`.

## 0.1.1 (2019-07-09)

### Bug Fixes

Actually include the .d.ts file in the published package.

## 0.1.0 (2019-07-09)

### New Features

First documented release.
