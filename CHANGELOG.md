## 0.9.1 (2020-06-29)

### Bug fixes

Fix accidental use of non-ES5 library methods.

## 0.9.0 (2020-06-08)

### Breaking changes

Upgrade to 0.9 parser serialization

## 0.8.5 (2020-05-01)

### Bug fixes

Publish less useless cruft to npm, reducing package size.

## 0.8.4 (2020-04-14)

### Bug fixes

Fix a bug in `Stack.startOf` that made it fail to find rules that were actually on the stack in many situations.

## 0.8.3 (2020-04-01)

### Bug fixes

Make the package load as an ES module on node

## 0.8.2 (2020-02-28)

### New features

The package now provides an ES6 module.

## 0.8.1 (2020-02-26)

### New features

You can now find the top node type parsed by the parser through its `topType` property.

## 0.8.0 (2020-02-03)

### Breaking changes

The serialized parser format changed.

### New features

Add support for multiple `@top` rules through the `top` parse option.

## 0.7.1 (2020-01-23)

### Bug fixes

Tweak recovery cost for forced reductions to prefer those to other recovery strategies.

More agressively reuse cached nodes.

## 0.7.0 (2020-01-20)

### Breaking changes

This now consumes the adjusted parser output of lezer-generator 0.7.0.

## 0.6.0 (2020-01-15)

### Bug fixes

Rewrite the way the parser advances and recovers from errors, to more rigorously address a number of infinite loops and poor recovery behaviors.

### New features

Parse context objects now have a `badness` property that you can use to estimate just how poorly the input matches the grammar.

## 0.5.2 (2020-01-09)

### Bug fixes

Fix an issue where the parser will sometimes continue, and even pick as result, a parse with error recovery even though there are error-free parses available.

Fix a mistake in our binary heap implementation that would cause stacks to be ordered incorrectly.

Fix an issue where the `Stack.startOf` method would ignore the top frame of the stack.

## 0.5.1 (2020-01-01)

### Bug fixes

Fix an issue where the parser would loop infinitely when leaving a nested parse in some circumstances.

Fix an infinite loop on incomplete input at end of file that occurred for some types of mutually recursive rules.

## 0.5.0 (2019-10-22)

### New features

Parser instances now have a `hasNested` property that tells you whether they nest grammars.

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
