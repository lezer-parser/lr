## 1.2.1 (2022-08-03)

### Bug fixes

Include `LRParser.deserialize` in the TypeScript declarations.

Fix a bug that broke `ParserConfig.specializers` (requires @lezer/generator 1.1.1).

## 1.2.0 (2022-07-04)

### New features

External specializers can now be replaced when reconfiguring an `LRParser`.

## 1.1.0 (2022-06-27)

### New features

Support the output produced by the new `@eof` marker in lezer-generator.

## 1.0.0 (2022-06-06)

### New features

First stable version.

## 0.16.3 (2022-05-11)

### Bug fixes

Don't crash when a non-Node environment defines a `process` global that doesn't have an `env` property.

Fix a bug that could cause partially-parsed nodes to be reused when they ended in skipped nodes, when parsing incrementally.

## 0.16.2 (2022-04-21)

### Bug fixes

Fix an issue that could make the parser merge adjacent error nodes even when they had different parent nodes.

## 0.16.1 (2022-04-21)

### Bug fixes

Fix an issue that caused error nodes created for inserted tokens to sometimes end up in the wrong parent node.

## 0.16.0 (2022-04-20)

### Bug fixes

Avoid the need for generated parser files to directly import @lezer/common.

## 0.15.8 (2022-02-04)

### Bug fixes

Fix a bug that caused reductions that didn't consume anything to sometimes end up outside their parent node in the tree.

## 0.15.7 (2022-01-21)

### Bug fixes

Fix a bug that could cause some kinds of composite skipped expressions to not be parsed correctly.

## 0.15.6 (2022-01-11)

### Bug fixes

Make sure tree depth does not grow unbounded, so that recursive tree traversal is safe from overflowing the stack.

Be less agressive about pruning long-running GLR parse splits.

## 0.15.5 (2021-12-01)

### Bug fixes

Fix a bug that caused node lookahead to be one less than it should be.

Fix an issue that could cause the parser to, when recovering, parse beyond the position given to `stopAt`.

## 0.15.4 (2021-09-27)

### Bug fixes

Fix a mistake in the way forced reductions are checked.

## 0.15.3 (2021-09-24)

### Bug fixes

Fix crashes or infinite recursion caused by applying forced reductions in situations where they were not valid.

## 0.15.2 (2021-08-31)

### Bug fixes

Fix a bug where the parse position could get corrupted during a parse, leading to crashes or nonsensical output.

## 0.15.1 (2021-08-16)

### Bug fixes

Fix an inconsistency in the .d.ts emitted by the build.

### New features

`LRParser.configure` now takes a `contextTracker` option to replace the context tracker used by the parser.

## 0.15.0 (2021-08-11)

### Breaking changes

The module's name changed from `lezer` to `@lezer/lr`.

The `Parser` class was renamed to `LRParser` (`Parser` is now the abstract class that all parsers extend).

Nested parsing is no longer handled inside the LR parser (instead, it is done by using `parseMixed` from the `@lezer/common` package as a wrapper).

External tokenizers are passed different arguments and get a different input stream abstraction (to make accidental uncontrolled lookahead and lookbehind, which were easy ways to break incremental parsing, harder).

Drops support for `Stack.startOf` (which is no longer useful without lookbehind).

Context trackers also get passed different arguments, following the changed input stream format.

This package no longer re-exports bindings from `@lezer/common`.

### Bug fixes

Slightly prefer deleting token to inserting them to avoid far-fetched parses.

Fix a problem where node reuse didn't take the amount of look-ahead done by the tokenizer into account, and could reuse nodes whose content would tokenize differently due to changes after them.

Track tokenizer lookahead to fix a problem where some incremental parses would produce incorrect results.

### New features

LR parsers now support a `wrapper` configuration option that allows you to inject additional logic by wrapping the `PartialParse` object it returns.

## 0.13.5 (2021-05-14)

### Bug fixes

Fix a bug with overeager reuse of nodes on change boundaries.

## 0.13.4 (2021-03-03)

### New features

`Parser` instances now have a `topNode` property that holds the type of the parser's top node.

## 0.13.3 (2021-02-17)

### New features

Context trackers can now disable strictness in node reuse.

## 0.13.2 (2021-02-17)

### New features

Add support for context trackers.

## 0.13.1 (2020-12-04)

### Bug fixes

Fix versions of lezer packages depended on.

## 0.13.0 (2020-12-04)

### Breaking changes

`Parser.group` is now called `Parser.nodeSet`.

Nested parsers now work differently. They don't have to be Lezer parsers, but have to produce object conforoming to the `PartialParse` interface. The interface with which non-trivial nested parsers are specified also changed—see the `NestedParser` type.

Parser objects no longer have a `topType` property (scan their node set for types with `isTop` set instead).

`Parser` objects no longer have `withProps`, `withNested`, and `withTokenizer` methods (use `configure` instead).

Both `Parser.parse` and `Parser.startParse` now take an optional start position as second parameter and an optional parse context as third. `startParse` returns an instance of the `PartialParse` interface instead of the old `ParseContext` class (whose name is now used for something else). Parse _options_ are no longer passed to these methods, but configured in advance through `Parser.configure`.

During incremental parsing, instead of passing a tree as the `cache` option, reusable subtrees (see `TreeFragment` from lezer-tree) are now retrieved from the `fragments` property of the parse context object, if provided.

`Parser.parse` and `Parser.startParse` no longer take an options parameter. Instead, `bufferLength` and `strict` can be configured with `Parser.configure` now, and the start position and context are passed as optional arguments.

The `InputStream` type has been replaced by `Input` from the lezer-tree package (which has the same interface but a more appropriate name).

### New features

The `Parser` class now has a `configure` method that is used to create a parser instance with a more specific configuration.

## 0.12.1 (2020-11-19)

### Bug fixes

Fix an infinite loop in incremental parsing when repeatedly reusing a zero-length cached node.

## 0.12.0 (2020-10-23)

### Breaking changes

Follow the change from `Subtree` to `TreeCursor` in lezer-tree.

The serialized parser format changed.

`Stack.startOf` now returns null, rather than -1, when it can't find the given element.

### New features

`Stack.startOf` now takes an optional second argument that allows you to select a match beyond the innermost one.

## 0.11.2 (2020-09-26)

### Bug fixes

Fix lezer depencency versions

## 0.11.1 (2020-09-26)

### Bug fixes

Fix an infinite loop that was sometimes hit during error recovery.

## 0.11.0 (2020-09-26)

### Breaking changes

Follow a breaking change in the way repeat nodes are represented.

Support the new action table format that allows sharing between states.

## 0.10.4 (2020-09-15)

### New features

Parser objects now have a `withTokenizer` method that can be used to replace external tokenizers.

## 0.10.3 (2020-09-10)

### Bug fixes

Fix a bug that caused the value returned by `ParseContext.badness` to be much higher than intended.

## 0.10.2 (2020-09-02)

### Bug fixes

`Stack.ruleStart` will now ignore repeat rules and node-less rules when determining the inner rule.

Work around a failure mode where error-recovery got stuck in an end-of-grammar state and thus could not continue meaningfully parsing anything by restarting such states back to their initial state.

### New features

External tokenizers can now provide an `extend` flag to allow their tokens to be used alongside tokens produced by other tokenizers.

Add support for dynamic precedences.

## 0.10.1 (2020-08-20)

### Bug fixes

Fixes an issue where repeated error recovery could lead to a tree so deep that recursive functions on it would overflow the stack.

## 0.10.0 (2020-08-07)

### New features

Add support for grammar dialects.

Add support for external specializers.

Stacks now have a `parser` accessor that gets you the active parser instance.

### Breaking changes

No longer list internal properties in the type definitions.

Follow changes in the serialized parser format.

The way different tokenizers are combined is now slightly different. The first one to return a token wins, even if that token has no actions in the current state. The old behavior, where further tokenizers are tried until actions are found, can be enabled for a given tokenizer by setting its `fallback` flag.

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
