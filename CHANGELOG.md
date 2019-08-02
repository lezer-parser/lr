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
