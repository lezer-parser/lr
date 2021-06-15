import {Input, InputGap} from "lezer-tree"
import {Stack} from "./stack"

export class InputStream {
  chunk = ""
  chunkOff = 0
  chunkPos: number
  next: number = -1
  maxPos: number
  gaps!: null | readonly InputGap[]

  constructor(readonly input: Input, public pos: number, public end: number, gaps: undefined | readonly InputGap[]) {
    this.maxPos = this.chunkPos = pos
    if (gaps && gaps.length) this.gaps = gaps
    this.readNext()
  }

  getChunk() {
    if (this.pos >= this.end) {
      this.next = -1
      this.chunk = ""
      this.chunkOff = 0
      return false
    }
    let nextChunk = this.input.chunk(this.pos)
    let end = this.pos + nextChunk.length
    this.chunk = end > this.end ? nextChunk.slice(0, this.end - this.pos) : nextChunk
    this.chunkPos = this.pos
    this.chunkOff = 0
    return this.gaps ? this.removeGapsFromChunk() : true
  }

  private removeGapsFromChunk(): boolean {
    let from = this.pos, to = this.pos + this.chunk.length
    for (let g of this.gaps!) {
      if (g.from >= to) break
      if (g.to > from) {
        if (from < g.from) {
          this.chunk = this.chunk.slice(0, g.from - from)
          return true
        } else {
          this.pos = this.chunkPos = g.to
          if (to > g.to) {
            this.chunk = this.chunk.slice(g.to - from)
            from = g.to
          } else {
            this.chunk = ""
            return this.getChunk()
          }
        }
      }
    }
    return true
  }

  readNext() {
    if (this.chunkOff == this.chunk.length)
      if (!this.getChunk()) return
    this.next = this.chunk.charCodeAt(this.chunkOff)
  }

  advance() {
    if (this.next < 0) return false
    this.chunkOff++
    this.pos++
    if (this.pos > this.maxPos) this.maxPos = this.pos
    this.readNext()
    return true
  }

  reset(pos: number) {
    if (this.pos == pos) return this
    // FIXME keep a prev chunk to avoid have to re-query the input every time at the end of a chunk
    this.pos = this.maxPos = pos
    if (pos >= this.chunkPos && pos < this.chunkPos + this.chunk.length) {
      this.chunkOff = pos - this.chunkPos
    } else {
      this.chunk = ""
      this.chunkOff = 0
    }
    this.readNext()
    return this
  }

  read(from: number, to: number) {
    let val = from >= this.chunkPos && to <= this.chunkPos + this.chunk.length
      ? this.chunk.slice(from - this.chunkPos, to - this.chunkPos)
      : this.input.read(from, to)
    if (this.gaps) {
      for (let i = this.gaps.length - 1; i >= 0; i--) {
        let g = this.gaps[i]
        if (g.to > from && g.from < to)
          val = val.slice(0, Math.max(0, g.from - from)) + val.slice(Math.min(val.length, g.to - from))
      }
    }
    return val
  }
}

InputStream.prototype.gaps = null

/// Tokenizers write the tokens they read into instances of this class.
export class Token {
  /// The start of the token. This is set by the parser, and should not
  /// be mutated by the tokenizer.
  start = -1
  /// This starts at -1, and should be updated to a term id when a
  /// matching token is found.
  value = -1
  /// When setting `.value`, you should also set `.end` to the end
  /// position of the token. (You'll usually want to use the `accept`
  /// method.)
  end = -1

  /// Accept a token, setting `value` and `end` to the given values.
  accept(value: number, end: number) {
    this.value = value
    this.end = end
  }
}

export interface Tokenizer {
  token(input: InputStream, token: Token, stack: Stack): void
  contextual: boolean
  fallback: boolean
  extend: boolean
}

/// @internal
export class TokenGroup implements Tokenizer {
  contextual!: boolean
  fallback!: boolean
  extend!: boolean

  constructor(readonly data: Readonly<Uint16Array>, readonly id: number) {}

  token(input: InputStream, token: Token, stack: Stack) { readToken(this.data, input, token, stack, this.id) }
}

TokenGroup.prototype.contextual = TokenGroup.prototype.fallback = TokenGroup.prototype.extend = false

interface ExternalOptions {
  /// When set to true, mark this tokenizer as depending on the
  /// current parse stack, which prevents its result from being cached
  /// between parser actions at the same positions.
  contextual?: boolean,
  /// By defaults, when a tokenizer returns a token, that prevents
  /// tokenizers with lower precedence from even running. When
  /// `fallback` is true, the tokenizer is allowed to run when a
  /// previous tokenizer returned a token that didn't match any of the
  /// current state's actions.
  fallback?: boolean
  /// When set to true, tokenizing will not stop after this tokenizer
  /// has produced a token. (But it will still fail to reach this one
  /// if a higher-precedence tokenizer produced a token.)
  extend?: boolean
}

/// Exports that are used for `@external tokens` in the grammar should
/// export an instance of this class.
export class ExternalTokenizer implements Tokenizer {
  contextual: boolean
  fallback: boolean
  extend: boolean

  /// Create a tokenizer. The first argument is the function that,
  /// given an input stream and a token object,
  /// [fills](#lezer.Token.accept) the token object if it recognizes a
  /// token. `token.start` should be used as the start position to
  /// scan from.
  constructor(
    readonly token: (input: InputStream, token: Token, stack: Stack) => void,
    options: ExternalOptions = {}
  ) {
    this.contextual = !!options.contextual
    this.fallback = !!options.fallback
    this.extend = !!options.extend
  }
}

// Tokenizer data is stored a big uint16 array containing, for each
// state:
//
//  - A group bitmask, indicating what token groups are reachable from
//    this state, so that paths that can only lead to tokens not in
//    any of the current groups can be cut off early.
//
//  - The position of the end of the state's sequence of accepting
//    tokens
//
//  - The number of outgoing edges for the state
//
//  - The accepting tokens, as (token id, group mask) pairs
//
//  - The outgoing edges, as (start character, end character, state
//    index) triples, with end character being exclusive
//
// This function interprets that data, running through a stream as
// long as new states with the a matching group mask can be reached,
// and updating `token` when it matches a token.
function readToken(data: Readonly<Uint16Array>,
                   input: InputStream,
                   token: Token,
                   stack: Stack,
                   group: number) {
  let state = 0, groupMask = 1 << group, dialect = stack.p.parser.dialect
  scan: for (;;) {
    if ((groupMask & data[state]) == 0) break
    let accEnd = data[state + 1]
    // Check whether this state can lead to a token in the current group
    // Accept tokens in this state, possibly overwriting
    // lower-precedence / shorter tokens
    for (let i = state + 3; i < accEnd; i += 2) if ((data[i + 1] & groupMask) > 0) {
      let term = data[i]
      if (dialect.allows(term) &&
          (token.value == -1 || token.value == term || stack.p.parser.overrides(term, token.value))) {
        token.accept(term, input.pos)
        break
      }
    }
    // Do a binary search on the state's edges
    for (let next = input.next, low = 0, high = data[state + 2]; low < high;) {
      let mid = (low + high) >> 1
      let index = accEnd + mid + (mid << 1)
      let from = data[index], to = data[index + 1]
      if (next < from) high = mid
      else if (next >= to) low = mid + 1
      else { state = data[index + 2]; input.advance(); continue scan }
    }
    break
  }
}
