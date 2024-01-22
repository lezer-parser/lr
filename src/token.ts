import {Input} from "@lezer/common"
import {Stack} from "./stack"
import {Seq} from "./constants"
import {decodeArray} from "./decode"

export class CachedToken {
  start = -1
  value = -1
  end = -1
  extended = -1
  lookAhead = 0
  mask = 0
  context = 0
}

const nullToken = new CachedToken

/// [Tokenizers](#lr.ExternalTokenizer) interact with the input
/// through this interface. It presents the input as a stream of
/// characters, tracking lookahead and hiding the complexity of
/// [ranges](#common.Parser.parse^ranges) from tokenizer code.
export class InputStream {
  /// @internal
  chunk = ""
  /// @internal
  chunkOff = 0
  /// @internal
  chunkPos: number
  /// Backup chunk
  private chunk2 = ""
  private chunk2Pos = 0

  /// The character code of the next code unit in the input, or -1
  /// when the stream is at the end of the input.
  next: number = -1

  /// @internal
  token = nullToken

  /// The current position of the stream. Note that, due to parses
  /// being able to cover non-contiguous
  /// [ranges](#common.Parser.startParse), advancing the stream does
  /// not always mean its position moves a single unit.
  pos: number

  /// @internal
  end: number

  private rangeIndex = 0
  private range: {from: number, to: number}

  /// @internal
  constructor(
    /// @internal
    readonly input: Input,
    /// @internal
    readonly ranges: readonly {from: number, to: number}[]
  ) {
    this.pos = this.chunkPos = ranges[0].from
    this.range = ranges[0]
    this.end = ranges[ranges.length - 1].to
    this.readNext()
  }

  /// @internal
  resolveOffset(offset: number, assoc: -1 | 1) {
    let range = this.range, index = this.rangeIndex
    let pos = this.pos + offset
    while (pos < range.from) {
      if (!index) return null
      let next = this.ranges[--index]
      pos -= range.from - next.to
      range = next
    }
    while (assoc < 0 ? pos > range.to : pos >= range.to) {
      if (index == this.ranges.length - 1) return null
      let next = this.ranges[++index]
      pos += next.from - range.to
      range = next
    }
    return pos
  }

  /// @internal
  clipPos(pos: number) {
    if (pos >= this.range.from && pos < this.range.to) return pos
    for (let range of this.ranges) if (range.to > pos) return Math.max(pos, range.from)
    return this.end
  }

  /// Look at a code unit near the stream position. `.peek(0)` equals
  /// `.next`, `.peek(-1)` gives you the previous character, and so
  /// on.
  ///
  /// Note that looking around during tokenizing creates dependencies
  /// on potentially far-away content, which may reduce the
  /// effectiveness incremental parsing—when looking forward—or even
  /// cause invalid reparses when looking backward more than 25 code
  /// units, since the library does not track lookbehind.
  peek(offset: number) {
    let idx = this.chunkOff + offset, pos, result
    if (idx >= 0 && idx < this.chunk.length) {
      pos = this.pos + offset
      result = this.chunk.charCodeAt(idx)
    } else {
      let resolved = this.resolveOffset(offset, 1)
      if (resolved == null) return -1
      pos = resolved
      if (pos >= this.chunk2Pos && pos < this.chunk2Pos + this.chunk2.length) {
        result = this.chunk2.charCodeAt(pos - this.chunk2Pos)
      } else {
        let i = this.rangeIndex, range = this.range
        while (range.to <= pos) range = this.ranges[++i]
        this.chunk2 = this.input.chunk(this.chunk2Pos = pos)
        if (pos + this.chunk2.length > range.to) this.chunk2 = this.chunk2.slice(0, range.to - pos)
        result = this.chunk2.charCodeAt(0)
      }
    }
    if (pos >= this.token.lookAhead) this.token.lookAhead = pos + 1
    return result
  }

  /// Accept a token. By default, the end of the token is set to the
  /// current stream position, but you can pass an offset (relative to
  /// the stream position) to change that.
  acceptToken(token: number, endOffset = 0) {
    let end = endOffset ? this.resolveOffset(endOffset, -1) : this.pos
    if (end == null || end < this.token.start) throw new RangeError("Token end out of bounds")
    this.token.value = token
    this.token.end = end
  }

  /// Accept a token ending at a specific given position.
  acceptTokenTo(token: number, endPos: number) {
    this.token.value = token
    this.token.end = endPos
  }

  private getChunk() {
    if (this.pos >= this.chunk2Pos && this.pos < this.chunk2Pos + this.chunk2.length) {
      let {chunk, chunkPos} = this
      this.chunk = this.chunk2; this.chunkPos = this.chunk2Pos
      this.chunk2 = chunk; this.chunk2Pos = chunkPos
      this.chunkOff = this.pos - this.chunkPos
    } else {
      this.chunk2 = this.chunk; this.chunk2Pos = this.chunkPos
      let nextChunk = this.input.chunk(this.pos)
      let end = this.pos + nextChunk.length
      this.chunk = end > this.range.to ? nextChunk.slice(0, this.range.to - this.pos) : nextChunk
      this.chunkPos = this.pos
      this.chunkOff = 0
    }
  }

  private readNext() {
    if (this.chunkOff >= this.chunk.length) {
      this.getChunk()
      if (this.chunkOff == this.chunk.length) return this.next = -1
    }
    return this.next = this.chunk.charCodeAt(this.chunkOff)
  }

  /// Move the stream forward N (defaults to 1) code units. Returns
  /// the new value of [`next`](#lr.InputStream.next).
  advance(n = 1) {
    this.chunkOff += n
    while (this.pos + n >= this.range.to) {
      if (this.rangeIndex == this.ranges.length - 1) return this.setDone()
      n -= this.range.to - this.pos
      this.range = this.ranges[++this.rangeIndex]
      this.pos = this.range.from
    }
    this.pos += n
    if (this.pos >= this.token.lookAhead) this.token.lookAhead = this.pos + 1
    return this.readNext()
  }

  private setDone() {
    this.pos = this.chunkPos = this.end
    this.range = this.ranges[this.rangeIndex = this.ranges.length - 1]
    this.chunk = ""
    return this.next = -1
  }

  /// @internal
  reset(pos: number, token?: CachedToken) {
    if (token) {
      this.token = token
      token.start = pos
      token.lookAhead = pos + 1
      token.value = token.extended = -1
    } else {
      this.token = nullToken
    }
    if (this.pos != pos) {
      this.pos = pos
      if (pos == this.end) {
        this.setDone()
        return this
      }
      while (pos < this.range.from) this.range = this.ranges[--this.rangeIndex]
      while (pos >= this.range.to) this.range = this.ranges[++this.rangeIndex]
      if (pos >= this.chunkPos && pos < this.chunkPos + this.chunk.length) {
        this.chunkOff = pos - this.chunkPos
      } else {
        this.chunk = ""
        this.chunkOff = 0
      }
      this.readNext()
    }
    return this
  }

  /// @internal
  read(from: number, to: number) {
    if (from >= this.chunkPos && to <= this.chunkPos + this.chunk.length)
      return this.chunk.slice(from - this.chunkPos, to - this.chunkPos)
    if (from >= this.chunk2Pos && to <= this.chunk2Pos + this.chunk2.length)
      return this.chunk2.slice(from - this.chunk2Pos, to - this.chunk2Pos)
    if (from >= this.range.from && to <= this.range.to)
      return this.input.read(from, to)
    let result = ""
    for (let r of this.ranges) {
      if (r.from >= to) break
      if (r.to > from) result += this.input.read(Math.max(r.from, from), Math.min(r.to, to))
    }
    return result
  }
}

export interface Tokenizer {
  /// @internal
  token(input: InputStream, stack: Stack): void
  /// @internal
  contextual: boolean
  /// @internal
  fallback: boolean
  /// @internal
  extend: boolean
}

/// @internal
export class TokenGroup implements Tokenizer {
  contextual!: boolean
  fallback!: boolean
  extend!: boolean

  constructor(readonly data: Readonly<Uint16Array>, readonly id: number) {}

  token(input: InputStream, stack: Stack) {
    let {parser} = stack.p
    readToken(this.data, input, stack, this.id, parser.data, parser.tokenPrecTable)
  }
}

TokenGroup.prototype.contextual = TokenGroup.prototype.fallback = TokenGroup.prototype.extend = false

/// @hide
export class LocalTokenGroup implements Tokenizer {
  contextual!: boolean
  fallback!: boolean
  extend!: boolean
  readonly data: Readonly<Uint16Array>

  constructor(data: Readonly<Uint16Array> | string, readonly precTable: number, readonly elseToken?: number) {
    this.data = typeof data == "string" ? decodeArray<Readonly<Uint16Array>>(data) : data
  }

  token(input: InputStream, stack: Stack) {
    let start = input.pos, skipped = 0
    for (;;) {
      let atEof = input.next < 0, nextPos = input.resolveOffset(1, 1)
      readToken(this.data, input, stack, 0, this.data, this.precTable)
      if (input.token.value > -1) break
      if (this.elseToken == null) return
      if (!atEof) skipped++
      if (nextPos == null) break
      input.reset(nextPos, input.token)
    }
    if (skipped) {
      input.reset(start, input.token)
      input.acceptToken(this.elseToken!, skipped)
    }
  }
}

LocalTokenGroup.prototype.contextual = TokenGroup.prototype.fallback = TokenGroup.prototype.extend = false

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

/// `@external tokens` declarations in the grammar should resolve to
/// an instance of this class.
export class ExternalTokenizer {
  /// @internal
  contextual: boolean
  /// @internal
  fallback: boolean
  /// @internal
  extend: boolean

  /// Create a tokenizer. The first argument is the function that,
  /// given an input stream, scans for the types of tokens it
  /// recognizes at the stream's position, and calls
  /// [`acceptToken`](#lr.InputStream.acceptToken) when it finds
  /// one.
  constructor(
    /// @internal
    readonly token: (input: InputStream, stack: Stack) => void,
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
// and updating `input.token` when it matches a token.
function readToken(data: Readonly<Uint16Array>,
                   input: InputStream,
                   stack: Stack,
                   group: number,
                   precTable: Readonly<Uint16Array>,
                   precOffset: number) {
  let state = 0, groupMask = 1 << group, {dialect} = stack.p.parser
  scan: for (;;) {
    if ((groupMask & data[state]) == 0) break
    let accEnd = data[state + 1]
    // Check whether this state can lead to a token in the current group
    // Accept tokens in this state, possibly overwriting
    // lower-precedence / shorter tokens
    for (let i = state + 3; i < accEnd; i += 2) if ((data[i + 1] & groupMask) > 0) {
      let term = data[i]
      if (dialect.allows(term) &&
          (input.token.value == -1 || input.token.value == term ||
           overrides(term, input.token.value, precTable, precOffset))) {
        input.acceptToken(term)
        break
      }
    }
    let next = input.next, low = 0, high = data[state + 2]
    // Special case for EOF
    if (input.next < 0 && high > low && data[accEnd + high * 3 - 3] == Seq.End) {
      state = data[accEnd + high * 3 - 1]
      continue scan
    }
    // Do a binary search on the state's edges
    for (; low < high;) {
      let mid = (low + high) >> 1
      let index = accEnd + mid + (mid << 1)
      let from = data[index], to = data[index + 1] || 0x10000
      if (next < from) high = mid
      else if (next >= to) low = mid + 1
      else { state = data[index + 2]; input.advance(); continue scan }
    }
    break
  }
}

function findOffset(data: Readonly<Uint16Array>, start: number, term: number) {
  for (let i = start, next; (next = data[i]) != Seq.End; i++)
    if (next == term) return i - start
  return -1
}

function overrides(token: number, prev: number, tableData: Readonly<Uint16Array>, tableOffset: number) {
  let iPrev = findOffset(tableData, tableOffset, prev)
  return iPrev < 0 || findOffset(tableData, tableOffset, token) < iPrev
}
