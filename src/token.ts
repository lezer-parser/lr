import {Term} from "./constants"
import {Stack} from "./stack"

// Tokenizers write the tokens they read into instances of this class.
export class Token {
  // The start of the token. This is set by the parser, and should not
  // be mutated by the tokenizer.
  start = -1
  // This starts at -1, and should be updated to a term id when a
  // matching token is found.
  value = -1
  // When setting `.value`, you should also set `.end` to the end
  // position of the token. (You'll usually want to use the `accept`
  // method.)
  end = -1

  // Accept a token, setting `value` and `end` to the given values.
  accept(value: number, end: number) {
    this.value = value
    this.end = end
  }

  // @internal
  asError(start: number, eof: number) {
    this.start = start
    if (start == eof) {
      this.value = Term.Eof
      this.end = start
    } else {
      this.value = Term.Err
      this.end = start + 1
    }
    return this
  }
}

// This is the interface the parser uses to access the document. It
// exposes a sequence of UTF16 code points. Most access will be
// sequential, so implementations can optimize for that.
export interface InputStream {
  // The end of the stream.
  length: number
  // Get the code point at the given position. Will return -1 when
  // asked for a point below 0 or beyond the end of the stream
  get(pos: number): number
  // Read part of the stream as a string
  read(from: number, to: number): string
  // Return a new `InputStream` over the same data, but with a lower
  // `length`. Used, for example, when nesting grammars to give the
  // inner grammar a narrower view of the input.
  clip(at: number): InputStream
}

// An `InputStream` that is backed by a single, flat string.
export class StringStream implements InputStream {
  constructor(readonly string: string, readonly length = string.length) {}

  get(pos: number) {
    return pos < 0 || pos >= this.length ? -1 : this.string.charCodeAt(pos)
  }
  
  read(from: number, to: number): string { return this.string.slice(from, Math.min(this.length, to)) }

  clip(at: number) { return new StringStream(this.string, at) }
}

export interface Tokenizer {
  token(input: InputStream, token: Token, stack: Stack): void
  contextual: boolean
}

export class TokenGroup implements Tokenizer {
  contextual!: boolean

  constructor(readonly data: Readonly<Uint16Array>, readonly id: number) {}

  token(input: InputStream, token: Token, stack: Stack) { readToken(this.data, input, token, stack, this.id) }
}

TokenGroup.prototype.contextual = false

export class ExternalTokenizer {
  contextual: boolean

  constructor(readonly token: (input: InputStream, token: Token, stack: Stack) => void,
              options: {contextual?: boolean} = {}) {
    this.contextual = options && options.contextual || false
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
  let state = 0, groupMask = 1 << group
  scan: for (let pos = token.start;;) {
    if ((groupMask & data[state]) == 0) break
    let accEnd = data[state + 1]
    // Check whether this state can lead to a token in the current group
    // Accept tokens in this state, possibly overwriting
    // lower-precedence / shorter tokens
    for (let i = state + 3; i < accEnd; i += 2) if ((data[i + 1] & groupMask) > 0) {
      let term = data[i]
      if (token.value == -1 || token.value == term || stack.cx.parser.overrides(term, token.value)) {
        token.accept(term, pos)
        break
      }
    }
    let next = input.get(pos++)
    // Do a binary search on the state's edges
    for (let low = 0, high = data[state + 2]; low < high;) {
      let mid = (low + high) >> 1
      let index = accEnd + mid + (mid << 1)
      let from = data[index], to = data[index + 1]
      if (next < from) high = mid
      else if (next >= to) low = mid + 1
      else { state = data[index + 2]; continue scan }
    }
    break
  }
}
