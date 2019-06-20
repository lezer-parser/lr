import {Stack} from "./stack"

export interface InputStream {
  pos: number
  length: number
  next(): number
  peek(pos?: number): number
  accept(term: number, pos?: number): void
  token: number
  tokenEnd: number
  goto(n: number): InputStream
  read(from: number, to: number): string
  clip(at: number): InputStream
}

export class StringStream implements InputStream {
  pos = 0
  token = -1
  tokenEnd = -1

  constructor(readonly string: string, readonly length = string.length) {}

  next(): number {
    if (this.pos == this.length) return -1
    return this.string.charCodeAt(this.pos++)
  }

  peek(pos = this.pos) {
    return pos < 0 || pos >= this.length ? -1 : this.string.charCodeAt(pos)
  }
  
  accept(term: number, pos = this.pos) {
    this.token = term
    this.tokenEnd = pos
  }

  goto(n: number) {
    this.pos = this.tokenEnd = n
    this.token = -1
    return this
  }

  read(from: number, to: number): string { return this.string.slice(from, Math.min(this.length, to)) }

  clip(at: number) { return new StringStream(this.string, at) }
}

export interface Tokenizer {
  token(input: InputStream, stack: Stack): void
  contextual: boolean
}

export class TokenGroup implements Tokenizer {
  contextual!: boolean

  constructor(readonly data: Readonly<Uint16Array>, readonly id: number) {}

  token(input: InputStream, stack: Stack) { token(this.data, input, stack, this.id) }
}

TokenGroup.prototype.contextual = false

export class ExternalTokenizer {
  contextual: boolean

  constructor(readonly token: (input: InputStream, stack: Stack) => void,
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
// and calling `input.accept` when it matches a token.
function token(data: Readonly<Uint16Array>,
               input: InputStream,
               stack: Stack,
               group: number) {
  let state = 0, groupMask = 1 << group
  scan: for (;;) {
    if ((groupMask & data[state]) == 0) break
    let accEnd = data[state + 1]
    // Check whether this state can lead to a token in the current group
    // Accept tokens in this state, possibly overwriting
    // lower-precedence / shorter tokens
    for (let i = state + 3; i < accEnd; i += 2) if ((data[i + 1] & groupMask) > 0) {
      let term = data[i]
      if (input.token == -1 || input.token == term || stack.cx.parser.overrides(term, input.token)) {
        input.accept(term)
        break
      }
    }
    let next = input.next()
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
