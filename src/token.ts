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
}

export class StringStream implements InputStream {
  pos = 0
  token = -1
  tokenEnd = -1

  constructor(readonly string: string) {}

  get length() { return this.string.length }

  next(): number {
    if (this.pos == this.string.length) return -1
    return this.string.charCodeAt(this.pos++)
  }

  peek(pos = this.pos) {
    return pos < 0 || pos >= this.string.length ? -1 : this.string.charCodeAt(pos)
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

  read(from: number, to: number): string { return this.string.slice(from, to) }
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
      if (input.token == -1 || input.token == term || mayOverride(stack.parser.tokenPrec, term, input.token)) {
        input.accept(term)
        break
      }
    }
    let next = input.next(), end = data[state + 2]
    for (let i = accEnd; i < end; i += 3) { // FIXME binary search, multiple table forms
      let from = data[i], to = data[i + 1]
      if (next >= from && next < to) { state = data[i + 2]; continue scan }
    }
    break
  }
}

function mayOverride(precedences: number[], token: number, prev: number) {
  let iPrev = precedences.indexOf(prev)
  return iPrev < 0 || precedences.indexOf(token) < iPrev
}
