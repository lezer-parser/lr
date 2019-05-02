import {Stack} from "./stack"

export interface InputStream {
  pos: number
  length: number
  next(): number
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

export class Tokenizer {
  contextual: boolean
  constructor(readonly token: (input: InputStream, stack: Stack) => number, options?: {contextual?: boolean}) {
    this.contextual = !!(options && options.contextual)
  }
}

export const noToken = new Tokenizer(() => -1)
