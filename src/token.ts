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
  prec = 2

  constructor(readonly data: readonly (readonly number[])[], options?: {contextual?: boolean}) {
    this.contextual = !!(options && options.contextual)
  }

  withPrec(prec: number) {
    this.prec = prec
    return this
  }

  token(input: InputStream, stack: Stack) {
    let state = 0
    scan: for (;;) {
      let array = this.data[state], acc = array[0]
      if (acc > 0) input.accept(array[1]) // FIXME precedence
      let next = input.next()
      for (let i = 1 + acc; i < array.length; i += 3) { // FIXME binary search, multiple table forms
        let from = array[i], to = array[i + 1]
        if (next >= from && next < to) { state = array[i + 2]; continue scan }
      }
      break
    }
  }
}

export const noToken = new Tokenizer([[0]])
