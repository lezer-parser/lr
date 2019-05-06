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

export class TokenGroup {
  constructor(readonly skip: readonly number[]) {}
}

export function token(data: readonly (readonly number[])[],
                      input: InputStream,
                      stack: Stack) {
  let state = 0, group = 1 << stack.state.tokenGroup
  scan: for (;;) {
    let array = data[state], accEnd = (array[1] << 1) + 2
    // Check whether this state can lead to a token in the current group
    if ((group & array[0]) == 0) break
    for (let i = 2; i < accEnd; i += 2)
      if ((array[i + 1] & group) > 0) input.accept(array[i])
    let next = input.next()
    for (let i = accEnd; i < array.length; i += 3) { // FIXME binary search, multiple table forms
      let from = array[i], to = array[i + 1]
      if (next >= from && next < to) { state = array[i + 2]; continue scan }
    }
    break
  }
}
