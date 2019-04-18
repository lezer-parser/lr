export interface InputStream {
  pos: number
  length: number
  next(): number
  adv(): void
  goto(n: number): InputStream
  read(from: number, to: number): string
}

export class StringStream implements InputStream {
  pos = 0

  constructor(readonly string: string) {}

  get length() { return this.string.length }

  next(): number {
    if (this.pos == this.string.length) return -1
    return this.string.charCodeAt(this.pos)
  }
  
  adv() { this.pos++ }

  goto(n: number) { this.pos = n; return this }

  read(from: number, to: number): string { return this.string.slice(from, to) }
}

export type Tokenizer = (input: InputStream) => number

export function noToken(input: InputStream) { return -1 }
