// Shifts are encoded as negative state IDs, reduces as bitmasks with
// the first 20 bits holding the term that's being reduced, and the
// bits after that the number of values consumed.

export const REDUCE_NAME_SIZE = 20, REDUCE_NAME_MASK = 2**REDUCE_NAME_SIZE - 1

export const none: ReadonlyArray<any> = []

export class Token {
  public start = 0
  public end = 0
  public term = -1
  public specialized = -1
}

export interface InputStream {
  pos: number
  next(): number
  adv(ch: number): void
  goto(n: number): void
  read(from: number, to: number): string
}

export type Tokenizer = (input: InputStream) => number

export function noToken(input: InputStream) { return -1 }

export class ParseState {
  constructor(readonly actions: ReadonlyArray<number>,
              readonly goto: ReadonlyArray<number>,
              readonly recover: ReadonlyArray<number>,
              readonly alwaysReduce: number,
              readonly defaultReduce: number,
              readonly skip: Tokenizer,
              readonly tokenizers: ReadonlyArray<Tokenizer>) {}
}

// Terms can be tagged (in which case they need to be included in the
// syntax tree), repeated, in which case they need to balance their
// content on reduce, or anonymous
export const FIRST_REPEAT_TERM = 2**16, MAX_TAGGED_TERM = FIRST_REPEAT_TERM - 1
export const FIRST_ANON_TERM = 2 * FIRST_REPEAT_TERM, MAX_REPEAT_TERM = FIRST_ANON_TERM - 1

export const TERM_ERR = 0, TERM_EOF = MAX_REPEAT_TERM + 1

export class Parser {
  constructor(readonly states: ReadonlyArray<ParseState>,
              readonly tags: ReadonlyArray<string>,
              readonly repeats: ReadonlyArray<number>,
              readonly specialized: ReadonlyArray<number>,
              readonly specializations: ReadonlyArray<{[value: string]: number}>) {}

  getTag(term: number): string | null {
    return term >= MAX_TAGGED_TERM ? null : this.tags[term]
  }
}
