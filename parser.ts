// Shifts are encoded as negative state IDs, reduces as bitmasks with
// the first 20 bits holding the term that's being reduced, and the
// bits after that the number of values consumed.

import {parse} from "./parse"
import {SyntaxTree} from "./tree"

export const REDUCE_DEPTH_SIZE = 6, REDUCE_DEPTH_MASK = 2**REDUCE_DEPTH_SIZE - 1

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

export class ParseState {
  constructor(readonly id: number,
              readonly actions: ReadonlyArray<number>,
              readonly goto: ReadonlyArray<number>,
              readonly recover: ReadonlyArray<number>,
              readonly alwaysReduce: number,
              readonly defaultReduce: number,
              readonly skip: Tokenizer,
              readonly tokenizers: ReadonlyArray<Tokenizer>) {}

  hasAction(terminal: number) { return lookup(this.actions, terminal) != 0 }

  anyReduce() {
    if (this.alwaysReduce >= 0) return this.alwaysReduce
    for (let i = 0; i < this.actions.length; i += 2) {
      let action = this.actions[i + 1]
      if (action > 0) return action
    }
    return 0
  }
  // Zero means no entry found, otherwise it'll be a state id (never
  // zero because no goto edges to the start state exist)
  getGoto(term: number) { return lookup(this.goto, term) }
  getRecover(terminal: number) { return lookup(this.recover, terminal) }
}

function lookup(actions: ReadonlyArray<number>, term: number) {
  for (let i = 0; i < actions.length; i+= 2) if (actions[i] == term) return actions[i + 1]
  return 0
}

// Terms can be tagged (in which case they need to be included in the
// syntax tree), repeated, in which case they need to balance their
// content on reduce, or anonymous
export const FIRST_REPEAT_TERM = 2**16, MAX_TAGGED_TERM = FIRST_REPEAT_TERM - 1
export const FIRST_ANON_TERM = 2 * FIRST_REPEAT_TERM, MAX_REPEAT_TERM = FIRST_ANON_TERM - 1

export const TERM_ERR = 0, TERM_EOF = MAX_REPEAT_TERM + 1

export type ParseOptions = {cache?: SyntaxTree | null, strict?: boolean, bufferLength?: number}

export class Parser {
  readonly specializations: ReadonlyArray<{[value: string]: number}>

  constructor(readonly states: ReadonlyArray<ParseState>,
              readonly tags: ReadonlyArray<string>,
              readonly repeats: ReadonlyArray<number>,
              readonly specialized: ReadonlyArray<number>,
              specializations: ReadonlyArray<{[value: string]: number}>,
              readonly termNames: null | {[id: number]: string} = null) {
    this.specializations = specializations.map(withoutPrototype)
  }

  getTag(term: number): string | null {
    return term >= MAX_TAGGED_TERM ? null : this.tags[term]
  }

  getName(term: number): string {
    return this.termNames ? this.termNames[term] : this.getTag(term) || String(term)
  }

  parse(input: InputStream, options?: ParseOptions) {
    return parse(input, this, options)
  }
}

function withoutPrototype(obj: {}) {
  if (!(obj instanceof Object)) return obj
  let result: {[key: string]: any} = Object.create(null)
  for (let prop in obj) if (Object.prototype.hasOwnProperty.call(obj, prop)) result[prop] = (obj as any)[prop]
  return result
}

const none: ReadonlyArray<any> = []

export function s(actions: number | ReadonlyArray<number>,
                  goto: ReadonlyArray<number>,
                  defaultReduce: number,
                  skip: Tokenizer,
                  tokenizers: ReadonlyArray<Tokenizer>,
                  recover?: ReadonlyArray<number>): ParseState {
  return new ParseState(s.id++, typeof actions == "number" ? none : actions,
                        goto, recover || none, typeof actions == "number" ? actions : -1,
                        defaultReduce, skip, tokenizers)
}

s.id = 0
