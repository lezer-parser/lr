import {Tokenizer} from "./token"

// Shifts are encoded as negative state IDs, reduces as bitmasks with
// the first 6 bits holding the depth of the reduce, and the bits
// after that the term that's being reduced.
export const REDUCE_DEPTH_SIZE = 6, REDUCE_DEPTH_MASK = 2**REDUCE_DEPTH_SIZE - 1

export class ParseState {
  constructor(readonly id: number,
              readonly actions: ReadonlyArray<number>,
              readonly goto: ReadonlyArray<number>,
              readonly recover: ReadonlyArray<number>,
              readonly defaultReduce: number,
              readonly forcedReduce: number,
              readonly skip: Tokenizer,
              readonly tokenizers: ReadonlyArray<Tokenizer>) {}

  hasAction(terminal: number) {
    return lookup(this.actions, terminal) != 0
  }

  anyReduce() {
    if (this.defaultReduce > 0) return this.defaultReduce
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

const none: ReadonlyArray<any> = []

export function s(actions: number | number[],
                  goto: readonly number[],
                  forcedReduce: number,
                  skip: Tokenizer,
                  tokenizers: readonly Tokenizer[],
                  recover: readonly number[] = none): ParseState {
  return new ParseState(s.id++, typeof actions == "number" ? none : actions, goto, recover,
                        typeof actions == "number" ? actions : 0, forcedReduce, skip, tokenizers)
}

s.id = 0

function lookup(actions: ReadonlyArray<number>, term: number) {
  for (let i = 0; i < actions.length; i+= 2) if (actions[i] == term) return actions[i + 1]
  return 0
}
