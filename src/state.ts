import {Tokenizer} from "./token"

// Shifts are encoded as negative state IDs, reduces as bitmasks with
// the first 6 bits holding the depth of the reduce, and the bits
// after that the term that's being reduced.
export const REDUCE_DEPTH_SIZE = 6, REDUCE_DEPTH_MASK = 2**REDUCE_DEPTH_SIZE - 1

export const GOTO_STAY = -0xffff

export class ParseState {
  constructor(readonly id: number,
              readonly actions: readonly number[],
              readonly recover: readonly number[],
              readonly defaultReduce: number,
              readonly forcedReduce: number,
              readonly tokenizers: readonly Tokenizer[],
              readonly skip: readonly number[]) {}

  hasAction(terminal: number) {
    return lookup(this.actions, terminal) != 0 ||
      lookup(this.skip, terminal) != 0
  }

  anyReduce() {
    if (this.defaultReduce > 0) return this.defaultReduce
    for (let i = 0; i < this.actions.length; i += 2) {
      let action = this.actions[i + 1]
      if (action > 0) return action
    }
    return 0
  }

  getRecover(terminal: number) { return lookup(this.recover, terminal) }
}

function lookup(actions: readonly number[], term: number) {
  for (let i = 0; i < actions.length; i+= 2) if (actions[i] == term) return actions[i + 1]
  return 0
}
