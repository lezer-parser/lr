// Shifts are encoded as negative state IDs, reduces as bitmasks with
// the first 6 bits holding the depth of the reduce, and the bits
// after that the term that's being reduced.
export const REDUCE_DEPTH_SIZE = 6, REDUCE_DEPTH_MASK = 2**REDUCE_DEPTH_SIZE - 1

export const ACTION_SKIP = -0xffff

export class ParseState {
  constructor(readonly id: number,
              // These are offsets into the state data array
              readonly actions: number,
              readonly recover: number,
              readonly skip: number,
              readonly tokenizerMask: number,
              readonly defaultReduce: number,
              readonly forcedReduce: number) {}

  // States are stored as:
  //
  //  - offset to action table
  //  - offset to recover table
  //  - offset to skip table
  //  - offset to tokenizer table
  //  - default reduce (depth, term)
  //  - forced reduce (depth, term)
  //
  // The run-time library decodes them into objects again when
  // starting up.
  deserialize() {}

  get accepting() { return this.forcedReduce == -1 }
}
