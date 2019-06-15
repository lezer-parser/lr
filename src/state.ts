// Actions hold one of:
//  - shift(target: u16)
//  - reduce(term: u16, depth: u1, is_repeat: u10)
// They are encoded as integers for conveniently (and cheaply) passing
// them around. The first 16 bits hold the target or term. Bit 17 is
// set to true for reduce actions. Bit 18 for reduce actions that are
// repeats. And in reduce actions, bits beyond 18 hold the depth.
// Shift actions have all their bits beyond 16 set to zero, so they
// directly hold the target state id.
// When storing actions in 16-bit number arrays, they are split in the
// middle, with the first element holding the first 16 bits, and the
// second the rest.
export const REDUCE_FLAG = 1 << 16, REDUCE_REPEAT_FLAG = 1 << 17, GOTO_FLAG = 1 << 17, STAY_FLAG = 1 << 18
export const REDUCE_DEPTH_SHIFT = 19, ACTION_VALUE_MASK = 2**16 - 1

// Invalid reduce value used in forcedReduce to encode that a state is
// accepting
export const ACCEPTING = REDUCE_REPEAT_FLAG

export class ParseState {
  constructor(readonly id: number,
              // These are offsets into the state data array
              readonly actions: number,
              readonly recover: number,
              readonly skip: number,
              readonly tokenizerMask: number,
              readonly defaultReduce: number,
              readonly forcedReduce: number) {}

  get accepting() { return this.forcedReduce == ACCEPTING }
}
