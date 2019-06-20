import {StateFlag} from "./constants"

export class ParseState {
  constructor(readonly id: number,
              readonly flags: number,

              // These are offsets into the state data array
              readonly actions: number,
              readonly recover: number,
              readonly skip: number,

              readonly tokenizerMask: number,
              readonly defaultReduce: number,
              readonly forcedReduce: number) {}

  get accepting() { return (this.flags & StateFlag.Accepting) > 0 }
  get skipped() { return (this.flags & StateFlag.Skipped) > 0 }
  get startNested() { return this.flags & StateFlag.StartNest ? this.flags >> StateFlag.NestShift : -1 }
}
