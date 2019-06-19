import {ParseState, REDUCE_FLAG, REDUCE_REPEAT_FLAG, REDUCE_DEPTH_SHIFT, ACTION_VALUE_MASK, STAY_FLAG, GOTO_FLAG} from "./state"
import {TERM_TAGGED, TERM_ERR} from "./term"
import {Parser} from "./parse"
import {InputStream} from "./token"
import {Tree, REUSED_VALUE, BufferCursor} from "lezer-tree"

const BADNESS_INCREMENT = 100
// Limits in between which stacks are less agressively pruned
export const BADNESS_STABILIZING = 50, BADNESS_WILD = 150

// Badness is a measure of how off-the-rails a given parse is. It is
// bumped when a recovery strategy is applied, and then reduced (by
// multiplication with a constant < 1) for every successful (real)
// token shifted.
//
// Stacks with a low badness are relatively credible parses that have
// shifts matching the input in their recent history. Stacks with a
// high badness are deeply in the weeds and likely wrong. In either of
// these situations, we prune agressively by dropping stacks when
// another stack at the same position is looking better.
//
// For those in the BADNESS_STABILIZING - BADNESS_WILD range, we
// assume that they are in the process of trying to recover and allow
// a bunch of them to continue alongside each other to see which one
// works out better.
//
// Stacks with the same low badness score are likely to be valid GLR
// parsing branches, so in that case it's often a good idea to let
// both continue.
//
// When a stack fails to find an advancing action, recovery is only
// applied when its badness is < BADNESS_WILD, or no better parse
// exists at that point.

export class StackContext {
  reused: Tree[] = []
  constructor(readonly parser: Parser,
              readonly maxBufferLength: number,
              readonly input: InputStream,
              readonly parent: Stack | null = null) {}
}

export class Stack {
  constructor(readonly cx: StackContext,
              // Holds state, pos, value stack pos (15 bits array index, 15 bits buffer index) triplets for all but the top state
              readonly stack: number[],
              public state: ParseState,
              public pos: number,
              public inputPos: number,
              public badness: number,
              // Holds type,start,end,nodeCount quads
              readonly buffer: number[],
              readonly bufferBase: number,
              readonly parent: Stack | null) {}

  toString() {
    return "[" + this.stack.filter((_, i) => i % 3 == 0).concat(this.state.id).join(",") + "]"
  }

  static start(cx: StackContext, pos = 0) {
    return new Stack(cx, [], cx.parser.states[0], pos, pos, 0, [], 0, null)
  }

  pushState(state: ParseState, start: number) {
    this.stack.push(this.state.id, start, this.bufferBase + this.buffer.length)
    this.state = state
  }

  reduce(action: number) { // Encoded reduction action
    let depth = action >> REDUCE_DEPTH_SHIFT, type = action & ACTION_VALUE_MASK
    if (depth == 0) {
      this.pushState(this.cx.parser.states[this.cx.parser.getGoto(this.state.id, type, true)], this.pos)
      return
    }

    let base = this.stack.length - ((depth - 1) * 3) - (action & STAY_FLAG ? 6 : 0)
    let start = this.stack[base - 2]
    let bufferBase = this.stack[base - 1], count = this.bufferBase + this.buffer.length - bufferBase
    if ((type & TERM_TAGGED) || (action & REDUCE_REPEAT_FLAG)) {
      let pos = this.state.skipped ? this.inputPos : this.pos
      if (this.inputPos == pos) { // Simple case, just append
        this.buffer.push(type, start, pos, count + 4)
      } else { // There may be skipped nodes that have to be moved forward
        let index = this.buffer.length
        while (index > 0 && this.buffer[index - 2] > pos) {
          // Move this record forward
          this.buffer[index] = this.buffer[index - 4]
          this.buffer[index + 1] = this.buffer[index - 3]
          this.buffer[index + 2] = this.buffer[index - 2]
          this.buffer[index + 3] = this.buffer[index - 1]
          index -= 4
          count -= 4
        }
        this.buffer[index] = type
        this.buffer[index + 1] = start
        this.buffer[index + 2] = pos
        this.buffer[index + 3] = count + 4
      }
    }
    if (action & STAY_FLAG) {
      this.state = this.cx.parser.states[this.stack[base]]
    } else {
      let baseStateID = this.stack[base - 3]
      this.state = this.cx.parser.states[this.cx.parser.getGoto(baseStateID, type, true)]
    }
    if (base < this.stack.length) this.stack.length = base
  }

  shiftValue(term: number, start: number, end: number, childCount = 4) {
    if (term == TERM_ERR) { // Try to omit superfluous error nodes
      let cur: Stack | null = this, top = this.buffer.length
      if (top == 0 && cur.parent) {
        top = cur.bufferBase - cur.parent.bufferBase
        cur = cur.parent
      }
      if (top > 0 && cur.buffer[top - 4] == TERM_ERR &&
          (start == end || cur.buffer[top - 2] >= start)) return
    }
    this.buffer.push(term, start, end, childCount)
  }

  shift(action: number, next: number, nextEnd: number) {
    if (action & GOTO_FLAG) {
      this.pushState(this.cx.parser.states[action & ACTION_VALUE_MASK], this.inputPos)
    } else if ((action & STAY_FLAG) == 0) { // Regular shift
      let start = this.inputPos, nextState = this.cx.parser.states[action]
      if (nextEnd > this.inputPos || (next & TERM_TAGGED)) {
        this.inputPos = nextEnd
        if (!nextState.skipped) this.pos = nextEnd
      }
      this.pushState(nextState, start)
      if (next & TERM_TAGGED) this.buffer.push(next, start, nextEnd, 4)
      this.badness = (this.badness >> 1) + (this.badness >> 2) // (* 0.75)
    } else { // Shift-and-stay, which means this is skipped token
      if (next & TERM_TAGGED) this.buffer.push(next, this.inputPos, nextEnd, 4)
      this.inputPos = nextEnd
    }
  }

  apply(action: number, next: number, nextEnd: number) {
    if (action & REDUCE_FLAG) this.reduce(action)
    else this.shift(action, next, nextEnd)
  }

  useNode(value: Tree, next: number) {
    let index = this.cx.reused.length - 1
    if (index < 0 || this.cx.reused[index] != value) {
      this.cx.reused.push(value)
      index++
    }
    let start = this.inputPos
    this.pos = this.inputPos = start + value.length
    this.pushState(this.cx.parser.states[next], start)
    this.badness >> 1 // FIXME
    this.buffer.push(index, start, this.pos, REUSED_VALUE)
  }

  split() {
    let parent: Stack | null = this
    let off = parent.buffer.length
    // Because the top of the buffer (after this.pos) may be mutated
    // to reorder reductions and skipped tokens, and shared buffers
    // should be immutable, this copies any outstanding skipped tokens
    // to the new buffer, and puts the base pointer before them.
    while (off > 0 && parent.buffer[off - 2] > parent.pos) off -= 4
    let buffer = parent.buffer.slice(off), base = parent.bufferBase + off
    // Make sure parent points to an actual parent with content, if there is such a parent.
    while (parent && base == parent.bufferBase) parent = parent.parent
    return new Stack(this.cx, this.stack.slice(), this.state, this.pos, this.inputPos,
                     this.badness, buffer, base, parent)
  }

  recoverByDelete(next: number, nextEnd: number) {
    if (next & TERM_TAGGED) this.shiftValue(next, this.inputPos, nextEnd)
    this.shiftValue(TERM_ERR, this.inputPos, nextEnd, (next & TERM_TAGGED) ? 8 : 4)
    this.inputPos = nextEnd
    this.badness += BADNESS_INCREMENT
  }

  canShift(term: number) {
    for (let sim = new SimulatedStack(this);;) {
      let action = sim.top.defaultReduce || this.cx.parser.hasAction(sim.top, term)
      if ((action & REDUCE_FLAG) == 0) return true
      if (action == 0) return false
      sim.reduce(action)
    }
  }

  get ruleStart() {
    let force = this.state.forcedReduce
    if (!(force & REDUCE_FLAG)) return 0
    let base = this.stack.length - (3 * (force >> REDUCE_DEPTH_SHIFT))
    return this.stack[base - 2]
  }

  canRecover(next: number) {
    // Scan for a state that has either a direct action or a recovery
    // action for next, without actually building up a new stack
    let visited: number[] | null = null, parser = this.cx.parser
    for (let sim = new SimulatedStack(this), i = 0;; i++) {
      if (parser.hasAction(sim.top, next) || parser.getRecover(sim.top, next) != 0) return true
      // Find a way to reduce from here
      let reduce = parser.anyReduce(sim.top)
      if (reduce == 0 && ((reduce = sim.top.forcedReduce) & REDUCE_FLAG) == 0) return false
      sim.reduce(reduce)
      if (i > 10) {
        // Guard against getting stuck in a cycle
        if (!visited) visited = []
        else if (i == 100 || visited.includes(sim.top.id)) return false
        visited.push(sim.top.id)
      }
    }
  }

  recoverByInsert(next: number, nextEnd: number): Stack | null {
    if (!this.canRecover(next)) return null

    // Now that we know there's a recovery to be found, run the
    // reduces again, the expensive way, updating the stack
    let result = this.split(), parser = this.cx.parser
    result.pos = result.inputPos
    result.badness += BADNESS_INCREMENT
    for (;;) {
      for (;;) {
        if (parser.hasAction(result.state, next)) return result
        let recover = parser.getRecover(result.state, next)
        if (!recover) break
        result.pushState(this.cx.parser.states[recover], result.pos)
        result.shiftValue(TERM_ERR, result.pos, result.pos)
      }

      result.forceReduce()
    }
  }

  forceReduce() {
    let reduce = this.cx.parser.anyReduce(this.state)
    if (reduce == 0) {
      // FIXME somehow mark the resulting node as not suitable for reuse
      reduce = this.state.forcedReduce
      if ((reduce & REDUCE_FLAG) == 0) return false
      this.shiftValue(TERM_ERR, this.pos, this.pos)
    }
    this.reduce(reduce)
    return true
  }

  compare(other: Stack) {
    return this.inputPos - other.inputPos || this.badness - other.badness
  }

  toTree(): Tree {
    return Tree.build(StackBufferCursor.create(this), this.cx.parser.id, this.cx.maxBufferLength, this.cx.reused)
  }
}

class SimulatedStack {
  top: ParseState
  rest: number[]
  offset: number
  constructor(readonly stack: Stack) {
    this.top = stack.state
    this.rest = stack.stack
    this.offset = this.rest.length
  }

  reduce(action: number) {
    let term = action & ACTION_VALUE_MASK, depth = action >> REDUCE_DEPTH_SHIFT
    if (depth == 0) {
      if (this.rest == this.stack.stack) this.rest = this.rest.slice()
      this.rest.push(this.top.id, 0, 0)
      this.offset += 3
    } else {
      this.offset -= (depth - 1) * 3
    }
    let goto = this.stack.cx.parser.getGoto(this.rest[this.offset - 3], term, true)
    this.top = this.stack.cx.parser.states[goto]
  }
}

class StackBufferCursor implements BufferCursor {
  buffer: number[]

  constructor(public stack: Stack, public pos: number, public index: number) {
    this.buffer = stack.buffer
    if (this.index == 0) this.maybeNext()
  }

  static create(stack: Stack) {
    return new StackBufferCursor(stack, stack.bufferBase + stack.buffer.length, stack.buffer.length)
  }

  maybeNext() {
    let next = this.stack.parent
    if (next != null) {
      this.index = this.stack.bufferBase - next.bufferBase
      this.stack = next
      this.buffer = next.buffer
    }
  }

  get type() { return this.buffer[this.index - 4] }
  get start() { return this.buffer[this.index - 3] }
  get end() { return this.buffer[this.index - 2] }
  get size() { return this.buffer[this.index - 1] }

  next() {
    this.index -= 4
    this.pos -= 4
    if (this.index == 0) this.maybeNext()
  }

  fork() {
    return new StackBufferCursor(this.stack, this.pos, this.index)
  }
}
