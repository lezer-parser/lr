import {Parser, ParseState,
        MAX_TAGGED_TERM, MAX_REPEAT_TERM, FIRST_REPEAT_TERM,
        TERM_ERR, REDUCE_NAME_MASK, REDUCE_NAME_SIZE} from "./parser"
import {Node, Tree, TreeBuffer, SyntaxTree, MAX_BUFFER_LENGTH, MAX_BUFFER_SIZE} from "./tree"

const VALUE_INDEX_SIZE = 15, VALUE_INDEX_MASK = 2**VALUE_INDEX_SIZE - 1

const BADNESS_DELETE = 100, BADNESS_RECOVER = 90
export const BADNESS_STABILIZING = 50, BADNESS_WILD = 150 // Limits in between which stacks are less agressively pruned

// (FIXME: this will go out of date before I know it, revisit at some
// point)
//
// Badness is a measure of how off-the-rails a given parse is. It is
// bumped when a recovery strategy is applied, and then reduced (by
// multiplication with a constant < 1) for every successful (real)
// token shifted.
//
// Stacks with a low badness are relatively credible parses that have
// shift matching the input in their recent history. Stacks with a
// high badness are deeply in the weeds and likely wrong. For each of
// these, we prune agressively by dropping stacks when another stack
// at the same position is looking better.
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

export class Stack {
  constructor(readonly parser: Parser,
              // Holds state, pos, value stack pos (15 bits array index, 15 bits buffer index) triplets for all but the top state
              readonly stack: number[],
              public state: ParseState,
              public pos: number,
              readonly values: (Node | number[])[],
              readonly valueInfo: number[],
              public badness: number) {}

  toString() {
    return "[" + this.stack.filter((_, i) => i % 3 == 0).concat(this.state.id).join(",") + "] " +
      this.values.map((v, i) => Array.isArray(v) ? Tree.fromBuffer(v, 0) : v).join(",")
  }

  static start(parser: Parser) {
    return new Stack(parser, [], parser.states[0], 0, [], [], 0)
  }

  reduceValue(stackIndex: number, stackOffset: number, start: number): Tree {
    let children: (Node | TreeBuffer)[] = [], positions: number[] = []
    for (let i = stackIndex; i < this.values.length; i++) {
      let value = this.values[i], valueInfo = this.valueInfo[i]
      if (Array.isArray(value)) {
        let startIndex = i == stackIndex ? stackOffset << 2 : 0
        if (startIndex < value.length)
          TreeBuffer.build(value, startIndex, start, children, positions)
        if (startIndex > 0) value.length = startIndex
      } else {
        children.push(value)
        positions.push(valueInfo - start)
      }
    }
    let newLen = stackIndex + (stackOffset ? 1 : 0)
    if (this.values.length > newLen)
      this.values.length = this.valueInfo.length = newLen
    return new Tree(children, positions)
  }

  pushState(state: ParseState, start: number) {
    let valueIndex
    if (this.values.length > 0) {
      let last = this.values[this.values.length - 1]
      valueIndex = last instanceof Node ? this.values.length : this.values.length - 1 + ((last.length >> 2) << VALUE_INDEX_SIZE)
    } else {
      valueIndex = 0
    }
    this.stack.push(this.state.id, start, valueIndex)
    this.state = state
  }

  reduce(depth: number, tag: number) {
    if (depth == 0) {
      this.pushState(this.parser.states[this.state.getGoto(tag)], this.pos)
      return
    }

    let base = this.stack.length - ((depth - 1) * 3)
    let start = this.stack[base - 2]
    if (tag <= MAX_TAGGED_TERM) {
      let valueIndex = this.stack[base - 1]
      let stackIndex = valueIndex & VALUE_INDEX_MASK, stackOffset = valueIndex >> VALUE_INDEX_SIZE
      let top = this.values.length - 1
      let length = this.pos - start, pushed = false
      push: if (length <= MAX_BUFFER_LENGTH && stackIndex >= top) {
        let needNew = stackIndex > top, childCount = 0, buffer!: number[]
        if (!needNew) {
          buffer = this.values[top] as number[]
          if (!Array.isArray(buffer)) break push
          childCount = (buffer.length >> 2) - stackOffset
          if (buffer.length >= MAX_BUFFER_SIZE) {
            if (childCount) break push
            needNew = true
          }
        }
        if (needNew) {
          this.values.push(buffer = [])
          this.valueInfo.push(0)
        }
        buffer.push(tag, start, this.pos, childCount)
        pushed = true
      }
      if (!pushed) {
        this.values.push(this.reduceValue(stackIndex, stackOffset, start).toNode(tag, length))
        this.valueInfo.push(start)
      }
    } else if (tag <= MAX_REPEAT_TERM && this.pos - start > MAX_BUFFER_LENGTH) {
      let valueIndex = this.stack[base - 1]
      let balanced = this.reduceValue(valueIndex & VALUE_INDEX_MASK, valueIndex >> VALUE_INDEX_SIZE, start)
        .balance(this.parser.repeats[tag - FIRST_REPEAT_TERM])
      this.values.push(balanced)
      this.valueInfo.push(start)
    }
    let baseState = this.parser.states[this.stack[base - 3]]
    this.state = this.parser.states[baseState.getGoto(tag)]
    if (depth > 1) this.stack.length = base
  }

  shiftValue(term: number, start: number, end: number, childCount = 0) {
    let last!: number[]
    if (this.values.length == 0 ||
        !Array.isArray(last = this.values[this.values.length - 1] as number[]) ||
        last.length >= MAX_BUFFER_SIZE) {
      this.values.push(last = [])
      this.valueInfo.push(0)
    }
    if (term == TERM_ERR && last.length && last[last.length - 4] == term &&
        (start == end || last[last.length - 3] == start)) return
    last.push(term, start, end, childCount)
  }

  apply(action: number, next: number, nextStart: number, nextEnd: number, skipped: number[]) {
    if (action >= 0) {
      this.reduce(action >> REDUCE_NAME_SIZE, action & REDUCE_NAME_MASK)
    } else { // Shift
      this.shiftSkipped(skipped)
      this.pushState(this.parser.states[-action], nextStart)
      this.pos = nextEnd
      if (next <= MAX_TAGGED_TERM) this.shiftValue(next, nextStart, nextEnd)
      this.badness = (this.badness >> 1) + (this.badness >> 2) // (* 0.75)
    }
  }

  useCached(value: Node, start: number, next: ParseState) {
    this.pushState(next, start)
    this.values.push(value)
    this.valueInfo.push(start)
    this.pos = start + value.length
    this.badness >> 1 // FIXME
  }

  split() {
    return new Stack(this.parser, this.stack.slice(), this.state, this.pos,
                     this.values.map(v => Array.isArray(v) ? v.slice() : v),
                     this.valueInfo.slice(), this.badness)
  }

  shiftSkipped(skipped: number[]) {
    for (let i = 0; i < skipped.length; i += 3)
      this.shiftValue(skipped[i + 2], skipped[i], skipped[i + 1])
  }

  recoverByDelete(next: number, nextStart: number, nextEnd: number, skipped: number[]) {
    this.shiftSkipped(skipped)
    if (next <= MAX_TAGGED_TERM) this.shiftValue(next, nextStart, nextEnd)
    this.shiftValue(TERM_ERR, nextStart, nextEnd, next <= MAX_TAGGED_TERM ? 1 : 0)
    this.pos = nextEnd
    this.badness += BADNESS_DELETE
  }

  canRecover(next: number) {
    // Scan for a state that has either a direct action or a recovery
    // action for next, without actually building up a new stack
    // FIXME this can continue infinitely without the i < 100 limit
    for (let top = this.state, rest = this.stack, offset = rest.length, i = 0; i < 100; i++) {
      if (top.hasAction(next) || top.getRecover(next) != 0) return true
      // Find a way to reduce from here
      let reduce = top.anyReduce()
      if (reduce < 0 && (reduce = top.defaultReduce) < 0) return false
      let term = reduce & REDUCE_NAME_MASK, n = reduce >> REDUCE_NAME_SIZE
      if (n == 0) { // FIXME
        rest = rest.slice()
        rest.push(term, 0, 0)
        offset += 3
      } else {
        offset -= (n - 1) * 3
      }
      let goto = this.parser.states[rest[offset - 3]].getGoto(term)
      if (goto < 0) return false
      top = this.parser.states[goto]
    }
    return false
  }

  recoverByInsert(next: number, nextStart: number, nextEnd: number): Stack | null {
    if (!this.canRecover(next)) return null

    // Now that we know there's a recovery to be found, run the
    // reduces again, the expensive way, updating the stack
    let result = this.split()
    result.badness += BADNESS_RECOVER
    for (;;) {
      for (;;) {
        if (result.state.hasAction(next)) return result
        let recover = result.state.getRecover(next)
        if (!recover) break
        let pos = result.pos
        result.pushState(this.parser.states[recover], pos)
        result.shiftValue(TERM_ERR, pos, pos)
      }
      
      let reduce = result.state.anyReduce()
      if (reduce == 0) {
        // Force a reduce using this state's default reduce
        result.shiftValue(TERM_ERR, result.pos, result.pos)
        reduce = result.state.defaultReduce
      }
      result.reduce(reduce >> REDUCE_NAME_SIZE, reduce & REDUCE_NAME_MASK)
    }
  }

  toTree(): SyntaxTree {
    return this.reduceValue(0, 0, 0)
  }

  compare(other: Stack) {
    return this.pos - other.pos || this.badness - other.badness
  }

  put(parses: Stack[], strict = this.badness < BADNESS_STABILIZING || this.badness > BADNESS_WILD): boolean {
    for (let i = 0; i < parses.length; i++) {
      let other = parses[i]
      if ((strict || other.state == this.state) && other.pos == this.pos) {
        let diff = this.badness - other.badness || (this.badness < BADNESS_STABILIZING ? 0 : this.stack.length - other.stack.length)
        if (diff < 0) { parses[i] = this; return true }
        else if (diff > 0) return false
      }
    }

    // Binary heap add
    let index = parses.push(this) - 1
    while (index > 0) {
      let parentIndex = index >> 1, parent = parses[parentIndex]
      if (this.compare(parent) >= 0) break
      parses[index] = parent
      parses[parentIndex] = this
      index = parentIndex
    }
    return true
  }

  static take(parses: Stack[]) {
    // Binary heap pop
    let elt = parses[0], replacement = parses.pop()!
    if (parses.length == 0) return elt
    parses[0] = replacement
    for (let index = 0;;) {
      let childIndex = (index << 1) + 1
      if (childIndex >= parses.length) break
      let child = parses[childIndex]
      if (childIndex + 1 < parses.length && child.compare(parses[childIndex + 1]) >= 0) {
        child = parses[childIndex + 1]
        childIndex++
      }
      if (replacement.compare(child) < 0) break
      parses[childIndex] = replacement
      parses[index] = child
      index = childIndex
    }
    return elt
  }
}
