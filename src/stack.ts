import {ParseState, REDUCE_DEPTH_MASK, REDUCE_DEPTH_SIZE} from "./state"
import {ANON_TERM, FIRST_REPEAT_TERM, TERM_ERR} from "./term"
import {Parser} from "./parse"
import {Node, Tree, TreeBuffer, SyntaxTree} from "./tree"

const BADNESS_DELETE = 100, BADNESS_RECOVER = 100
export const BADNESS_STABILIZING = 50, BADNESS_WILD = 150 // Limits in between which stacks are less agressively pruned

const REUSED_VALUE = -1

export const DEFAULT_BUFFER_LENGTH = 2048
export let MAX_BUFFER_LENGTH = DEFAULT_BUFFER_LENGTH

export function setBufferLength(len: number) { MAX_BUFFER_LENGTH = len }

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
  constructor(readonly parser: Parser, // FIXME put these top two in dynamic variables?
              readonly reused: Node[],
              // Holds state, pos, value stack pos (15 bits array index, 15 bits buffer index) triplets for all but the top state
              readonly stack: number[],
              public state: ParseState,
              public pos: number,
              public badness: number,
              // Holds tag,start,end,nodeCount quads
              readonly buffer: number[],
              readonly bufferBase: number,
              readonly parent: Stack | null) {}

  toString() {
    return "[" + this.stack.filter((_, i) => i % 3 == 0).concat(this.state.id).join(",") + "]"
  }

  static start(parser: Parser) {
    return new Stack(parser, [], [], parser.states[0], 0, 0, [], 0, null)
  }

  pushState(state: ParseState, start: number) {
    this.stack.push(this.state.id, start, this.bufferBase + this.buffer.length)
    this.state = state
  }

  reduce(action: number) { // Encoded reduction action
    let depth = action & REDUCE_DEPTH_MASK, tag = action >> REDUCE_DEPTH_SIZE
    if (depth == 0) {
      this.pushState(this.parser.states[this.state.getGoto(tag)], this.pos)
      return
    }

    let base = this.stack.length - ((depth - 1) * 3)
    let start = this.stack[base - 2]
    let count = this.bufferBase + this.buffer.length - this.stack[base - 1]
    if ((tag & ANON_TERM) == 0 ||
        (tag >= FIRST_REPEAT_TERM && this.pos - start > MAX_BUFFER_LENGTH && count > 0))
      this.buffer.push(tag, start, this.pos, count + 4)
    let baseState = this.parser.states[this.stack[base - 3]]
    this.state = this.parser.states[baseState.getGoto(tag)]
    if (depth > 1) this.stack.length = base
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

  apply(action: number, next: number, nextStart: number, nextEnd: number, skipped: number[]) {
    if (action >= 0) {
      this.reduce(action)
    } else { // Shift
      this.shiftSkipped(skipped)
      this.pushState(this.parser.states[-action], nextStart)
      this.pos = nextEnd
      if ((next & ANON_TERM) == 0) this.shiftValue(next, nextStart, nextEnd)
      this.badness = (this.badness >> 1) + (this.badness >> 2) // (* 0.75)
    }
  }

  useCached(value: Node, start: number, next: ParseState) {
    let index = this.reused.length - 1
    if (index < 0 || this.reused[index] != value) {
      this.reused.push(value)
      index++
    }
    this.pos = start + value.length
    this.pushState(next, start)
    this.badness >> 1 // FIXME
    this.buffer.push(index, start, this.pos, REUSED_VALUE)
  }

  split() {
    let parent: Stack | null = this
    // Make sure parent points to an actual parent with content, if there is such a parent.
    while (parent && parent.buffer.length == 0) parent = parent.parent
    return new Stack(this.parser, this.reused, this.stack.slice(), this.state, this.pos,
                     this.badness, [], parent ? parent.bufferBase + parent.buffer.length : 0, parent)
  }

  shiftSkipped(skipped: number[]) {
    for (let i = 0; i < skipped.length; i += 3)
      this.buffer.push(skipped[i + 2], skipped[i], skipped[i + 1], 4)
  }

  recoverByDelete(next: number, nextStart: number, nextEnd: number, skipped: number[]) {
    this.shiftSkipped(skipped)
    if ((next & ANON_TERM) == 0) this.shiftValue(next, nextStart, nextEnd)
    this.shiftValue(TERM_ERR, nextStart, nextEnd, (next & ANON_TERM) ? 4 : 8)
    this.pos = nextEnd
    this.badness += BADNESS_DELETE
  }

  canRecover(next: number) {
    // Scan for a state that has either a direct action or a recovery
    // action for next, without actually building up a new stack
    // FIXME this can continue infinitely without the i < 100 limit, should build up a set of visited states
    for (let top = this.state, rest = this.stack, offset = rest.length, i = 0; i < 100; i++) {
      if (top.hasAction(next) || top.getRecover(next) != 0) return true
      // Find a way to reduce from here
      let reduce = top.anyReduce()
      if (reduce == 0 && (reduce = top.defaultReduce) < 0) return false
      let term = reduce >> REDUCE_DEPTH_SIZE, depth = reduce & REDUCE_DEPTH_MASK
      if (depth == 0) {
        if (rest == this.stack) rest = rest.slice()
        rest.push(top.id, 0, 0)
        offset += 3
      } else {
        offset -= (depth - 1) * 3
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
      result.reduce(reduce)
    }
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

  toTree(): SyntaxTree {
    let children: (Node | TreeBuffer)[] = [], positions: number[] = []
    let cursor = new BufferCursor(this)
    while (!cursor.done) cursor.takeNode(0, children, positions)
    return new Tree(children.reverse(), positions.reverse())
  }
}

const BALANCE_BRANCH_FACTOR = 5

class BufferCursor {
  buffer: number[]
  index: number
  taken = 0

  constructor(public stack: Stack) {
    this.buffer = stack.buffer
    this.index = this.buffer.length
    if (this.index == 0) this.maybeNext()
  }

  maybeNext() {
    let next = this.stack.parent
    if (next != null) {
      this.index = this.stack.bufferBase - next.bufferBase
      this.stack = next
      this.buffer = next.buffer
    }
  }

  get tag() { return this.buffer[this.index - 4] }
  get start() { return this.buffer[this.index - 3] }
  get end() { return this.buffer[this.index - 2] }
  get size() { return this.buffer[this.index - 1] }

  get done() { return this.index == 0 }

  forward() {
    this.index -= 4
    this.taken += 4
    if (this.index == 0) this.maybeNext()
  }

  takeNode(parentStart: number, children: (Node | TreeBuffer)[], positions: number[]) {
    let {tag, start, end, size} = this
    if (size == REUSED_VALUE) {
      this.forward()
      children.push(this.stack.reused[tag])
      positions.push(start - parentStart)
    } else if (end - start > MAX_BUFFER_LENGTH) { // Too big for a buffer, make it a node
      let endCount = this.taken + size
      this.forward()
      let localChildren: (Node | TreeBuffer)[] = [], localPositions: number[] = []
      while (this.taken < endCount) this.takeNode(start, localChildren, localPositions)
      localChildren.reverse(); localPositions.reverse()
      if (tag >= FIRST_REPEAT_TERM)
        children.push(this.balanceRange(this.stack.parser.getRepeat(tag), localChildren, localPositions, 0, localChildren.length))
      else
        children.push(new Node(tag, end - start, localChildren, localPositions))
      positions.push(start - parentStart)
    } else {
      let {bufferSize, bufferStart} = this.findBufferStart(size, start, end)
      let buffer = new Uint16Array(bufferSize)
      let endCount = this.taken + bufferSize, index = bufferSize
      while (this.taken < endCount)
        index = this.copyToBuffer(bufferStart, buffer, index)
      children.push(new TreeBuffer(buffer))
      positions.push(bufferStart - parentStart)
    }
  }

  // FIXME this should unwrap overbig nodes with the matching tag to prevent repeated reuse from creating an unbalanced tree
  balanceRange(tag: number,
               children: readonly (Node | TreeBuffer)[], positions: readonly number[],
               from: number, to: number): Node {
    let start = positions[from], length = (positions[to - 1] + children[to - 1].length) - start
    if (from == to - 1 && start == 0) {
      let first = children[from]
      if (first instanceof Node) return first
    }
    let localChildren = [], localPositions = []
    if (length <= MAX_BUFFER_LENGTH) {
      for (let i = from; i < to; i++) {
        let child = children[i]
        localChildren.push(child)
        localPositions.push(positions[i] - start)
      }
    } else {
      let maxChild = Math.max(MAX_BUFFER_LENGTH, Math.ceil(length / BALANCE_BRANCH_FACTOR))
      for (let i = from; i < to;) {
        let groupFrom = i, groupStart = positions[i]
        i++
        for (; i < to; i++) {
          let nextEnd = positions[i] + children[i].length
          if (nextEnd - groupStart > maxChild) break
        }
        if (i == groupFrom + 1) {
          let only = children[groupFrom]
          if (!(only instanceof Node) || only.tag != tag) only = new Node(tag, only.length, [only], [0])
          localChildren.push(only)
        } else {
          localChildren.push(this.balanceRange(tag, children, positions, groupFrom, i))
        }
        localPositions.push(groupStart - start)
      }
    }
    return new Node(tag, length, localChildren, localPositions)
  }

  findBufferStart(size: number, start: number, end: number) {
    let stack = this.stack, index = this.index, skip = size
    outer: for (;;) {
      for (;;) { // Forward stack/index to provide access to next node
        if (index > skip) { index -= skip; break }
        if (!stack.parent) break outer
        skip -= index
        index = stack.bufferBase - stack.parent.bufferBase
        stack = stack.parent
      }
      let nextSize = stack.buffer[index - 1]
      let nextStart = stack.buffer[index - 3]
      if (nextSize == REUSED_VALUE || end - nextStart > MAX_BUFFER_LENGTH) break outer
      start = nextStart
      size += nextSize
      skip = nextSize
    }
    return {bufferSize: size, bufferStart: start}
  }

  copyToBuffer(bufferStart: number, buffer: Uint16Array, index: number): number {
    let {tag, start, end, size} = this
    this.forward()
    if (size > 4) {
      let firstChildIndex = index - (size - 4)
      while (index > firstChildIndex)
        index = this.copyToBuffer(bufferStart, buffer, index)
    }
    buffer[--index] = (size >> 2) - 1
    buffer[--index] = end - bufferStart
    buffer[--index] = start - bufferStart
    buffer[--index] = tag
    return index
  }
}
