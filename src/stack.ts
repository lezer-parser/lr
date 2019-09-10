import {Action, Term, StateFlag, ParseState} from "./constants"
import {StackContext} from "./parse"
import {Tree, BufferCursor, NodeProp} from "lezer-tree"

export const enum Badness {
  // Amount to add for a single recover action
  Unit = 100,

  // Badness at which we disallow adding a stack if another stack
  // shares its top state and position.
  Deduplicate = 200,

  // The maximum amount of active stacks at which recovery actions are
  // applied
  MaxRecoverStacks = 25,

  // If badness reaches this level (and there are sibling stacks),
  // don't recover.
  TooBadToRecover = 500,

  // If the best sibling is this amount better than the current stack,
  // don't apply recovery.
  RecoverSiblingFactor = 3,

  // Constant used to prune stacks that run error-free alongside each
  // other for too long
  MaxParallelBufferLength = 800
}

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
// For those in the `Badness.Stabilizing` to `Badness.Wild` range, we
// assume that they are in the process of trying to recover and allow
// a bunch of them to continue alongside each other to see which one
// works out better.
//
// Stacks with the same low badness score are likely to be valid GLR
// parsing branches, so in that case it's often a good idea to let
// both continue.
//
// When a stack fails to find an advancing action, recovery is only
// applied when its badness is < `Badness.Wild`, or no better parse
// exists at that point.

/// A parse stack. These are used internally by the parser to track
/// parsing progress. They also provide some properties and methods
/// that external code such as a tokenizer can use to get information
/// about the parse state.
export class Stack {
  /// @internal
  constructor(
    // A group of values that the stack will share with all
    // split instances
    ///@internal
    readonly cx: StackContext,
    // Holds state, pos, value stack pos (15 bits array index, 15 bits
    // buffer index) triplets for all but the top state
    /// @internal
    readonly stack: number[],
    // The current parse state
    /// @internal
    public state: number,
    // The position at which the next reduce should take place. This
    // can be less than `this.pos` when skipped expressions have been
    // added to the stack (which should be moved outside of the next
    // reduction)
    /// @internal
    public reducePos: number,
    // The input position up to which this stack has parsed.
    public pos: number,
    // A measure of the amount of error-recovery that recently
    // happened on this stack
    /// @internal
    public badness: number,
    // The output buffer. Holds (type, start, end, size) quads
    // representing nodes created by the parser, where `size` is
    // amount of buffer array entries covered by this node.
    /// @internal
    readonly buffer: number[],
    // The base offset of the buffer. When stacks are split, the split
    // instance shared the buffer history with its parent up to
    // `bufferBase`, which is the absolute offset (including the
    // offset of previous splits) into the buffer at which this stack
    // starts writing.
    /// @internal
    readonly bufferBase: number,
    // A parent stack from which this was split off, if any. This is
    // set up so that it always points to a stack that has some
    // additional buffer content, never to a stack with an equal
    // `bufferBase`.
    /// @internal
    readonly parent: Stack | null
  ) {}

  /// @internal
  toString() {
    return "[" + this.stack.filter((_, i) => i % 3 == 0).concat(this.state).join(",") + "]"
  }

  // Start an empty stack
  /// @internal
  static start(cx: StackContext, pos = 0) {
    return new Stack(cx, [], cx.parser.states[0], pos, pos, 0, [], 0, null)
  }

  // Push a state onto the stack, tracking its start position as well
  // as the buffer base at that point.
  /// @internal
  pushState(state: number, start: number) {
    this.stack.push(this.state, start, this.bufferBase + this.buffer.length)
    this.state = state
  }

  // Apply a reduce action
  /// @internal
  reduce(action: number) {
    let depth = action >> Action.ReduceDepthShift, type = action & Action.ValueMask
    let {parser} = this.cx
    if (depth == 0) {
      // Zero-depth reductions are a special case—they add stuff to
      // the stack without popping anything off.
      if (type <= parser.maxNode) this.storeNode(type, this.reducePos, this.reducePos, 4, true)
      this.pushState(parser.getGoto(this.state, type, true), this.reducePos)
      return
    }

    // Find the base index into `this.stack`, content after which will
    // be dropped. Note that with `StayFlag` reductions we need to
    // consume two extra frames (the dummy parent node for the skipped
    // expression and the state that we'll be staying in, which should
    // be moved to `this.state`).
    let base = this.stack.length - ((depth - 1) * 3) - (action & Action.StayFlag ? 6 : 0)
    let start = this.stack[base - 2]
    let bufferBase = this.stack[base - 1], count = this.bufferBase + this.buffer.length - bufferBase
    if (type <= parser.maxNode && ((action & Action.RepeatFlag) || !parser.group.types[type].prop(NodeProp.repeated))) {
      let pos = parser.stateFlag(this.state, StateFlag.Skipped) ? this.pos : this.reducePos
      this.storeNode(type, start, pos, count + 4, true)
    }
    if (action & Action.StayFlag) {
      this.state = this.stack[base]
    } else {
      let baseStateID = this.stack[base - 3]
      this.state = parser.getGoto(baseStateID, type, true)
    }
    while (this.stack.length > base) this.stack.pop()
  }

  // Shift a value into the buffer
  /// @internal
  storeNode(term: number, start: number, end: number, size = 4, isReduce = false) {
    if (term == Term.Err) { // Try to omit/merge adjacent error nodes
      let cur: Stack | null = this, top = this.buffer.length
      if (top == 0 && cur.parent) {
        top = cur.bufferBase - cur.parent.bufferBase
        cur = cur.parent
      }
      if (top > 0 && cur.buffer[top - 4] == Term.Err && cur.buffer[top - 1] > -1) {
        if (start == end) return
        if (cur.buffer[top - 2] >= start) { cur.buffer[top - 2] = end; return }
      }
    }

    if (!isReduce || this.pos == end) { // Simple case, just append
      this.buffer.push(term, start, end, size)
    } else { // There may be skipped nodes that have to be moved forward
      let index = this.buffer.length
      if (index > 0 && this.buffer[index - 4] != Term.Err) while (index > 0 && this.buffer[index - 2] > end) {
        // Move this record forward
        this.buffer[index] = this.buffer[index - 4]
        this.buffer[index + 1] = this.buffer[index - 3]
        this.buffer[index + 2] = this.buffer[index - 2]
        this.buffer[index + 3] = this.buffer[index - 1]
        index -= 4
        if (size > 4) size -= 4
      }
      this.buffer[index] = term
      this.buffer[index + 1] = start
      this.buffer[index + 2] = end
      this.buffer[index + 3] = size
    }
  }

  // Apply a shift action
  /// @internal
  shift(action: number, next: number, nextEnd: number) {
    if (action & Action.GotoFlag) {
      this.pushState(action & Action.ValueMask, this.pos)
    } else if ((action & Action.StayFlag) == 0) { // Regular shift
      let start = this.pos, nextState = action, {parser} = this.cx
      if (nextEnd > this.pos || next <= parser.maxNode) {
        this.pos = nextEnd
        if (!parser.stateFlag(nextState, StateFlag.Skipped)) this.reducePos = nextEnd
      }
      this.pushState(nextState, start)
      if (next <= parser.maxNode) this.buffer.push(next, start, nextEnd, 4)
      this.badness = (this.badness >> 1) + (this.badness >> 2) // (* 0.75)
    } else { // Shift-and-stay, which means this is skipped token
      if (next <= this.cx.parser.maxNode) this.buffer.push(next, this.pos, nextEnd, 4)
      this.pos = nextEnd
    }
  }

  // Apply an action
  /// @internal
  apply(action: number, next: number, nextEnd: number) {
    if (action & Action.ReduceFlag) this.reduce(action)
    else this.shift(action, next, nextEnd)
  }

  // Add a prebuilt node into the buffer. This may be a reused node or
  // the result of running a nested parser.
  /// @internal
  useNode(value: Tree, next: number) {
    let index = this.cx.reused.length - 1
    if (index < 0 || this.cx.reused[index] != value) {
      this.cx.reused.push(value)
      index++
    }
    let start = this.pos
    this.reducePos = this.pos = start + value.length
    this.pushState(next, start)
    this.badness >>= 2 // (* 0.25)
    this.buffer.push(index, start, this.reducePos, -1 /* size < 0 means this is a reused value */)
  }

  // Split the stack. Due to the buffer sharing and the fact
  // that `this.stack` tends to stay quite shallow, this isn't very
  // expensive.
  /// @internal
  split() {
    let parent: Stack | null = this
    let off = parent.buffer.length
    // Because the top of the buffer (after this.pos) may be mutated
    // to reorder reductions and skipped tokens, and shared buffers
    // should be immutable, this copies any outstanding skipped tokens
    // to the new buffer, and puts the base pointer before them.
    while (off > 0 && parent.buffer[off - 2] > parent.reducePos) off -= 4
    let buffer = parent.buffer.slice(off), base = parent.bufferBase + off
    // Make sure parent points to an actual parent with content, if there is such a parent.
    while (parent && base == parent.bufferBase) parent = parent.parent
    return new Stack(this.cx, this.stack.slice(), this.state, this.reducePos, this.pos,
                     this.badness, buffer, base, parent)
  }

  // Try to recover from an error by 'deleting' (ignoring) one token.
  /// @internal
  recoverByDelete(next: number, nextEnd: number) {
    let isNode = next <= this.cx.parser.maxNode
    if (isNode) this.storeNode(next, this.pos, nextEnd)
    this.storeNode(Term.Err, this.pos, nextEnd, isNode ? 8 : 4)
    this.pos = this.reducePos = nextEnd
    this.badness += Badness.Unit
  }

  /// Check if the given term would be able to be shifted (optionally
  /// after some reductions) on this stack. This can be useful for
  /// external tokenizers that want to make sure they only provide a
  /// given token when it applies.
  canShift(term: number) {
    for (let sim = new SimulatedStack(this);;) {
      let action = this.cx.parser.stateSlot(sim.top, ParseState.DefaultReduce) || this.cx.parser.hasAction(sim.top, term)
      if ((action & Action.ReduceFlag) == 0) return true
      if (action == 0) return false
      sim.reduce(action)
    }
  }

  /// Find the start position of the rule that is currently being parsed.
  get ruleStart() {
    let force = this.cx.parser.stateSlot(this.state, ParseState.ForcedReduce)
    if (!(force & Action.ReduceFlag)) return 0
    let base = this.stack.length - (3 * (force >> Action.ReduceDepthShift))
    return this.stack[base + 1]
  }

  /// Find the start position of the innermost instance of any of the
  /// given term types, or return `-1` when none of them are found.
  ///
  /// **Note:** this is only reliable when there is at least some
  /// state that unambiguously matches the given rule on the stack.
  /// I.e. if you have a grammar like this, where the difference
  /// between `a` and `b` is only apparent at the third token:
  ///
  ///     a { b | c }
  ///     b { "x" "y" "x" }
  ///     c { "x" "y" "z" }
  ///
  /// Then a parse state after `"x"` will not reliably tell you that
  /// `b` is on the stack. You _can_ pass `[b, c]` to reliably check
  /// for either of those two rules (assuming that `a` isn't part of
  /// some rule that includes other things starting with `"x"`).
  startOf(types: readonly number[]) {
    for (let frame = this.stack.length - 3; frame >= 0; frame -= 3) {
      let force = this.cx.parser.stateSlot(this.stack[frame], ParseState.ForcedReduce)
      if (types.includes(force & Action.ValueMask)) {
        let base = frame - (3 * (force >> Action.ReduceDepthShift))
        return this.stack[base + 1]
      }
    }
    return -1
  }

  // Apply up to Recover.MaxNext recovery actions that conceptually
  // inserts some missing token or rule.
  /// @internal
  recoverByInsert(next: number): Stack[] {
    let nextStates = this.cx.parser.nextStates(this.state)
    if (nextStates.length > Recover.MaxNext) {
      let best = nextStates.filter(s => s != this.state && this.cx.parser.hasAction(s, next))
      for (let i = 0; best.length < Recover.MaxNext && i < nextStates.length; i++)
        if (!best.includes(nextStates[i])) best.push(nextStates[i])
      nextStates = best
    }
    let result: Stack[] = []
    for (let i = 0; i < nextStates.length && result.length < Recover.MaxNext; i++) {
      if (nextStates[i] == this.state) continue
      let stack = this.split()
      stack.storeNode(Term.Err, stack.pos, stack.pos, 4, true)
      stack.pushState(nextStates[i], this.pos)
      stack.badness += Badness.Unit
      result.push(stack)
    }
    return result
  }

  // Force a reduce, if possible. Return false if that can't
  // be done.
  /// @internal
  forceReduce() {
    let reduce = this.cx.parser.anyReduce(this.state)
    if ((reduce >> Action.ReduceDepthShift) == 0) { // Don't use 0 or a zero-depth reduction
      reduce = this.cx.parser.stateSlot(this.state, ParseState.ForcedReduce)
      if ((reduce & Action.ReduceFlag) == 0) return false
      this.storeNode(Term.Err, this.reducePos, this.reducePos, 4, true)
      this.badness += Badness.Unit
    }
    this.reduce(reduce)
    return true
  }

  // Compare two stacks to get a number that indicates which one is
  // behind or, if they are at the same position, which one has less
  // badness.
  /// @internal
  compare(other: Stack) {
    return this.pos - other.pos || this.badness - other.badness
  }

  // Convert the stack's buffer to a syntax tree.
  /// @internal
  toTree(): Tree {
    return Tree.build(StackBufferCursor.create(this), this.cx.parser.group, Term.Top, this.cx.maxBufferLength, this.cx.reused)
  }
}

const enum Recover {
  MaxNext = 4
}

// Used to cheaply run some reductions to scan ahead without mutating
// an entire stack
class SimulatedStack {
  top: number
  rest: number[]
  offset: number

  constructor(readonly stack: Stack) {
    this.top = stack.state
    this.rest = stack.stack
    this.offset = this.rest.length
  }

  reduce(action: number) {
    let term = action & Action.ValueMask, depth = action >> Action.ReduceDepthShift
    if (depth == 0) {
      if (this.rest == this.stack.stack) this.rest = this.rest.slice()
      this.rest.push(this.top, 0, 0)
      this.offset += 3
    } else {
      this.offset -= (depth - 1) * 3
    }
    let goto = this.stack.cx.parser.getGoto(this.rest[this.offset - 3], term, true)
    this.top = goto
  }
}

// This is given to `Tree.build` to build a buffer, and encapsulates
// the parent-stack-walking necessary to read the nodes.
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

  get id() { return this.buffer[this.index - 4] }
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
