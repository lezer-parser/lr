import {Action, Term, StateFlag, ParseState, Seq} from "./constants"
import {Parse, ContextTracker} from "./parse"
import {Tree, TreeBuffer, BufferCursor} from "lezer-tree"

/// A parse stack. These are used internally by the parser to track
/// parsing progress. They also provide some properties and methods
/// that external code such as a tokenizer can use to get information
/// about the parse state.
export class Stack {
  /// @internal
  constructor(
    /// A the parse that this stack is part of @internal
    readonly p: Parse,
    /// Holds state, pos, value stack pos (15 bits array index, 15 bits
    /// buffer index) triplets for all but the top state
    /// @internal
    readonly stack: number[],
    /// The current parse state @internal
    public state: number,
    // The position at which the next reduce should take place. This
    // can be less than `this.pos` when skipped expressions have been
    // added to the stack (which should be moved outside of the next
    // reduction)
    /// @internal
    public reducePos: number,
    /// The input position up to which this stack has parsed.
    public pos: number,
    /// The dynamic score of the stack, including dynamic precedence
    /// and error-recovery penalties
    /// @internal
    public score: number,
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
    /// @internal
    public curContext: StackContext | null,
    // A parent stack from which this was split off, if any. This is
    // set up so that it always points to a stack that has some
    // additional buffer content, never to a stack with an equal
    // `bufferBase`.
    /// @internal
    readonly parent: Stack | null
  ) {}

  /// @internal
  toString() {
    return `[${this.stack.filter((_, i) => i % 3 == 0).concat(this.state)}]@${this.pos}${this.score ? "!" + this.score : ""}`
  }

  // Start an empty stack
  /// @internal
  static start(p: Parse, state: number, pos = 0) {
    let cx = p.parser.context
    return new Stack(p, [], state, pos, pos, 0, [], 0, cx ? new StackContext(cx, cx.start) : null, null)
  }

  /// The stack's current [context](#lezer.ContextTracker) value, if
  /// any. Its type will depend on the context tracker's type
  /// parameter, or it will be `null` if there is no context
  /// tracker.
  get context() { return this.curContext ? this.curContext.context : null }

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
    let {parser} = this.p

    let dPrec = parser.dynamicPrecedence(type)
    if (dPrec) this.score += dPrec

    if (depth == 0) {
      // Zero-depth reductions are a special case—they add stuff to
      // the stack without popping anything off.
      if (type < parser.minRepeatTerm) this.storeNode(type, this.reducePos, this.reducePos, 4, true)
      this.pushState(parser.getGoto(this.state, type, true), this.reducePos)
      this.reduceContext(type)
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
    // Store normal terms or `R -> R R` repeat reductions
    if (type < parser.minRepeatTerm || (action & Action.RepeatFlag)) {
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
    this.reduceContext(type)
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
      let start = this.pos, nextState = action, {parser} = this.p
      if (nextEnd > this.pos || next <= parser.maxNode) {
        this.pos = nextEnd
        if (!parser.stateFlag(nextState, StateFlag.Skipped)) this.reducePos = nextEnd
      }
      this.pushState(nextState, start)
      if (next <= parser.maxNode) this.buffer.push(next, start, nextEnd, 4)
      this.shiftContext(next)
    } else { // Shift-and-stay, which means this is a skipped token
      if (next <= this.p.parser.maxNode) this.buffer.push(next, this.pos, nextEnd, 4)
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
  useNode(value: Tree | TreeBuffer, next: number) {
    let index = this.p.reused.length - 1
    if (index < 0 || this.p.reused[index] != value) {
      this.p.reused.push(value)
      index++
    }
    let start = this.pos
    this.reducePos = this.pos = start + value.length
    this.pushState(next, start)
    this.buffer.push(index, start, this.reducePos, -1 /* size < 0 means this is a reused value */)
    if (this.curContext) this.updateContext(this.curContext.tracker.reuse(this.curContext.context, value, this.p.input, this))
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
    return new Stack(this.p, this.stack.slice(), this.state, this.reducePos, this.pos,
                     this.score, buffer, base, this.curContext, parent)
  }

  // Try to recover from an error by 'deleting' (ignoring) one token.
  /// @internal
  recoverByDelete(next: number, nextEnd: number) {
    let isNode = next <= this.p.parser.maxNode
    if (isNode) this.storeNode(next, this.pos, nextEnd)
    this.storeNode(Term.Err, this.pos, nextEnd, isNode ? 8 : 4)
    this.pos = this.reducePos = nextEnd
    this.score -= Recover.Token
  }

  /// Check if the given term would be able to be shifted (optionally
  /// after some reductions) on this stack. This can be useful for
  /// external tokenizers that want to make sure they only provide a
  /// given token when it applies.
  canShift(term: number) {
    for (let sim = new SimulatedStack(this);;) {
      let action = this.p.parser.stateSlot(sim.top, ParseState.DefaultReduce) || this.p.parser.hasAction(sim.top, term)
      if ((action & Action.ReduceFlag) == 0) return true
      if (action == 0) return false
      sim.reduce(action)
    }
  }

  /// Find the start position of the rule that is currently being parsed.
  get ruleStart() {
    for (let state = this.state, base = this.stack.length;;) {
      let force = this.p.parser.stateSlot(state, ParseState.ForcedReduce)
      if (!(force & Action.ReduceFlag)) return 0
      base -= 3 * (force >> Action.ReduceDepthShift)
      if ((force & Action.ValueMask) < this.p.parser.minRepeatTerm)
        return this.stack[base + 1]
      state = this.stack[base]
    }
  }

  /// Find the start position of an instance of any of the given term
  /// types, or return `null` when none of them are found.
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
  ///
  /// When `before` is given, this keeps scanning up the stack until
  /// it finds a match that starts before that position.
  ///
  /// Note that you have to be careful when using this in tokenizers,
  /// since it's relatively easy to introduce data dependencies that
  /// break incremental parsing by using this method.
  startOf(types: readonly number[], before?: number) {
    let state = this.state, frame = this.stack.length, {parser} = this.p
    for (;;) {
      let force = parser.stateSlot(state, ParseState.ForcedReduce)
      let depth = force >> Action.ReduceDepthShift, term = force & Action.ValueMask
      if (types.indexOf(term) > -1) {
        let base = frame - (3 * (force >> Action.ReduceDepthShift)), pos = this.stack[base + 1]
        if (before == null || before > pos) return pos
      }
      if (frame == 0) return null
      if (depth == 0) {
        frame -= 3
        state = this.stack[frame]
      } else {
        frame -= 3 * (depth - 1)
        state = parser.getGoto(this.stack[frame - 3], term, true)
      }
    }
  }

  // Apply up to Recover.MaxNext recovery actions that conceptually
  // inserts some missing token or rule.
  /// @internal
  recoverByInsert(next: number): Stack[] {
    if (this.stack.length >= Recover.MaxInsertStackDepth) return []

    let nextStates = this.p.parser.nextStates(this.state)
    if (nextStates.length > Recover.MaxNext << 1 || this.stack.length >= Recover.DampenInsertStackDepth) {
      let best = []
      for (let i = 0, s; i < nextStates.length; i += 2) {
        if ((s = nextStates[i + 1]) != this.state && this.p.parser.hasAction(s, next))
          best.push(nextStates[i], s)
      }
      if (this.stack.length < Recover.DampenInsertStackDepth)
        for (let i = 0; best.length < Recover.MaxNext << 1 && i < nextStates.length; i += 2) {
          let s = nextStates[i + 1]
          if (!best.some((v, i) => (i & 1) && v == s)) best.push(nextStates[i], s)
        }
      nextStates = best
    }
    let result: Stack[] = []
    for (let i = 0; i < nextStates.length && result.length < Recover.MaxNext; i += 2) {
      let s = nextStates[i + 1]
      if (s == this.state) continue
      let stack = this.split()
      stack.storeNode(Term.Err, stack.pos, stack.pos, 4, true)
      stack.pushState(s, this.pos)
      stack.shiftContext(nextStates[i])
      stack.score -= Recover.Token
      result.push(stack)
    }
    return result
  }

  // Force a reduce, if possible. Return false if that can't
  // be done.
  /// @internal
  forceReduce() {
    let reduce = this.p.parser.stateSlot(this.state, ParseState.ForcedReduce)
    if ((reduce & Action.ReduceFlag) == 0) return false
    if (!this.p.parser.validAction(this.state, reduce)) {
      this.storeNode(Term.Err, this.reducePos, this.reducePos, 4, true)
      this.score -= Recover.Reduce
    }
    this.reduce(reduce)
    return true
  }

  /// @internal
  forceAll() {
    while (!this.p.parser.stateFlag(this.state, StateFlag.Accepting) && this.forceReduce()) {}
    return this
  }

  /// Check whether this state has no further actions (assumed to be a direct descendant of the
  /// top state, since any other states must be able to continue
  /// somehow). @internal
  get deadEnd() {
    if (this.stack.length != 3) return false
    let {parser} = this.p
    return parser.data[parser.stateSlot(this.state, ParseState.Actions)] == Seq.End &&
      !parser.stateSlot(this.state, ParseState.DefaultReduce)
  }

  /// Restart the stack (put it back in its start state). Only safe
  /// when this.stack.length == 3 (state is directly below the top
  /// state). @internal
  restart() {
    this.state = this.stack[0]
    this.stack.length = 0
  }

  /// @internal
  sameState(other: Stack) {
    if (this.state != other.state || this.stack.length != other.stack.length) return false
    for (let i = 0; i < this.stack.length; i += 3)
      if (this.stack[i] != other.stack[i]) return false
    return true
  }

  /// Get the parser used by this stack.
  get parser() { return this.p.parser }

  /// Test whether a given dialect (by numeric ID, as exported from
  /// the terms file) is enabled.
  dialectEnabled(dialectID: number) { return this.p.parser.dialect.flags[dialectID] }

  private shiftContext(term: number) {
    if (this.curContext)
      this.updateContext(this.curContext.tracker.shift(this.curContext.context, term, this.p.input, this))
  }

  private reduceContext(term: number) {
    if (this.curContext)
      this.updateContext(this.curContext.tracker.reduce(this.curContext.context, term, this.p.input, this))
  }

  /// @internal
  emitContext() {
    let last = this.buffer.length - 1
    if (last < 0 || this.buffer[last] != -2)
      this.buffer.push(this.curContext!.hash, this.reducePos, this.reducePos, -2)
  }

  private updateContext(context: any) {
    if (context != this.curContext!.context) {
      let newCx = new StackContext(this.curContext!.tracker, context)
      if (newCx.hash != this.curContext!.hash) this.emitContext()
      this.curContext = newCx
    }
  }
}

class StackContext {
  readonly hash: number
  constructor(readonly tracker: ContextTracker<any>, readonly context: any) {
    this.hash = tracker.hash(context)
  }
}

export const enum Recover {
  Token = 200,
  Reduce = 100,
  MaxNext = 4,
  MaxInsertStackDepth = 300,
  DampenInsertStackDepth = 120
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
    let goto = this.stack.p.parser.getGoto(this.rest[this.offset - 3], term, true)
    this.top = goto
  }
}

// This is given to `Tree.build` to build a buffer, and encapsulates
// the parent-stack-walking necessary to read the nodes.
export class StackBufferCursor implements BufferCursor {
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
