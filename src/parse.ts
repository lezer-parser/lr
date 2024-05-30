import {DefaultBufferLength, Tree, TreeFragment, NodeSet, NodeType, NodeProp, NodePropSource,
        Input, PartialParse, Parser, ParseWrapper, IterMode} from "@lezer/common"
import {Stack, StackBufferCursor, Lookahead} from "./stack"
import {Action, Specialize, Term, Seq, StateFlag, ParseState, File} from "./constants"
import {Tokenizer, TokenGroup, ExternalTokenizer, CachedToken, InputStream} from "./token"
import {decodeArray} from "./decode"

// Environment variable used to control console output
const verbose = typeof process != "undefined" && process.env && /\bparse\b/.test(process.env.LOG!)

let stackIDs: WeakMap<Stack, string> | null = null

function cutAt(tree: Tree, pos: number, side: 1 | -1) {
  let cursor = tree.cursor(IterMode.IncludeAnonymous)
  cursor.moveTo(pos)
  for (;;) {
    if (!(side < 0 ? cursor.childBefore(pos) : cursor.childAfter(pos))) for (;;) {
      if ((side < 0 ? cursor.to < pos : cursor.from > pos) && !cursor.type.isError)
        return side < 0 ? Math.max(0, Math.min(cursor.to - 1, pos - Lookahead.Margin))
          : Math.min(tree.length, Math.max(cursor.from + 1, pos + Lookahead.Margin))
      if (side < 0 ? cursor.prevSibling() : cursor.nextSibling()) break
      if (!cursor.parent()) return side < 0 ? 0 : tree.length
    }
  }
}

class FragmentCursor {
  i = 0
  fragment: TreeFragment | null = null
  safeFrom = -1
  safeTo = -1
  trees: Tree[] = []
  start: number[] = []
  index: number[] = []
  nextStart!: number

  constructor(readonly fragments: readonly TreeFragment[],
              readonly nodeSet: NodeSet) {
    this.nextFragment()
  }

  nextFragment() {
    let fr = this.fragment = this.i == this.fragments.length ? null : this.fragments[this.i++]
    if (fr) {
      this.safeFrom = fr.openStart ? cutAt(fr.tree, fr.from + fr.offset, 1) - fr.offset : fr.from
      this.safeTo = fr.openEnd ? cutAt(fr.tree, fr.to + fr.offset, -1) - fr.offset : fr.to
      while (this.trees.length) { this.trees.pop(); this.start.pop(); this.index.pop() }
      this.trees.push(fr.tree)
      this.start.push(-fr.offset)
      this.index.push(0)
      this.nextStart = this.safeFrom
    } else {
      this.nextStart = 1e9
    }
  }

  // `pos` must be >= any previously given `pos` for this cursor
  nodeAt(pos: number): Tree | null {
    if (pos < this.nextStart) return null
    while (this.fragment && this.safeTo <= pos) this.nextFragment()
    if (!this.fragment) return null

    for (;;) {
      let last = this.trees.length - 1
      if (last < 0) { // End of tree
        this.nextFragment()
        return null
      }
      let top = this.trees[last], index = this.index[last]
      if (index == top.children.length) {
        this.trees.pop()
        this.start.pop()
        this.index.pop()
        continue
      }
      let next = top.children[index]
      let start = this.start[last] + top.positions[index]
      if (start > pos) {
        this.nextStart = start
        return null
      }
      if (next instanceof Tree) {
        if (start == pos) {
          if (start < this.safeFrom) return null
          let end = start + next.length
          if (end <= this.safeTo) {
            let lookAhead = next.prop(NodeProp.lookAhead)
            if (!lookAhead || end + lookAhead < this.fragment.to) return next
          }
        }
        this.index[last]++
        if (start + next.length >= Math.max(this.safeFrom, pos)) { // Enter this node
          this.trees.push(next)
          this.start.push(start)
          this.index.push(0)
        }
      } else {
        this.index[last]++
        this.nextStart = start + next.length
      }
    }
  }
}

class TokenCache {
  tokens: CachedToken[] = []
  mainToken: CachedToken | null = null

  actions: number[] = []

  constructor(parser: LRParser, readonly stream: InputStream) {
    this.tokens = parser.tokenizers.map(_ => new CachedToken)
  }

  getActions(stack: Stack) {
    let actionIndex = 0
    let main: CachedToken | null = null
    let {parser} = stack.p, {tokenizers} = parser

    let mask = parser.stateSlot(stack.state, ParseState.TokenizerMask)
    let context = stack.curContext ? stack.curContext.hash : 0
    let lookAhead = 0
    for (let i = 0; i < tokenizers.length; i++) {
      if (((1 << i) & mask) == 0) continue
      let tokenizer = tokenizers[i], token = this.tokens[i]
      if (main && !tokenizer.fallback) continue
      if (tokenizer.contextual || token.start != stack.pos || token.mask != mask || token.context != context) {
        this.updateCachedToken(token, tokenizer, stack)
        token.mask = mask
        token.context = context
      }
      if (token.lookAhead > token.end + Lookahead.Margin)
        lookAhead = Math.max(token.lookAhead, lookAhead)

      if (token.value != Term.Err) {
        let startIndex = actionIndex
        if (token.extended > -1) actionIndex = this.addActions(stack, token.extended, token.end, actionIndex)
        actionIndex = this.addActions(stack, token.value, token.end, actionIndex)
        if (!tokenizer.extend) {
          main = token
          if (actionIndex > startIndex) break
        }
      }
    }

    while (this.actions.length > actionIndex) this.actions.pop()
    if (lookAhead) stack.setLookAhead(lookAhead)
    if (!main && stack.pos == this.stream.end) {
      main = new CachedToken
      main.value = stack.p.parser.eofTerm
      main.start = main.end = stack.pos
      actionIndex = this.addActions(stack, main.value, main.end, actionIndex)
    }
    this.mainToken = main
    return this.actions
  }

  getMainToken(stack: Stack) {
    if (this.mainToken) return this.mainToken
    let main = new CachedToken, {pos, p} = stack
    main.start = pos
    main.end = Math.min(pos + 1, p.stream.end)
    main.value = pos == p.stream.end ? p.parser.eofTerm : Term.Err
    return main
  }

  updateCachedToken(token: CachedToken, tokenizer: Tokenizer, stack: Stack) {
    let start = this.stream.clipPos(stack.pos)
    tokenizer.token(this.stream.reset(start, token), stack)
    if (token.value > -1) {
      let {parser} = stack.p

      for (let i = 0; i < parser.specialized.length; i++) if (parser.specialized[i] == token.value) {
        let result = parser.specializers[i](this.stream.read(token.start, token.end), stack)
        if (result >= 0 && stack.p.parser.dialect.allows(result >> 1)) {
          if ((result & 1) == Specialize.Specialize) token.value = result >> 1
          else token.extended = result >> 1
          break
        }
      }
    } else {
      token.value = Term.Err
      token.end = this.stream.clipPos(start + 1)
    }
  }

  putAction(action: number, token: number, end: number, index: number) {
    // Don't add duplicate actions
    for (let i = 0; i < index; i += 3) if (this.actions[i] == action) return index
    this.actions[index++] = action
    this.actions[index++] = token
    this.actions[index++] = end
    return index
  }

  addActions(stack: Stack, token: number, end: number, index: number) {
    let {state} = stack, {parser} = stack.p, {data} = parser
    for (let set = 0; set < 2; set++) {
      for (let i = parser.stateSlot(state, set ? ParseState.Skip : ParseState.Actions);; i += 3) {
        if (data[i] == Seq.End) {
          if (data[i + 1] == Seq.Next) {
            i = pair(data, i + 2)
          } else {
            if (index == 0 && data[i + 1] == Seq.Other)
              index = this.putAction(pair(data, i + 2), token, end, index)
            break
          }
        }
        if (data[i] == token) index = this.putAction(pair(data, i + 1), token, end, index)
      }
    }
    return index
  }
}

const enum Rec {
  Distance = 5,
  MaxRemainingPerStep = 3,
  // When two stacks have been running independently long enough to
  // add this many elements to their buffers, prune one.
  MinBufferLengthPrune = 500,
  ForceReduceLimit = 10,
  // Once a stack reaches this depth (in .stack.length) force-reduce
  // it back to CutTo to avoid creating trees that overflow the stack
  // on recursive traversal.
  CutDepth = 2800 * 3,
  CutTo = 2000 * 3,
  MaxLeftAssociativeReductionCount = 300,
  // The maximum number of non-recovering stacks to explore (to avoid
  // getting bogged down with exponentially multiplying stacks in
  // ambiguous content)
  MaxStackCount = 12
}

export class Parse implements PartialParse {
  // Active parse stacks.
  stacks: Stack[]
  recovering = 0
  fragments: FragmentCursor | null
  nextStackID = 0x2654 // ♔, ♕, ♖, ♗, ♘, ♙, ♠, ♡, ♢, ♣, ♤, ♥, ♦, ♧
  minStackPos = 0

  reused: Tree[] = []
  stream: InputStream
  tokens: TokenCache
  topTerm: number
  stoppedAt: null | number = null

  lastBigReductionStart = -1
  lastBigReductionSize = 0
  bigReductionCount = 0

  constructor(
    readonly parser: LRParser,
    readonly input: Input,
    fragments: readonly TreeFragment[],
    readonly ranges: readonly {from: number, to: number}[]
  ) {
    this.stream = new InputStream(input, ranges)
    this.tokens = new TokenCache(parser, this.stream)
    this.topTerm = parser.top[1]
    let {from} = ranges[0]
    this.stacks = [Stack.start(this, parser.top[0], from)]
    this.fragments = fragments.length && this.stream.end - from > parser.bufferLength * 4
      ? new FragmentCursor(fragments, parser.nodeSet) : null
  }

  get parsedPos() {
    return this.minStackPos
  }

  // Move the parser forward. This will process all parse stacks at
  // `this.pos` and try to advance them to a further position. If no
  // stack for such a position is found, it'll start error-recovery.
  //
  // When the parse is finished, this will return a syntax tree. When
  // not, it returns `null`.
  advance() {
    let stacks = this.stacks, pos = this.minStackPos
    // This will hold stacks beyond `pos`.
    let newStacks: Stack[] = this.stacks = []
    let stopped: Stack[] | undefined, stoppedTokens: number[] | undefined

    // If a large amount of reductions happened with the same start
    // position, force the stack out of that production in order to
    // avoid creating a tree too deep to recurse through.
    // (This is an ugly kludge, because unfortunately there is no
    // straightforward, cheap way to check for this happening, due to
    // the history of reductions only being available in an
    // expensive-to-access format in the stack buffers.)
    if (this.bigReductionCount > Rec.MaxLeftAssociativeReductionCount && stacks.length == 1) {
      let [s] = stacks
      while (s.forceReduce() && s.stack.length && s.stack[s.stack.length - 2] >= this.lastBigReductionStart) {}
      this.bigReductionCount = this.lastBigReductionSize = 0
    }

    // Keep advancing any stacks at `pos` until they either move
    // forward or can't be advanced. Gather stacks that can't be
    // advanced further in `stopped`.
    for (let i = 0; i < stacks.length; i++) {
      let stack = stacks[i]
      for (;;) {
        this.tokens.mainToken = null
        if (stack.pos > pos) {
          newStacks.push(stack)
        } else if (this.advanceStack(stack, newStacks, stacks)) {
          continue
        } else {
          if (!stopped) { stopped = []; stoppedTokens = [] }
          stopped.push(stack)
          let tok = this.tokens.getMainToken(stack)
          stoppedTokens!.push(tok.value, tok.end)
        }
        break
      }
    }

    if (!newStacks.length) {
      let finished = stopped && findFinished(stopped)
      if (finished) {
        if (verbose) console.log("Finish with " + this.stackID(finished))
        return this.stackToTree(finished)
      }

      if (this.parser.strict) {
        if (verbose && stopped)
          console.log("Stuck with token " + (this.tokens.mainToken ? this.parser.getName(this.tokens.mainToken.value) : "none"))
        throw new SyntaxError("No parse at " + pos)
      }
      if (!this.recovering) this.recovering = Rec.Distance
    }

    if (this.recovering && stopped) {
      let finished = this.stoppedAt != null && stopped[0].pos > this.stoppedAt ? stopped[0]
        : this.runRecovery(stopped, stoppedTokens!, newStacks)
      if (finished) {
        if (verbose) console.log("Force-finish " + this.stackID(finished))
        return this.stackToTree(finished.forceAll())
      }
    }

    if (this.recovering) {
      let maxRemaining = this.recovering == 1 ? 1 : this.recovering * Rec.MaxRemainingPerStep
      if (newStacks.length > maxRemaining) {
        newStacks.sort((a, b) => b.score - a.score)
        while (newStacks.length > maxRemaining) newStacks.pop()
      }
      if (newStacks.some(s => s.reducePos > pos)) this.recovering--
    } else if (newStacks.length > 1) {
      // Prune stacks that are in the same state, or that have been
      // running without splitting for a while, to avoid getting stuck
      // with multiple successful stacks running endlessly on.
      outer: for (let i = 0; i < newStacks.length - 1; i++) {
        let stack = newStacks[i]
        for (let j = i + 1; j < newStacks.length; j++) {
          let other = newStacks[j]
          if (stack.sameState(other) ||
              stack.buffer.length > Rec.MinBufferLengthPrune && other.buffer.length > Rec.MinBufferLengthPrune) {
            if (((stack.score - other.score) || (stack.buffer.length - other.buffer.length)) > 0) {
              newStacks.splice(j--, 1)
            } else {
              newStacks.splice(i--, 1)
              continue outer
            }
          }
        }
      }
      if (newStacks.length > Rec.MaxStackCount)
        newStacks.splice(Rec.MaxStackCount, newStacks.length - Rec.MaxStackCount)
    }

    this.minStackPos = newStacks[0].pos
    for (let i = 1; i < newStacks.length; i++) if (newStacks[i].pos < this.minStackPos) this.minStackPos = newStacks[i].pos
    return null
  }

  stopAt(pos: number) {
    if (this.stoppedAt != null && this.stoppedAt < pos) throw new RangeError("Can't move stoppedAt forward")
    this.stoppedAt = pos
  }

  // Returns an updated version of the given stack, or null if the
  // stack can't advance normally. When `split` and `stacks` are
  // given, stacks split off by ambiguous operations will be pushed to
  // `split`, or added to `stacks` if they move `pos` forward.
  private advanceStack(stack: Stack, stacks: null | Stack[], split: null | Stack[]) {
    let start = stack.pos, {parser} = this
    let base = verbose ? this.stackID(stack) + " -> " : ""

    if (this.stoppedAt != null && start > this.stoppedAt)
      return stack.forceReduce() ? stack : null

    if (this.fragments) {
      let strictCx = stack.curContext && stack.curContext.tracker.strict, cxHash = strictCx ? stack.curContext!.hash : 0
      for (let cached = this.fragments.nodeAt(start); cached;) {
        let match = this.parser.nodeSet.types[cached.type.id] == cached.type ? parser.getGoto(stack.state, cached.type.id) : -1
        if (match > -1 && cached.length && (!strictCx || (cached.prop(NodeProp.contextHash) || 0) == cxHash)) {
          stack.useNode(cached, match)
          if (verbose) console.log(base + this.stackID(stack) + ` (via reuse of ${parser.getName(cached.type.id)})`)
          return true
        }
        if (!(cached instanceof Tree) || cached.children.length == 0 || cached.positions[0] > 0) break
        let inner = cached.children[0]
        if (inner instanceof Tree && cached.positions[0] == 0) cached = inner
        else break
      }
    }

    let defaultReduce = parser.stateSlot(stack.state, ParseState.DefaultReduce)
    if (defaultReduce > 0) {
      stack.reduce(defaultReduce)
      if (verbose)
        console.log(base + this.stackID(stack) + ` (via always-reduce ${parser.getName(defaultReduce & Action.ValueMask)})`)
      return true
    }

    if (stack.stack.length >= Rec.CutDepth) {
      while (stack.stack.length > Rec.CutTo && stack.forceReduce()) {}
    }

    let actions = this.tokens.getActions(stack)
    for (let i = 0; i < actions.length;) {
      let action = actions[i++], term = actions[i++], end = actions[i++]
      let last = i == actions.length || !split
      let localStack = last ? stack : stack.split()
      let main = this.tokens.mainToken
      localStack.apply(action, term, main ? main.start : localStack.pos, end)
      if (verbose)
        console.log(base + this.stackID(localStack) + ` (via ${(action & Action.ReduceFlag) == 0 ? "shift"
                     : `reduce of ${parser.getName(action & Action.ValueMask)}`} for ${
        parser.getName(term)} @ ${start}${localStack == stack ? "" : ", split"})`)
      if (last) return true
      else if (localStack.pos > start) stacks!.push(localStack)
      else split!.push(localStack)
    }

    return false
  }

  // Advance a given stack forward as far as it will go. Returns the
  // (possibly updated) stack if it got stuck, or null if it moved
  // forward and was given to `pushStackDedup`.
  private advanceFully(stack: Stack, newStacks: Stack[]) {
    let pos = stack.pos
    for (;;) {
      if (!this.advanceStack(stack, null, null)) return false
      if (stack.pos > pos) {
        pushStackDedup(stack, newStacks)
        return true
      }
    }
  }

  private runRecovery(stacks: Stack[], tokens: number[], newStacks: Stack[]) {
    let finished: Stack | null = null, restarted = false
    for (let i = 0; i < stacks.length; i++) {
      let stack = stacks[i], token = tokens[i << 1], tokenEnd = tokens[(i << 1) + 1]
      let base = verbose ? this.stackID(stack) + " -> " : ""

      if (stack.deadEnd) {
        if (restarted) continue
        restarted = true
        stack.restart()
        if (verbose) console.log(base + this.stackID(stack) + " (restarted)")
        let done = this.advanceFully(stack, newStacks)
        if (done) continue
      }

      let force = stack.split(), forceBase = base
      for (let j = 0; force.forceReduce() && j < Rec.ForceReduceLimit; j++) {
        if (verbose) console.log(forceBase + this.stackID(force) + " (via force-reduce)")
        let done = this.advanceFully(force, newStacks)
        if (done) break
        if (verbose) forceBase = this.stackID(force) + " -> "
      }

      for (let insert of stack.recoverByInsert(token)) {
        if (verbose) console.log(base + this.stackID(insert) + " (via recover-insert)")
        this.advanceFully(insert, newStacks)
      }

      if (this.stream.end > stack.pos) {
        if (tokenEnd == stack.pos) {
          tokenEnd++
          token = Term.Err
        }
        stack.recoverByDelete(token, tokenEnd)
        if (verbose) console.log(base + this.stackID(stack) + ` (via recover-delete ${this.parser.getName(token)})`)
        pushStackDedup(stack, newStacks)
      } else if (!finished || finished.score < stack.score) {
        finished = stack
      }
    }

    return finished
  }

  // Convert the stack's buffer to a syntax tree.
  stackToTree(stack: Stack): Tree {
    stack.close()
    return Tree.build({buffer: StackBufferCursor.create(stack),
                       nodeSet: this.parser.nodeSet,
                       topID: this.topTerm,
                       maxBufferLength: this.parser.bufferLength,
                       reused: this.reused,
                       start: this.ranges[0].from,
                       length: stack.pos - this.ranges[0].from,
                       minRepeatType: this.parser.minRepeatTerm})
  }

  private stackID(stack: Stack) {
    let id = (stackIDs || (stackIDs = new WeakMap)).get(stack)
    if (!id) stackIDs.set(stack, id = String.fromCodePoint(this.nextStackID++))
    return id + stack
  }
}

function pushStackDedup(stack: Stack, newStacks: Stack[]) {
  for (let i = 0; i < newStacks.length; i++) {
    let other = newStacks[i]
    if (other.pos == stack.pos && other.sameState(stack)) {
      if (newStacks[i].score < stack.score) newStacks[i] = stack
      return
    }
  }
  newStacks.push(stack)
}

export class Dialect {
  constructor(readonly source: string | undefined,
              readonly flags: readonly boolean[],
              readonly disabled: null | Uint8Array) {}

  allows(term: number) { return !this.disabled || this.disabled[term] == 0 }
}

const id: <T>(x: T) => T = x => x

/// Context trackers are used to track stateful context (such as
/// indentation in the Python grammar, or parent elements in the XML
/// grammar) needed by external tokenizers. You declare them in a
/// grammar file as `@context exportName from "module"`.
///
/// Context values should be immutable, and can be updated (replaced)
/// on shift or reduce actions.
///
/// The export used in a `@context` declaration should be of this
/// type.
export class ContextTracker<T> {
  /// @internal
  start: T
  /// @internal
  shift: (context: T, term: number, stack: Stack, input: InputStream) => T
  /// @internal
  reduce: (context: T, term: number, stack: Stack, input: InputStream) => T
  /// @internal
  reuse: (context: T, node: Tree, stack: Stack, input: InputStream) => T
  /// @internal
  hash: (context: T) => number
  /// @internal
  strict: boolean

  /// Define a context tracker.
  constructor(spec: {
    /// The initial value of the context at the start of the parse.
    start: T,
    /// Update the context when the parser executes a
    /// [shift](https://en.wikipedia.org/wiki/LR_parser#Shift_and_reduce_actions)
    /// action.
    shift?(context: T, term: number, stack: Stack, input: InputStream): T
    /// Update the context when the parser executes a reduce action.
    reduce?(context: T, term: number, stack: Stack, input: InputStream): T
    /// Update the context when the parser reuses a node from a tree
    /// fragment.
    reuse?(context: T, node: Tree, stack: Stack, input: InputStream): T
    /// Reduce a context value to a number (for cheap storage and
    /// comparison). Only needed for strict contexts.
    hash?(context: T): number
    /// By default, nodes can only be reused during incremental
    /// parsing if they were created in the same context as the one in
    /// which they are reused. Set this to false to disable that
    /// check (and the overhead of storing the hashes).
    strict?: boolean
  }) {
    this.start = spec.start
    this.shift = spec.shift || id
    this.reduce = spec.reduce || id
    this.reuse = spec.reuse || id
    this.hash = spec.hash || (() => 0)
    this.strict = spec.strict !== false
  }
}

type SpecializerSpec = {
  term: number,
  get?: (value: string, stack: Stack) => number,
  external?: any,
  extend?: boolean
}

type ParserSpec = {
  version: number,
  states: string | Uint32Array,
  stateData: string | Uint16Array,
  goto: string | Uint16Array,
  nodeNames: string,
  maxTerm: number,
  repeatNodeCount: number,
  nodeProps?: [NodeProp<any> | string, ...(string | number)[]][],
  propSources?: NodePropSource[],
  skippedNodes?: number[],
  tokenData: string,
  tokenizers: (Tokenizer | number)[],
  topRules: {[name: string]: [number, number]},
  context: ContextTracker<any> | null,
  dialects?: {[name: string]: number},
  dynamicPrecedences?: {[term: number]: number},
  specialized?: SpecializerSpec[],
  tokenPrec: number,
  termNames?: {[id: number]: string}
}

/// Configuration options when
/// [reconfiguring](#lr.LRParser.configure) a parser.
export interface ParserConfig {
  /// Node prop values to add to the parser's node set.
  props?: readonly NodePropSource[]
  /// The name of the `@top` declaration to parse from. If not
  /// specified, the first top rule declaration in the grammar is
  /// used.
  top?: string
  /// A space-separated string of dialects to enable.
  dialect?: string
  /// Replace the given external tokenizers with new ones.
  tokenizers?: {from: ExternalTokenizer, to: ExternalTokenizer}[]
  /// Replace external specializers with new ones.
  specializers?: {from: (value: string, stack: Stack) => number, to: (value: string, stack: Stack) => number}[],
  /// Replace the context tracker with a new one.
  contextTracker?: ContextTracker<any>,
  /// When true, the parser will raise an exception, rather than run
  /// its error-recovery strategies, when the input doesn't match the
  /// grammar.
  strict?: boolean
  /// Add a wrapper, which can extend parses created by this parser
  /// with additional logic (usually used to add
  /// [mixed-language](#common.parseMixed) parsing).
  wrap?: ParseWrapper
  /// The maximum length of the TreeBuffers generated in the output
  /// tree. Defaults to 1024.
  bufferLength?: number
}

/// Holds the parse tables for a given grammar, as generated by
/// `lezer-generator`, and provides [methods](#common.Parser) to parse
/// content with.
export class LRParser extends Parser {
  /// The parse states for this grammar @internal
  readonly states: Readonly<Uint32Array>
  /// A blob of data that the parse states, as well as some
  /// of `LRParser`'s fields, point into @internal
  readonly data: Readonly<Uint16Array>
  /// The goto table. See `computeGotoTable` in
  /// lezer-generator for details on the format @internal
  readonly goto: Readonly<Uint16Array>
  /// The highest term id @internal
  readonly maxTerm: number
  /// The first repeat-related term id @internal
  readonly minRepeatTerm: number
  /// The tokenizer objects used by the grammar @internal
  readonly tokenizers: readonly Tokenizer[]
  /// Maps top rule names to [state ID, top term ID] pairs. @internal
  readonly topRules: {[name: string]: [number, number]}
  /// @internal
  readonly context: ContextTracker<unknown> | null
  /// A mapping from dialect names to the tokens that are exclusive
  /// to them. @internal
  readonly dialects: {[name: string]: number}
  /// Null if there are no dynamic precedences, a map from term ids
  /// to precedence otherwise. @internal
  readonly dynamicPrecedences: {[term: number]: number} | null
  /// The token types have specializers (in this.specializers) @internal
  readonly specialized: Uint16Array
  /// The specializer functions for the token types in specialized @internal
  readonly specializers: ((value: string, stack: Stack) => number)[]
  /// FIXME @internal
  readonly specializerSpecs: SpecializerSpec[]
  /// Points into this.data at an array that holds the
  /// precedence order (higher precedence first) for ambiguous
  /// tokens @internal
  readonly tokenPrecTable: number
  /// An optional object mapping term ids to name strings @internal
  readonly termNames: null | {[id: number]: string}
  /// @internal
  readonly maxNode: number
  /// @internal
  readonly dialect: Dialect
  /// @internal
  readonly wrappers: readonly ParseWrapper[] = []
  /// @internal
  readonly top: [number, number]
  /// @internal
  readonly bufferLength: number
  /// @internal
  readonly strict: boolean
  /// The nodes used in the trees emitted by this parser.
  readonly nodeSet: NodeSet

  /// @internal
  constructor(spec: ParserSpec) {
    super()
    if (spec.version != File.Version)
      throw new RangeError(`Parser version (${spec.version}) doesn't match runtime version (${File.Version})`)
    let nodeNames = spec.nodeNames.split(" ")
    this.minRepeatTerm = nodeNames.length
    for (let i = 0; i < spec.repeatNodeCount; i++) nodeNames.push("")
    let topTerms = Object.keys(spec.topRules).map(r => spec.topRules[r][1])
    let nodeProps: [NodeProp<any>, any][][] = []
    for (let i = 0; i < nodeNames.length; i++) nodeProps.push([])
    function setProp(nodeID: number, prop: NodeProp<any>, value: any) {
      nodeProps[nodeID].push([prop, prop.deserialize(String(value))])
    }
    if (spec.nodeProps) for (let propSpec of spec.nodeProps) {
      let prop = propSpec[0]
      if (typeof prop == "string") prop = (NodeProp as unknown as {[name: string]: NodeProp<any>})[prop]
      for (let i = 1; i < propSpec.length;) {
        let next = propSpec[i++] as number
        if (next >= 0) {
          setProp(next as number, prop, propSpec[i++] as string)
        } else {
          let value = propSpec[i + -next] as string
          for (let j = -next; j > 0; j--) setProp(propSpec[i++] as number, prop, value)
          i++
        }
      }
    }
    this.nodeSet = new NodeSet(nodeNames.map((name, i) => NodeType.define({
      name: i >= this.minRepeatTerm ? undefined: name,
      id: i,
      props: nodeProps[i],
      top: topTerms.indexOf(i) > -1,
      error: i == 0,
      skipped: spec.skippedNodes && spec.skippedNodes.indexOf(i) > -1
    })))
    if (spec.propSources) this.nodeSet = this.nodeSet.extend(...spec.propSources)
    this.strict = false
    this.bufferLength = DefaultBufferLength

    let tokenArray = decodeArray<Uint16Array>(spec.tokenData)
    this.context = spec.context
    this.specializerSpecs = spec.specialized || []
    this.specialized = new Uint16Array(this.specializerSpecs.length)
    for (let i = 0; i < this.specializerSpecs.length; i++) this.specialized[i] = this.specializerSpecs[i].term
    this.specializers = this.specializerSpecs.map(getSpecializer)

    this.states = decodeArray(spec.states, Uint32Array)
    this.data = decodeArray(spec.stateData)
    this.goto = decodeArray(spec.goto)
    this.maxTerm = spec.maxTerm
    this.tokenizers = spec.tokenizers.map(value => typeof value == "number" ? new TokenGroup(tokenArray, value) : value)
    this.topRules = spec.topRules
    this.dialects = spec.dialects || {}
    this.dynamicPrecedences = spec.dynamicPrecedences || null
    this.tokenPrecTable = spec.tokenPrec
    this.termNames = spec.termNames || null
    this.maxNode = this.nodeSet.types.length - 1

    this.dialect = this.parseDialect()
    this.top = this.topRules[Object.keys(this.topRules)[0]]
  }

  createParse(input: Input, fragments: readonly TreeFragment[], ranges: readonly {from: number, to: number}[]): PartialParse {
    let parse: PartialParse = new Parse(this, input, fragments, ranges)
    for (let w of this.wrappers) parse = w(parse, input, fragments, ranges)
    return parse
  }

  /// Get a goto table entry @internal
  getGoto(state: number, term: number, loose = false) {
    let table = this.goto
    if (term >= table[0]) return -1
    for (let pos = table[term + 1];;) {
      let groupTag = table[pos++], last = groupTag & 1
      let target = table[pos++]
      if (last && loose) return target
      for (let end = pos + (groupTag >> 1); pos < end; pos++)
        if (table[pos] == state) return target
      if (last) return -1
    }
  }

  /// Check if this state has an action for a given terminal @internal
  hasAction(state: number, terminal: number) {
    let data = this.data
    for (let set = 0; set < 2; set++) {
      for (let i = this.stateSlot(state, set ? ParseState.Skip : ParseState.Actions), next;; i += 3) {
        if ((next = data[i]) == Seq.End) {
          if (data[i + 1] == Seq.Next) next = data[i = pair(data, i + 2)]
          else if (data[i + 1] == Seq.Other) return pair(data, i + 2)
          else break
        }
        if (next == terminal || next == Term.Err) return pair(data, i + 1)
      }
    }
    return 0
  }

  /// @internal
  stateSlot(state: number, slot: number) {
    return this.states[(state * ParseState.Size) + slot]
  }

  /// @internal
  stateFlag(state: number, flag: number) {
    return (this.stateSlot(state, ParseState.Flags) & flag) > 0
  }

  /// @internal
  validAction(state: number, action: number) {
    return !!this.allActions(state, a => a == action ? true : null)
  }

  /// @internal
  allActions<T>(state: number, action: (action: number) => void | T): void | T {
    let deflt = this.stateSlot(state, ParseState.DefaultReduce)
    let result: void | T = deflt ? action(deflt) : undefined
    for (let i = this.stateSlot(state, ParseState.Actions); result == null; i += 3) {
      if (this.data[i] == Seq.End) {
        if (this.data[i + 1] == Seq.Next) i = pair(this.data, i + 2)
        else break
      }
      result = action(pair(this.data, i + 1))
    }
    return result
  }

  /// Get the states that can follow this one through shift actions or
  /// goto jumps. @internal
  nextStates(state: number): readonly number[] {
    let result: number[] = []
    for (let i = this.stateSlot(state, ParseState.Actions);; i += 3) {
      if (this.data[i] == Seq.End) {
        if (this.data[i + 1] == Seq.Next) i = pair(this.data, i + 2)
        else break
      }
      if ((this.data[i + 2] & (Action.ReduceFlag >> 16)) == 0) {
        let value = this.data[i + 1]
        if (!result.some((v, i) => (i & 1) && v == value)) result.push(this.data[i], value)
      }
    }
    return result
  }

  /// Configure the parser. Returns a new parser instance that has the
  /// given settings modified. Settings not provided in `config` are
  /// kept from the original parser.
  configure(config: ParserConfig) {
    // Hideous reflection-based kludge to make it easy to create a
    // slightly modified copy of a parser.
    let copy = Object.assign(Object.create(LRParser.prototype), this)
    if (config.props)
      copy.nodeSet = this.nodeSet.extend(...config.props)
    if (config.top) {
      let info = this.topRules[config.top!]
      if (!info) throw new RangeError(`Invalid top rule name ${config.top}`)
      copy.top = info
    }
    if (config.tokenizers)
      copy.tokenizers = this.tokenizers.map(t => {
        let found = config.tokenizers!.find(r => r.from == t)
        return found ? found.to : t
      })
    if (config.specializers) {
      copy.specializers = this.specializers.slice()
      copy.specializerSpecs = this.specializerSpecs.map((s, i) => {
        let found = config.specializers!.find(r => r.from == s.external)
        if (!found) return s
        let spec = {...s, external: found.to}
        copy.specializers[i] = getSpecializer(spec)
        return spec
      })
    }
    if (config.contextTracker)
      copy.context = config.contextTracker
    if (config.dialect)
      copy.dialect = this.parseDialect(config.dialect)
    if (config.strict != null)
      copy.strict = config.strict
    if (config.wrap)
      copy.wrappers = copy.wrappers.concat(config.wrap)
    if (config.bufferLength != null)
      copy.bufferLength = config.bufferLength
    return copy as LRParser
  }

  /// Tells you whether any [parse wrappers](#lr.ParserConfig.wrap)
  /// are registered for this parser.
  hasWrappers() {
    return this.wrappers.length > 0
  }

  /// Returns the name associated with a given term. This will only
  /// work for all terms when the parser was generated with the
  /// `--names` option. By default, only the names of tagged terms are
  /// stored.
  getName(term: number): string {
    return this.termNames ? this.termNames[term] : String(term <= this.maxNode && this.nodeSet.types[term].name || term)
  }

  /// The eof term id is always allocated directly after the node
  /// types. @internal
  get eofTerm() { return this.maxNode + 1 }

  /// The type of top node produced by the parser.
  get topNode() { return this.nodeSet.types[this.top[1]] }

  /// @internal
  dynamicPrecedence(term: number) {
    let prec = this.dynamicPrecedences
    return prec == null ? 0 : prec[term] || 0
  }

  /// @internal
  parseDialect(dialect?: string) {
    let values = Object.keys(this.dialects), flags = values.map(() => false)
    if (dialect) for (let part of dialect.split(" ")) {
      let id = values.indexOf(part)
      if (id >= 0) flags[id] = true
    }
    let disabled: Uint8Array | null = null
    for (let i = 0; i < values.length; i++) if (!flags[i]) {
      for (let j = this.dialects[values[i]], id; (id = this.data[j++]) != Seq.End;)
        (disabled || (disabled = new Uint8Array(this.maxTerm + 1)))[id] = 1
    }
    return new Dialect(dialect, flags, disabled)
  }

  /// Used by the output of the parser generator. Not available to
  /// user code. @hide
  static deserialize(spec: any): LRParser {
    return new LRParser(spec as ParserSpec)
  }
}

function pair(data: Readonly<Uint16Array>, off: number) { return data[off] | (data[off + 1] << 16) }

function findFinished(stacks: Stack[]) {
  let best: Stack | null = null
  for (let stack of stacks) {
    let stopped = stack.p.stoppedAt
    if ((stack.pos == stack.p.stream.end || stopped != null && stack.pos > stopped) &&
        stack.p.parser.stateFlag(stack.state, StateFlag.Accepting) &&
        (!best || best.score < stack.score))
      best = stack
  }
  return best
}

function getSpecializer(spec: SpecializerSpec) {
  if (spec.external) {
    let mask = spec.extend ? Specialize.Extend : Specialize.Specialize
    return (value: string, stack: Stack) => (spec.external!(value, stack) << 1) | mask
  }
  return spec.get!
}
