import {Stack, Recover} from "./stack"
import {Action, Specialize, Term, Seq, StateFlag, ParseState, File} from "./constants"
import {Input, Token, stringInput, Tokenizer, TokenGroup, ExternalTokenizer} from "./token"
import {DefaultBufferLength, Tree, TreeBuffer, TreeFragment, NodeSet, NodeType, NodeProp, NodePropSource} from "lezer-tree"
import {decodeArray} from "./decode"

// Environment variable used to control console output
const verbose = typeof process != "undefined" && /\bparse\b/.test(process.env.LOG!)

let stackIDs: WeakMap<Stack, string> | null = null

/// Interface that a parser that is used as a nested incremental
/// parser must conform to.
export interface IncrementalParse {
  /// Advance the parse state by some amount.
  advance(): Tree | null
  /// The current parse position.
  pos: number
  /// Get the currently parsed content as a tree, even though the
  /// parse hasn't finished yet.
  forceFinish(): Tree
}

/// Generic interface for parsers.
export interface IncrementalParser {
  /// Start a parse.
  startParse(input: Input, options?: {
    /// The position to start parsing at. Defaults to 0. The returned
    /// tree should start at this position.
    startPos?: number,
    /// Fragments to reuse, if any.
    fragments?: readonly TreeFragment[]
  }): IncrementalParse
}

/// Used to configure a [nested parse](#lezer.Parser.withNested).
export type NestedParserSpec = {
  /// The inner parser. Will be passed the input,
  /// [clipped](#lezer.Input.clip) to the size of the parseable
  /// region, the start position of the inner region as `startPos`,
  /// and an optional array of tree fragments from a previous parse
  /// that can be reused.
  ///
  /// When this property isn't given, the inner region is simply
  /// skipped over intead of parsed.
  parser?: IncrementalParser
  /// When given, an additional node will be wrapped around the
  /// part of the tree produced by this inner parse.
  wrapType?: NodeType | number
  /// When given, this will be called with the token that ends the
  /// inner region. It can return `false` to cause a given end token
  /// to be ignored.
  filterEnd?(endToken: string): boolean
}

/// This type is used to specify a nested parser. It may directly be a
/// nested parse [spec](#lezer.NestedParseSpec), or a function that,
/// given an input document and a stack, returns such a spec or `null`
/// to indicate that the nested parse should not happen (and the
/// grammar's fallback expression should be used).
export type NestedParser = NestedParserSpec | ((input: Input, stack: Stack) => NestedParserSpec | null)

class FragmentCursor {
  i = 0
  fragment: TreeFragment | null = null
  trees: Tree[] = []
  start: number[] = []
  index: number[] = []
  nextStart!: number

  constructor(readonly fragments: readonly TreeFragment[]) {
    this.nextFragment()
  }

  nextFragment() {
    this.fragment = this.i == this.fragments.length ? null : this.fragments[this.i++]
    if (this.fragment) {
      while (this.trees.length) { this.trees.pop(); this.start.pop(); this.index.pop() }
      this.trees.push(this.fragment.tree)
      this.start.push(-this.fragment.offset)
      this.index.push(0)
      this.nextStart = this.fragment.safeFrom
    } else {
      this.nextStart = 1e9
    }
  }

  // `pos` must be >= any previously given `pos` for this cursor
  nodeAt(pos: number): Tree | TreeBuffer | null {
    if (pos < this.nextStart) return null
    while (this.fragment && this.fragment.safeTo <= pos) this.nextFragment()
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
      } else if (start == pos && start + next.length <= this.fragment.safeTo) {
        return start == pos && start >= this.fragment.safeFrom ? next : null
      }
      if (next instanceof TreeBuffer) {
        this.index[last]++
        this.nextStart = start + next.length
      } else {
        this.index[last]++
        if (start + next.length >= pos) { // Enter this node
          this.trees.push(next)
          this.start.push(start)
          this.index.push(0)
        }
      }
    }
  }
}

class CachedToken extends Token {
  extended = -1
  mask = 0

  clear(start: number) {
    this.start = start
    this.value = this.extended = -1
  }
}

const dummyToken = new Token

class TokenCache {
  tokens: CachedToken[] = []
  mainToken: Token = dummyToken

  actions: number[] = []

  constructor(parser: Parser) {
    this.tokens = parser.tokenizers.map(_ => new CachedToken)
  }

  getActions(stack: Stack, input: Input) {
    let actionIndex = 0
    let main: Token | null = null
    let {parser} = stack.cx, {tokenizers} = parser

    let mask = parser.stateSlot(stack.state, ParseState.TokenizerMask)
    for (let i = 0; i < tokenizers.length; i++) {
      if (((1 << i) & mask) == 0) continue
      let tokenizer = tokenizers[i], token = this.tokens[i]
      if (main && !tokenizer.fallback) continue
      if (tokenizer.contextual || token.start != stack.pos || token.mask != mask) {
        this.updateCachedToken(token, tokenizer, stack, input)
        token.mask = mask
      }

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
    if (!main) {
      main = dummyToken
      main.start = stack.pos
      if (stack.pos == input.length) main.accept(stack.cx.parser.eofTerm, stack.pos)
      else main.accept(Term.Err, stack.pos + 1)
    }
    this.mainToken = main
    return this.actions
  }

  updateCachedToken(token: CachedToken, tokenizer: Tokenizer, stack: Stack, input: Input) {
    token.clear(stack.pos)
    tokenizer.token(input, token, stack)
    if (token.value > -1) {
      let {parser} = stack.cx

      for (let i = 0; i < parser.specialized.length; i++) if (parser.specialized[i] == token.value) {
        let result = parser.specializers[i](input.read(token.start, token.end), stack)
        if (result >= 0 && stack.cx.parser.dialect.allows(result >> 1)) {
          if ((result & 1) == Specialize.Specialize) token.value = result >> 1
          else token.extended = result >> 1
          break
        }
      }
    } else if (stack.pos == input.length) {
      token.accept(stack.cx.parser.eofTerm, stack.pos)
    } else {
      token.accept(Term.Err, stack.pos + 1)
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
    let {state} = stack, {parser} = stack.cx, {data} = parser
    for (let set = 0; set < 2; set++) {
      for (let i = parser.stateSlot(state, set ? ParseState.Skip : ParseState.Actions);; i += 3) {
        if (data[i] == Seq.End) {
          if (data[i + 1] == Seq.Next) {
            i = pair(data, i + 2)
          } else {
            if (index == 0 && data[i + 1] == Seq.Other)
              index = this.putAction(pair(data, i + 1), token, end, index)
            break
          }
        }
        if (data[i] == token) index = this.putAction(pair(data, i + 1), token, end, index)
      }
    }
    return index
  }
}

/// Options that can be passed to control parsing.
export interface ParseOptions {
  /// Passing a set of fragments from a previous parse is used for
  /// incremental parsing. These should be aligned with the current
  /// document (though a call to
  /// [`TreeFragment.applyChanges`](#tree.TreeFragment^applyChanges))
  /// if any changes were made since they were produced. The parser
  /// will try to reuse nodes from the fragments in the new parse,
  /// greatly speeding up the parse when it can do so for most of the
  /// document.
  fragments?: readonly TreeFragment[],
  /// The input position to start parsing from.
  startPos?: number,
  /// When true, the parser will raise an exception, rather than run
  /// its error-recovery strategies, when the input doesn't match the
  /// grammar.
  strict?: boolean,
  /// The maximum length of the TreeBuffers generated in the output
  /// tree. Defaults to 1024.
  bufferLength?: number
}

const enum Rec {
  Distance = 5,
  MaxRemainingPerStep = 3,
  MinBufferLengthPrune = 200,
  ForceReduceLimit = 10
}

/// A parse context can be used for step-by-step parsing. After
/// creating it, you repeatedly call `.advance()` until it returns a
/// tree to indicate it has reached the end of the parse.
export class ParseContext implements IncrementalParse {
  // Active parse stacks.
  private stacks: Stack[]
  // The position to which the parse has advanced.
  public pos = 0
  private recovering = 0
  private tokenCount = 0
  private fragments: FragmentCursor | null
  private strict: boolean
  private nextStackID = 0x2654
  private nested: IncrementalParse | null = null
  private nestEnd = 0
  private nestWrap: NodeType | null = null

  public reused: (Tree | TreeBuffer)[] = []
  private tokens: TokenCache
  public maxBufferLength: number
  public topTerm: number
  public startPos: number

  constructor(
    public parser: Parser,
    public input: Input,
    options: ParseOptions = {}
  ) {
    let {fragments = undefined, strict = false, bufferLength = DefaultBufferLength} = options
    this.tokens = new TokenCache(parser)
    this.topTerm = parser.top[1]
    this.maxBufferLength = bufferLength
    this.startPos = options.startPos || 0
    this.stacks = [Stack.start(this, parser.top[0], this.startPos)]
    this.strict = strict
    this.fragments = fragments && fragments.length ? new FragmentCursor(fragments) : null
  }

  // Move the parser forward. This will process all parse stacks at
  // `this.pos` and try to advance them to a further position. If no
  // stack for such a position is found, it'll start error-recovery.
  //
  // When the parse is finished, this will return a syntax tree. When
  // not, it returns `null`.
  advance() {
    if (this.nested) {
      let result = this.nested.advance()
      this.pos = this.nested.pos
      if (result) {
        this.finishNested(this.stacks[0], result)
        this.nested = null
      }
      return null
    }

    let stacks = this.stacks, pos = this.pos
    // This will hold stacks beyond `pos`.
    let newStacks: Stack[] = this.stacks = []
    let stopped: Stack[] | undefined, stoppedTokens: number[] | undefined
    let maybeNest

    // Keep advancing any stacks at `pos` until they either move
    // forward or can't be advanced. Gather stacks that can't be
    // advanced further in `stopped`.
    for (let i = 0; i < stacks.length; i++) {
      let stack = stacks[i], nest
      for (;;) {
        if (stack.pos > pos) {
          newStacks.push(stack)
        } else if (nest = this.checkNest(stack)) {
          if (!maybeNest || maybeNest.stack.score < stack.score) maybeNest = nest
        } else if (this.advanceStack(stack, newStacks, stacks)) {
          continue
        } else {
          if (!stopped) { stopped = []; stoppedTokens = [] }
          stopped.push(stack)
          let tok = this.tokens.mainToken
          stoppedTokens!.push(tok.value, tok.end)
        }
        break
      }
    }

    if (maybeNest) {
      this.startNested(maybeNest)
      return null
    }

    if (!newStacks.length) {
      let finished = stopped && findFinished(stopped)
      if (finished) return finished.toTree()

      if (this.strict) {
        if (verbose && stopped)
          console.log("Stuck with token " + this.parser.getName(this.tokens.mainToken.value))
        throw new SyntaxError("No parse at " + pos)
      }
      if (!this.recovering) this.recovering = Rec.Distance
    }

    if (this.recovering && stopped) {
      let finished = this.runRecovery(stopped, stoppedTokens!, newStacks)
      if (finished) return finished.forceAll().toTree()
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
    }

    this.tokenCount++
    this.pos = newStacks[0].pos
    for (let i = 1; i < newStacks.length; i++) if (newStacks[i].pos < this.pos) this.pos = newStacks[i].pos
    return null
  }

  // Returns an updated version of the given stack, or null if the
  // stack can't advance normally. When `split` and `stacks` are
  // given, stacks split off by ambiguous operations will be pushed to
  // `split`, or added to `stacks` if they move `pos` forward.
  private advanceStack(stack: Stack, stacks: null | Stack[], split: null | Stack[]) {
    let start = stack.pos, {input, parser} = this
    let base = verbose ? this.stackID(stack) + " -> " : ""

    if (this.fragments) {
      for (let cached = this.fragments.nodeAt(start); cached;) {
        let match = this.parser.nodeSet.types[cached.type.id] == cached.type ? parser.getGoto(stack.state, cached.type.id) : -1
        if (match > -1 && cached.length) {
          stack.useNode(cached, match)
          if (verbose) console.log(base + this.stackID(stack) + ` (via reuse of ${parser.getName(cached.type.id)})`)
          return true
        }
        if (!(cached instanceof Tree) || cached.children.length == 0 || cached.positions[0] > 0) break
        let inner = cached.children[0]
        if (inner instanceof Tree) cached = inner
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

    let actions = this.tokens.getActions(stack, input)
    for (let i = 0; i < actions.length;) {
      let action = actions[i++], term = actions[i++], end = actions[i++]
      let last = i == actions.length || !split
      let localStack = last ? stack : stack.split()
      localStack.apply(action, term, end)
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
      let nest = this.checkNest(stack)
      if (nest) return nest
      if (!this.advanceStack(stack, null, null)) return false
      if (stack.pos > pos) {
        pushStackDedup(stack, newStacks)
        return true
      }
    }
  }

  private runRecovery(stacks: Stack[], tokens: number[], newStacks: Stack[]) {
    let finished: Stack | null = null, restarted = false
    let maybeNest
    for (let i = 0; i < stacks.length; i++) {
      let stack = stacks[i], token = tokens[i << 1], tokenEnd = tokens[(i << 1) + 1]
      let base = verbose ? this.stackID(stack) + " -> " : ""

      if (stack.deadEnd) {
        if (restarted) continue
        restarted = true
        stack.restart()
        if (verbose) console.log(base + this.stackID(stack) + " (restarted)")
        let done = this.advanceFully(stack, newStacks)
        if (done) {
          if (done !== true) maybeNest = done
          continue
        }
      }

      let force = stack.split(), forceBase = base
      for (let j = 0; force.forceReduce() && j < Rec.ForceReduceLimit; j++) {
        if (verbose) console.log(forceBase + this.stackID(force) + " (via force-reduce)")
        let done = this.advanceFully(force, newStacks)
        if (done) {
          if (done !== true) maybeNest = done
          break
        }
        if (verbose) forceBase = this.stackID(force) + " -> "
      }

      for (let insert of stack.recoverByInsert(token)) {
        if (verbose) console.log(base + this.stackID(insert) + " (via recover-insert)")
        this.advanceFully(insert, newStacks)
      }

      if (this.input.length > stack.pos) {
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

    if (finished) return finished

    if (maybeNest) for (let s of this.stacks) if (s.score > maybeNest.stack.score) {
      maybeNest = undefined
      break
    }
    if (maybeNest) this.startNested(maybeNest)
    return null
  }

  forceFinish() {
    let stack = this.stacks[0].split()
    if (this.nested) this.finishNested(stack, this.nested.forceFinish())
    return stack.forceAll().toTree()
  }

  // A value that indicates how successful the parse is so far, as
  // the number of error-recovery steps taken divided by the number
  // of tokens parsed. Could be used to decide to abort a parse when
  // the input doesn't appear to match the grammar at all.
  // FIXME replace this with automatic skip-ahead logic
  get badness() {
    if (!this.stacks.length) return 0
    return -(this.stacks[0].score / (Recover.Token * this.tokenCount))
  }

  private checkNest(stack: Stack) {
    let info = this.parser.findNested(stack.state)
    if (!info) return null
    let spec: NestedParser | null = info.value
    if (typeof spec == "function") spec = spec(this.input, stack)
    return spec ? {stack, info, spec} : null
  }

  private startNested(nest: {stack: Stack, info: NestInfo, spec: NestedParserSpec}) {
    let {stack, info, spec} = nest
    this.stacks = [stack]
    this.nestEnd = this.scanForNestEnd(stack, info.end, spec.filterEnd)
    this.nestWrap = typeof spec.wrapType == "number" ? this.parser.nodeSet.types[spec.wrapType] : spec.wrapType || null
    if (spec.parser) {
      this.nested = spec.parser.startParse(this.input.clip(this.nestEnd),
                                           {startPos: stack.pos, fragments: this.fragments?.fragments})
    } else {
      this.finishNested(stack)
    }
  }

  private scanForNestEnd(stack: Stack, endToken: TokenGroup, filter?: (token: string) => boolean) {
    for (let pos = stack.pos; pos < this.input.length; pos++) {
      dummyToken.start = pos
      dummyToken.value = -1
      endToken.token(this.input, dummyToken, stack)
      if (dummyToken.value > -1 && (!filter || filter(this.input.read(pos, dummyToken.end)))) return pos
    }
    return this.input.length
  }

  private finishNested(stack: Stack, tree?: Tree) {
    if (this.nestWrap) tree = new Tree(this.nestWrap, tree ? [tree] : [], tree ? [0] : [], this.nestEnd - stack.pos)
    else if (!tree) tree = new Tree(NodeType.none, [], [], this.nestEnd - stack.pos)
    let info = this.parser.findNested(stack.state)!
    stack.useNode(tree, this.parser.getGoto(stack.state, info.placeholder, true))
    if (verbose) console.log(this.stackID(stack) + ` (via unnest)`)
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

type ParserSpec = {
  version: number,
  states: string | Uint32Array,
  stateData: string | Uint16Array,
  goto: string | Uint16Array,
  nodeNames: string,
  maxTerm: number,
  repeatNodeCount: number,
  nodeProps?: [NodeProp<any>, ...(string | number)[]][],
  skippedNodes?: number[],
  tokenData: string,
  tokenizers: (Tokenizer | number)[],
  topRules: {[name: string]: [number, number]},
  nested?: [string, NestedParser, string | Uint16Array, number][],
  dialects?: {[name: string]: number},
  dynamicPrecedences?: {[term: number]: number},
  specialized?: {term: number, get: (value: string, stack: Stack) => number}[],
  tokenPrec: number,
  termNames?: {[id: number]: string}
}

type NestInfo = {
  // A name, used by `withNested`
  name: string,
  value: NestedParser,
  // A token-recognizing automaton for the end of the nesting
  end: TokenGroup,
  // The id of the placeholder term that appears in the grammar at
  // the position of this nesting
  placeholder: number
}

/// A parser holds the parse tables for a given grammar, as generated
/// by `lezer-generator`.
export class Parser implements IncrementalParser {
  /// The parse states for this grammar @internal
  readonly states: Readonly<Uint32Array>
  /// A blob of data that the parse states, as well as some
  /// of `Parser`'s fields, point into @internal
  readonly data: Readonly<Uint16Array>
  /// The goto table. See `computeGotoTable` in
  /// lezer-generator for details on the format @internal
  readonly goto: Readonly<Uint16Array>
  /// A node set with the node types used by this parser.
  readonly nodeSet: NodeSet
  /// The highest term id @internal
  readonly maxTerm: number
  /// The first repeat-related term id @internal
  readonly minRepeatTerm: number
  /// The tokenizer objects used by the grammar @internal
  readonly tokenizers: readonly Tokenizer[]
  /// Maps top rule names to [state ID, top term ID] pairs.
  readonly topRules: {[name: string]: [number, number]}
  /// Metadata about nested grammars used in this grammar @internal
  readonly nested: readonly NestInfo[]
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
  readonly top: [number, number]

  private nextStateCache: (readonly number[] | null)[] = []
  private cachedDialect: Dialect | null = null

  /// @internal
  constructor(spec: ParserSpec) {
    if (spec.version != File.Version)
      throw new RangeError(`Parser version (${spec.version}) doesn't match runtime version (${File.Version})`)
    let tokenArray = decodeArray<Uint16Array>(spec.tokenData)
    let nodeNames = spec.nodeNames.split(" ")
    this.minRepeatTerm = nodeNames.length
    for (let i = 0; i < spec.repeatNodeCount; i++) nodeNames.push("")
    let nodeProps: [NodeProp<any>, any][][] = []
    for (let i = 0; i < nodeNames.length; i++) nodeProps.push([])
    function setProp(nodeID: number, prop: NodeProp<any>, value: any) {
      nodeProps[nodeID].push([prop, prop.deserialize(String(value))])
    }
    if (spec.nodeProps) for (let propSpec of spec.nodeProps) {
      let prop = propSpec[0]
      for (let i = 1; i < propSpec.length;) {
        let next = propSpec[i++]
        if (next >= 0) {
          setProp(next as number, prop, propSpec[i++] as string)
        } else {
          let value = propSpec[i + -next] as string
          for (let j = -next; j > 0; j--) setProp(propSpec[i++] as number, prop, value)
          i++
        }
      }
    }
    this.specialized = new Uint16Array(spec.specialized ? spec.specialized.length : 0)
    this.specializers = []
    if (spec.specialized) for (let i = 0; i < spec.specialized.length; i++) {
      this.specialized[i] = spec.specialized[i].term
      this.specializers[i] = spec.specialized[i].get
    }

    this.states = decodeArray(spec.states, Uint32Array)
    this.data = decodeArray(spec.stateData)
    this.goto = decodeArray(spec.goto)
    let topTerms = Object.keys(spec.topRules).map(r => spec.topRules[r][1])
    this.nodeSet = new NodeSet(nodeNames.map((name, i) => NodeType.define({
      name: i >= this.minRepeatTerm ? undefined: name,
      id: i,
      props: nodeProps[i],
      top: topTerms.indexOf(i) > -1,
      error: i == 0,
      skipped: spec.skippedNodes && spec.skippedNodes.indexOf(i) > -1
    })))
    this.maxTerm = spec.maxTerm
    this.tokenizers = spec.tokenizers.map(value => typeof value == "number" ? new TokenGroup(tokenArray, value) : value)
    this.topRules = spec.topRules
    this.nested = (spec.nested || []).map(([name, value, endToken, placeholder]) => {
      return {name, value: ensureNested(value), end: new TokenGroup(decodeArray(endToken), 0), placeholder}
    })
    this.dialects = spec.dialects || {}
    this.dynamicPrecedences = spec.dynamicPrecedences || null
    this.tokenPrecTable = spec.tokenPrec
    this.termNames = spec.termNames || null
    this.maxNode = this.nodeSet.types.length - 1
    for (let i = 0, l = this.states.length / ParseState.Size; i < l; i++) this.nextStateCache[i] = null

    this.dialect = this.parseDialect()
    this.top = this.topRules[Object.keys(this.topRules)[0]]
  }

  /// Parse a given string or stream.
  parse(input: Input | string, options?: ParseOptions) {
    if (typeof input == "string") input = stringInput(input)
    let cx = new ParseContext(this, input, options)
    for (;;) {
      let done = cx.advance()
      if (done) return done
    }
  }

  /// Create a `ParseContext`.
  startParse(input: Input | string, options?: ParseOptions): IncrementalParse {
    if (typeof input == "string") input = stringInput(input)
    return new ParseContext(this, input, options)
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
  findNested(state: number) {
    let flags = this.stateSlot(state, ParseState.Flags)
    return flags & StateFlag.StartNest ? this.nested[flags >> StateFlag.NestShift] : null
  }

  /// @internal
  validAction(state: number, action: number) {
    if (action == this.stateSlot(state, ParseState.DefaultReduce)) return true
    for (let i = this.stateSlot(state, ParseState.Actions);; i += 3) {
      if (this.data[i] == Seq.End) {
        if (this.data[i + 1] == Seq.Next) i = pair(this.data, i + 2)
        else return false
      }
      if (action == pair(this.data, i + 1)) return true
    }
  }

  /// Get the states that can follow this one through shift actions or
  /// goto jumps. @internal
  nextStates(state: number): readonly number[] {
    let cached = this.nextStateCache[state]
    if (cached) return cached
    let result: number[] = []
    for (let i = this.stateSlot(state, ParseState.Actions);; i += 3) {
      if (this.data[i] == Seq.End) {
        if (this.data[i + 1] == Seq.Next) i = pair(this.data, i + 2)
        else break
      }
      if ((this.data[i + 2] & (Action.ReduceFlag >> 16)) == 0 && result.indexOf(this.data[i + 1]) < 0)
        result.push(this.data[i + 1])
    }
    let table = this.goto, max = table[0]
    for (let term = 0; term < max; term++) {
      for (let pos = table[term + 1];;) {
        let groupTag = table[pos++], target = table[pos++]
        for (let end = pos + (groupTag >> 1); pos < end; pos++)
          if (table[pos] == state && result.indexOf(target) < 0) result.push(target)
        if (groupTag & 1) break
      }
    }
    return this.nextStateCache[state] = result
  }

  /// @internal
  overrides(token: number, prev: number) {
    let iPrev = findOffset(this.data, this.tokenPrecTable, prev)
    return iPrev < 0 || findOffset(this.data, this.tokenPrecTable, token) < iPrev
  }

  /// Configure the parser. Returns a new parser instance that has the
  /// given settings modified. Settings not provided in `config` are
  /// kept from the original parser.
  configure(config: {
    /// Node props to add to the parser's node set.
    props?: readonly NodePropSource[],
    /// The name of the @top declaration to parse from. If not
    /// specified, the first @top declaration is used.
    top?: string,
    /// A space-separated string of dialects to enable.
    dialect?: string,
    /// The nested grammars to use. This can be used to, for example,
    /// swap in a different language for a nested grammar or fill in a
    /// nested grammar that was left blank by the original grammar.
    nested?: {[name: string]: NestedParser | IncrementalParser},
    /// Replace the given external tokenizers with new ones.
    tokenizers?: {from: ExternalTokenizer, to: ExternalTokenizer}[]
  }) {
    // Hideous reflection-based kludge to make it easy to create a
    // slightly modified copy of a parser.
    let copy = Object.assign(Object.create(Parser.prototype), this)
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
    if (config.dialect)
      copy.dialect = this.parseDialect(config.dialect)
    if (config.nested)
      copy.nested = this.nested.map(obj => {
        if (!Object.prototype.hasOwnProperty.call(config.nested, obj.name)) return obj
        return {name: obj.name, value: ensureNested(config.nested![obj.name]), end: obj.end, placeholder: obj.placeholder}
      })
    return copy as Parser
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

  /// Tells you whether this grammar has any nested grammars.
  get hasNested() { return this.nested.length > 0 }

  /// @internal
  dynamicPrecedence(term: number) {
    let prec = this.dynamicPrecedences
    return prec == null ? 0 : prec[term] || 0
  }

  /// @internal
  parseDialect(dialect?: string) {
    if (this.cachedDialect && this.cachedDialect.source == dialect) return this.cachedDialect
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
    return this.cachedDialect = new Dialect(dialect, flags, disabled)
  }

  /// (used by the output of the parser generator) @internal
  static deserialize(spec: ParserSpec) {
    return new Parser(spec)
  }
}

function ensureNested(parser: NestedParser | IncrementalParser): NestedParser {
  return (parser as any).startParse ? {parser: parser as IncrementalParser} : parser as NestedParser
}

function pair(data: Readonly<Uint16Array>, off: number) { return data[off] | (data[off + 1] << 16) }

// Hidden export for use by lezer-generator
;(Parser as any).TokenGroup = TokenGroup

function findOffset(data: Readonly<Uint16Array>, start: number, term: number) {
  for (let i = start, next; (next = data[i]) != Seq.End; i++)
    if (next == term) return i - start
  return -1
}

function findFinished(stacks: Stack[]) {
  let best: Stack | null = null
  for (let stack of stacks) {
    if (stack.pos == stack.cx.input.length &&
        stack.cx.parser.stateFlag(stack.state, StateFlag.Accepting) &&
        (!best || best.score < stack.score))
      best = stack
  }
  return best
}
