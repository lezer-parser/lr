import {Stack, Recover} from "./stack"
import {Action, Specialize, Term, Seq, StateFlag, ParseState, NodeFlag, File} from "./constants"
import {InputStream, Token, StringStream, Tokenizer, TokenGroup, ExternalTokenizer} from "./token"
import {DefaultBufferLength, Tree, TreeBuffer, NodeGroup, NodeType, NodeProp, NodePropSource} from "lezer-tree"
import {decodeArray} from "./decode"

// Environment variable used to control console output
const verbose = typeof process != "undefined" && /\bparse\b/.test(process.env.LOG!)

let stackIDs: WeakMap<Stack, string> | null = null

/// Nested grammar values are associated with nesting positions in the
/// grammar. If they are null, the nested region is simply skipped
/// over. If they hold a parser object, that parser is used to parse
/// the region. To implement dynamic behavior, the value may also be a
/// function which returns a description of the way the region should
/// be parsed.
export type NestedGrammar = null | Parser | ((input: InputStream, stack: Stack) => NestedGrammarSpec)

/// An object indicating how to proceed with a nested parse.
export interface NestedGrammarSpec {
  /// When given, this is used to provide a parser that should be used
  /// to parse the content.
  parser?: Parser
  /// When `parser` is given, this can be used to configure which top
  /// rule to parse with it.
  top?: string
  /// When `parser` is given, this can be used to configure a dialect.
  dialect?: string
  /// This being true means that the outer grammar should use
  /// the fallback expression provided for the nesting to parse the
  /// content.
  stay?: boolean
  /// Alternatively, `parseNode` may hold a function which will be made
  /// responsible for parsing the region.
  parseNode?: (input: InputStream, start: number) => Tree
  /// An optional extra type to tag the resulting tree with.
  wrapType?: number,
  /// When a `filterEnd` property is present, that should hold a
  /// function that determines whether a given end token (which matches
  /// the end token specified in the grammar) should be used (true) or
  /// ignored (false). This is mostly useful for implementing things
  /// like XML closing tag matching.
  filterEnd?: (endToken: string) => boolean
}

class CacheCursor {
  trees: Tree[]
  start = [0]
  index = [0]
  nextStart: number = 0

  constructor(tree: Tree) { this.trees = [tree] }

  // `pos` must be >= any previously given `pos` for this cursor
  nodeAt(pos: number): Tree | TreeBuffer | null {
    if (pos < this.nextStart) return null

    for (;;) {
      let last = this.trees.length - 1
      if (last < 0) { // End of tree
        this.nextStart = 1e9
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
      if (start >= pos) return start == pos ? next : null
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

  getActions(stack: Stack, input: InputStream) {
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

  updateCachedToken(token: CachedToken, tokenizer: Tokenizer, stack: Stack, input: InputStream) {
    token.clear(stack.pos)
    tokenizer.token(input, token, stack)
    if (token.value > -1) {
      let {parser} = stack.cx

      for (let i = 0; i < parser.specialized.length; i++) if (parser.specialized[i] == token.value) {
        let result = parser.specializers[i](input.read(token.start, token.end), stack)
        if (result >= 0 && stack.cx.dialect.allows(result >> 1)) {
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
  /// Passing a cached tree is used for incremental parsing. This
  /// should be a tree whose content is aligned with the current
  /// document (though a call to `Tree.unchanged`) if any changes were
  /// made since it was produced. The parser will try to reuse nodes
  /// from this tree in the new parse, greatly speeding up the parse
  /// when it can reuse nodes for most of the document.
  cache?: Tree
  /// When true, the parser will raise an exception, rather than run
  /// its error-recovery strategies, when the input doesn't match the
  /// grammar.
  strict?: boolean,
  /// The maximum length of the TreeBuffers generated in the output
  /// tree. Defaults to 1024.
  bufferLength?: number,
  /// The name of the @top declaration to parse from. If not
  /// specified, the first @top declaration is used.
  top?: string,
  /// A space-separated string of dialects to enable.
  dialect?: string
}

export class StackContext {
  reused: (Tree | TreeBuffer)[] = []
  tokens: TokenCache
  constructor(
    readonly parser: Parser,
    readonly maxBufferLength: number,
    readonly input: InputStream,
    readonly topTerm: number,
    readonly dialect: Dialect,
    readonly parent: Stack | null = null,
    public wrapType: number = -1 // Set to -2 when a stack descending from this nesting event finishes
  ) {
    this.tokens = new TokenCache(parser)
  }
}

const recoverDist = 5, maxRemainingPerStep = 3, minBufferLengthPrune = 200, forceReduceLimit = 10

/// A parse context can be used for step-by-step parsing. After
/// creating it, you repeatedly call `.advance()` until it returns a
/// tree to indicate it has reached the end of the parse.
export class ParseContext {
  // Active parse stacks.
  private stacks: Stack[]
  // The position to which the parse has advanced.
  public pos = 0
  private recovering = 0
  private tokenCount = 0
  private cache: CacheCursor | null
  private strict: boolean
  private nextStackID = 0x2654

  /// @internal
  constructor(parser: Parser,
              input: InputStream,
              options: ParseOptions = {}) {
    let {cache = undefined, strict = false, bufferLength = DefaultBufferLength, top = undefined, dialect} = options
    let topInfo = top ? parser.topRules[top] : parser.defaultTop
    if (!topInfo) throw new RangeError(`Invalid top rule name ${top}`)
    this.stacks = [Stack.start(new StackContext(parser, bufferLength, input, topInfo[1], parser.parseDialect(dialect)), topInfo[0])]
    this.strict = strict
    this.cache = cache ? new CacheCursor(cache) : null
  }

  /// @internal
  putStack(stack: Stack) {
    this.stacks.push(stack)
    if (this.pos < 0 || stack.pos < this.pos) this.pos = stack.pos
  }

  /// @internal
  putStackDedup(stack: Stack) {
    for (let i = 0; i < this.stacks.length; i++) {
      let other = this.stacks[i]
      if (other.pos == stack.pos && other.sameState(stack)) {
        if (this.stacks[i].score < stack.score) this.stacks[i] = stack
        return
      }
    }
    this.putStack(stack)
  }

  /// Move the parser forward. This will process all parse stacks at
  /// `this.pos` and try to advance them to a further position. If no
  /// stack for such a position is found, it'll start error-recovery.
  ///
  /// When the parse is finished, this will return a syntax tree. When
  /// not, it returns `null`.
  advance() {
    let stacks = this.stacks, pos = this.pos
    // This will now hold stacks beyond `pos`.
    this.stacks = []
    // Will be reset to the next position by `putStack`.
    this.pos = -1
    let stopped: Stack[] | null = null, stoppedTokens: number[] | null = null

    // Keep advancing any stacks at `pos` until they either move
    // forward or can't be advanced. Gather stacks that can't be
    // advanced further in `stopped`.
    for (let i = 0; i < stacks.length; i++) {
      let stack = stacks[i]
      for (;;) {
        if (stack.pos > pos) {
          this.putStack(stack)
        } else {
          let result = this.advanceStack(stack, stacks)
          if (result) {
            stack = result
            continue
          } else {
            if (!stopped) { stopped = []; stoppedTokens = [] }
            stopped.push(stack)
            let tok = stack.cx.tokens.mainToken
            stoppedTokens!.push(tok.value, tok.end)
          }
        }
        break
      }
    }

    if (!this.stacks.length) {
      let finished = stopped && findFinished(stopped)
      if (finished) return finished.toTree()

      if (this.strict) {
        if (verbose && stopped)
          console.log("Stuck with token " + stopped[0].cx.parser.getName(stopped[0].cx.tokens.mainToken.value))
        throw new SyntaxError("No parse at " + pos)
      }
      if (!this.recovering) this.recovering = recoverDist
    }

    if (this.recovering && stopped) {
      let finished = this.runRecovery(stopped, stoppedTokens!)
      if (finished) return finished.forceAll().toTree()
    }

    if (this.recovering) {
      let maxRemaining = this.recovering == 1 ? 1 : this.recovering * maxRemainingPerStep
      if (this.stacks.length > maxRemaining) {
        this.stacks.sort((a, b) => b.score - a.score)
        this.stacks.length = maxRemaining
      }
      if (this.stacks.some(s => s.reducePos > pos)) this.recovering--
    } else if (this.stacks.length > 1) {
      // Prune stacks that are in the same state, or that have been
      // running without splitting for a while, to avoid getting stuck
      // with multiple successful stacks running endlessly on.
      outer: for (let i = 0; i < this.stacks.length - 1; i++) {
        let stack = this.stacks[i]
        for (let j = i + 1; j < this.stacks.length; j++) {
          let other = this.stacks[j]
          if (stack.sameState(other) ||
              stack.buffer.length > minBufferLengthPrune && other.buffer.length > minBufferLengthPrune) {
            if (((stack.score - other.score) || (stack.buffer.length - other.buffer.length)) > 0) {
              this.stacks.splice(j--, 1)
            } else {
              this.stacks.splice(i--, 1)
              continue outer
            }
          }
        }
      }
    }

    this.tokenCount++
    return null
  }

  // Returns an updated version of the given stack, or null if the
  // stack can't advance normally. When `split` is given, stacks split
  // off by ambiguous operations will be pushed to that, or given to
  // `putStack` if they move `pos` forward.
  private advanceStack(stack: Stack, split: null | Stack[]) {
    let start = stack.pos, {input, parser} = stack.cx
    let base = verbose ? this.stackID(stack) + " -> " : ""

    if (this.cache) {
      for (let cached = this.cache.nodeAt(start); cached;) {
        let match = parser.group.types[cached.type.id] == cached.type ? parser.getGoto(stack.state, cached.type.id) : -1
        if (match > -1 && cached.length) {
          stack.useNode(cached, match)
          if (verbose) console.log(base + this.stackID(stack) + ` (via reuse of ${parser.getName(cached.type.id)})`)
          return stack
        }
        if (!(cached instanceof Tree) || cached.children.length == 0 || cached.positions[0] > 0) break
        let inner = cached.children[0]
        if (inner instanceof Tree) cached = inner
        else break
      }
    }

    let nest = parser.startNested(stack.state)
    maybeNest: if (nest > -1) {
      let {grammar, end: endToken, placeholder} = parser.nested[nest]
      let filterEnd = undefined, parseNode = null, nested, top, dialect, wrapType = undefined
      if (typeof grammar == "function") {
        let query = grammar(input, stack)
        if (query.stay) break maybeNest
        ;({parseNode, parser: nested, top, dialect, filterEnd, wrapType} = query)
      } else {
        nested = grammar
      }
      let end = this.scanForNestEnd(stack, endToken, filterEnd)
      let clippedInput = stack.cx.input.clip(end)
      if (parseNode || !nested) {
        let node = parseNode ? parseNode(clippedInput, stack.pos) : Tree.empty
        if (node.length != end - stack.pos) node = new Tree(node.type, node.children, node.positions, end - stack.pos)
        if (wrapType != null) node = new Tree(parser.group.types[wrapType], [node], [0], node.length)
        stack.useNode(node, parser.getGoto(stack.state, placeholder, true))
        return stack
      } else {
        let topInfo = top ? nested.topRules[top] : nested.defaultTop
        let newStack = Stack.start(new StackContext(nested, stack.cx.maxBufferLength, clippedInput, topInfo[1],
                                                    nested.parseDialect(dialect), stack, wrapType),
                                   topInfo[0], stack.pos)
        if (verbose) console.log(base + this.stackID(newStack) + ` (nested)`)
        return newStack
      }
    }

    let defaultReduce = parser.stateSlot(stack.state, ParseState.DefaultReduce)
    if (defaultReduce > 0) {
      stack.reduce(defaultReduce)
      if (verbose) console.log(base + this.stackID(stack) + ` (via always-reduce ${parser.getName(defaultReduce & Action.ValueMask)})`)
      return stack
    }

    let actions = stack.cx.tokens.getActions(stack, input)
    for (let i = 0; i < actions.length;) {
      let action = actions[i++], term = actions[i++], end = actions[i++]
      let last = i == actions.length || !split
      let localStack = last ? stack : stack.split()
      localStack.apply(action, term, end)
      if (verbose)
        console.log(base + this.stackID(localStack) + ` (via ${(action & Action.ReduceFlag) == 0 ? "shift"
                     : `reduce of ${parser.getName(action & Action.ValueMask)}`} for ${
        parser.getName(term)} @ ${start}${localStack == stack ? "" : ", split"})`)
      if (last) return localStack
      else if (localStack.pos > start) this.putStack(localStack)
      else split!.push(localStack)
    }

    if (stack.cx.parent && stack.pos == input.length) return this.finishNested(stack)
    return null
  }

  // Advance a given stack forward as far as it will go. Returns the
  // (possibly updated) stack if it got stuck, or null if it moved
  // forward and was given to `putStackDedup`.
  private advanceFully(stack: Stack) {
    let pos = stack.pos
    for (;;) {
      let result = this.advanceStack(stack, null)
      if (!result) return stack
      if (result.pos > pos) {
        this.putStackDedup(result)
        return null
      }
      stack = result
    }
  }

  private runRecovery(stacks: Stack[], tokens: number[]) {
    let finished: Stack | null = null, restarted = false
    for (let i = 0; i < stacks.length; i++) {
      let stack = stacks[i], token = tokens[i << 1], tokenEnd = tokens[(i << 1) + 1]
      let base = verbose ? this.stackID(stack) + " -> " : ""

      if (stack.deadEnd) {
        if (restarted) continue
        restarted = true
        stack.restart()
        if (verbose) console.log(base + this.stackID(stack) + " (restarted)")
        let stopped = this.advanceFully(stack)
        if (stopped) stack = stopped
        else continue
      }

      let force = stack.split(), forceBase = base
      for (let j = 0; force.forceReduce() && j < forceReduceLimit; j++) {
        if (verbose) console.log(forceBase + this.stackID(force) + " (via force-reduce)")
        let stopped = this.advanceFully(force)
        if (!stopped) break
        force = stopped
        if (verbose) forceBase = this.stackID(stopped) + " -> "
      }

      for (let insert of stack.recoverByInsert(token)) {
        if (verbose) console.log(base + this.stackID(insert) + " (via recover-insert)")
        this.advanceFully(insert)
      }

      if (stack.cx.input.length > stack.pos) {
        if (tokenEnd == stack.pos) {
          tokenEnd++
          token = Term.Err
        }
        stack.recoverByDelete(token, tokenEnd)
        if (verbose) console.log(base + this.stackID(stack) + ` (via recover-delete ${stack.cx.parser.getName(token)})`)
        this.putStackDedup(stack)
      } else if (!stack.cx.parent && (!finished || finished.score < stack.score)) {
        finished = stack
      }
    }
    return finished
  }

  /// Force the parse to finish, generating a tree containing the nodes
  /// parsed so far.
  forceFinish() {
    return this.stacks[0].split().forceAll().toTree()
  }

  /// A value that indicates how successful the parse is so far, as
  /// the number of error-recovery steps taken divided by the number
  /// of tokens parsed. Could be used to decide to abort a parse when
  /// the input doesn't appear to match the grammar at all.
  get badness() {
    if (!this.stacks.length) return 0
    return -(this.stacks[0].score / (Recover.Token * this.tokenCount))
  }

  private scanForNestEnd(stack: Stack, endToken: TokenGroup, filter?: ((token: string) => boolean)) {
    let input = stack.cx.input
    for (let pos = stack.pos; pos < input.length; pos++) {
      dummyToken.start = pos
      dummyToken.value = -1
      endToken.token(input, dummyToken, stack)
      if (dummyToken.value > -1 && (!filter || filter(input.read(pos, dummyToken.end)))) return pos
    }
    return input.length
  }

  private finishNested(stack: Stack) {
    if (stack.cx.wrapType == -2) return null // Another nested stack already finished
    let parent = stack.cx.parent!, tree = stack.forceAll().toTree()
    let parentParser = parent.cx.parser, info = parentParser.nested[parentParser.startNested(parent.state)]
    tree = new Tree(tree.type, tree.children, tree.positions.map(p => p - parent!.pos), stack.pos - parent.pos)
    if (stack.cx.wrapType > -1) tree = new Tree(parentParser.group.types[stack.cx.wrapType], [tree], [0], tree.length)
    stack.cx.wrapType = -2
    parent.useNode(tree, parentParser.getGoto(parent.state, info.placeholder, true))
    if (verbose) console.log(this.stackID(parent) + ` (via unnest ${stack.cx.wrapType > -1 ? parentParser.getName(stack.cx.wrapType) : tree.type.name})`)
    return parent
  }

  private stackID(stack: Stack) {
    let id = (stackIDs || (stackIDs = new WeakMap)).get(stack)
    if (!id) stackIDs.set(stack, id = String.fromCodePoint(this.nextStackID++))
    return id + stack
  }
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
  nested?: [string, null | NestedGrammar, string | Uint16Array, number][],
  dialects?: {[name: string]: number},
  dynamicPrecedences?: {[term: number]: number},
  specialized?: {term: number, get: (value: string, stack: Stack) => number}[],
  tokenPrec: number,
  termNames?: {[id: number]: string}
}

/// A parser holds the parse tables for a given grammar, as generated
/// by `lezer-generator`.
export class Parser {
  /// The parse states for this grammar @internal
  readonly states: Readonly<Uint32Array>
  /// A blob of data that the parse states, as well as some
  /// of `Parser`'s fields, point into @internal
  readonly data: Readonly<Uint16Array>
  /// The goto table. See `computeGotoTable` in
  /// lezer-generator for details on the format @internal
  readonly goto: Readonly<Uint16Array>
  /// A node group with the node types used by this parser.
  readonly group: NodeGroup
  /// The highest term id @internal
  readonly maxTerm: number
  /// The first repeat-related term id @internal
  readonly minRepeatTerm: number
  /// The tokenizer objects used by the grammar @internal
  readonly tokenizers: readonly Tokenizer[]
  /// Maps top rule names to [state ID, top term ID] pairs.
  readonly topRules: {[name: string]: [number, number]}
  /// Metadata about nested grammars used in this grammar @internal
  readonly nested: readonly {
    /// A name, used by `withNested`
    name: string,
    /// The grammar or grammar query function to use
    grammar: NestedGrammar,
    /// A token-recognizing automaton for the end of the nesting
    end: TokenGroup,
    /// The id of the placeholder term that appears in the grammar at
    /// the position of this nesting
    placeholder: number
  }[]
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
    let nodeProps: {[id: number]: any}[] = []
    for (let i = 0; i < nodeNames.length; i++) nodeProps.push(noProps)
    function setProp(nodeID: number, prop: NodeProp<any>, value: any) {
      if (nodeProps[nodeID] == noProps) nodeProps[nodeID] = Object.create(null)
      prop.set(nodeProps[nodeID], prop.deserialize(String(value)))
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
    this.group = new NodeGroup(nodeNames.map((name, i) => {
      let flags = (i >= this.minRepeatTerm ? NodeFlag.Anonymous : 0) |
        (topTerms.indexOf(i) > -1 ? NodeFlag.Top : 0) |
        (i == 0 ? NodeFlag.Error : 0) |
        (spec.skippedNodes && spec.skippedNodes.indexOf(i) > -1 ? NodeFlag.Skipped : 0)
      return new (NodeType as any)(name, nodeProps[i], i, flags)
    }))
    this.maxTerm = spec.maxTerm
    this.tokenizers = spec.tokenizers.map(value => typeof value == "number" ? new TokenGroup(tokenArray, value) : value)
    this.topRules = spec.topRules
    this.nested = (spec.nested || []).map(([name, grammar, endToken, placeholder]) => {
      return {name, grammar, end: new TokenGroup(decodeArray(endToken), 0), placeholder}
    })
    this.dialects = spec.dialects || {}
    this.dynamicPrecedences = spec.dynamicPrecedences || null
    this.tokenPrecTable = spec.tokenPrec
    this.termNames = spec.termNames || null
    this.maxNode = this.group.types.length - 1
    for (let i = 0, l = this.states.length / ParseState.Size; i < l; i++) this.nextStateCache[i] = null
  }

  /// Parse a given string or stream.
  parse(input: InputStream | string, options?: ParseOptions) {
    if (typeof input == "string") input = new StringStream(input)
    let cx = new ParseContext(this, input, options)
    for (;;) {
      let done = cx.advance()
      if (done) return done
    }
  }

  /// Create a `ParseContext`.
  startParse(input: InputStream | string, options?: ParseOptions) {
    if (typeof input == "string") input = new StringStream(input)
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
  startNested(state: number) {
    let flags = this.stateSlot(state, ParseState.Flags)
    return flags & StateFlag.StartNest ? flags >> StateFlag.NestShift : -1
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

  /// Create a new `Parser` instance with different values for (some
  /// of) the nested grammars. This can be used to, for example, swap
  /// in a different language for a nested grammar or fill in a nested
  /// grammar that was left blank by the original grammar.
  withNested(spec: {[name: string]: NestedGrammar | null}) {
    return this.copy({nested: this.nested.map(obj => {
      if (!Object.prototype.hasOwnProperty.call(spec, obj.name)) return obj
      return {name: obj.name, grammar: spec[obj.name], end: obj.end, placeholder: obj.placeholder}
    })})
  }

  /// Create a new `Parser` instance whose node types have the given
  /// props added. You should use [`NodeProp.add`](#tree.NodeProp.add)
  /// to create the arguments to this method.
  withProps(...props: NodePropSource[]) {
    return this.copy({group: this.group.extend(...props)})
  }

  /// Replace the given external tokenizer with another one, returning
  /// a new parser object.
  withTokenizer(from: ExternalTokenizer, to: ExternalTokenizer) {
    return this.copy({tokenizers: this.tokenizers.map(t => t == from ? to : t)})
  }

  private copy(props: {[name: string]: any}): Parser {
    // Hideous reflection-based kludge to make it easy to create a
    // slightly modified copy of a parser.
    let obj = Object.create(Parser.prototype)
    for (let key of Object.keys(this))
      obj[key] = key in props ? props[key] : (this as any)[key]
    return obj
  }

  /// Returns the name associated with a given term. This will only
  /// work for all terms when the parser was generated with the
  /// `--names` option. By default, only the names of tagged terms are
  /// stored.
  getName(term: number): string {
    return this.termNames ? this.termNames[term] : String(term <= this.maxNode && this.group.types[term].name || term)
  }

  /// The eof term id is always allocated directly after the node
  /// types. @internal
  get eofTerm() { return this.maxNode + 1 }

  /// Tells you whether this grammar has any nested grammars.
  get hasNested() { return this.nested.length > 0 }

  /// @internal
  get defaultTop() { return this.topRules[Object.keys(this.topRules)[0]] }

  /// @internal
  dynamicPrecedence(term: number) {
    let prec = this.dynamicPrecedences
    return prec == null ? 0 : prec[term] || 0
  }

  /// The node type produced by the default top rule.
  get topType() { return this.group.types[this.defaultTop[1]] }

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

function pair(data: Readonly<Uint16Array>, off: number) { return data[off] | (data[off + 1] << 16) }

// Hidden export for use by lezer-generator
;(Parser as any).TokenGroup = TokenGroup

const noProps: {[propID: number]: any} = Object.create(null)

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
