import {Stack, Badness} from "./stack"
import {Action, Specialize, Term, Seq, StateFlag, ParseState} from "./constants"
import {InputStream, Token, StringStream, Tokenizer, TokenGroup} from "./token"
import {DefaultBufferLength, Tag, Tree, TreeBuffer} from "lezer-tree"
import {decodeArray} from "./decode"

// Environment variable used to control console output
const verbose = typeof process != "undefined" && /\bparse\b/.test(process.env.LOG!)

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
  nodeAt(pos: number): Tree | null {
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
      if (next instanceof TreeBuffer) {
        this.index[last]++
        this.nextStart = start + next.length
      } else if (start >= pos) {
        return start == pos ? next : null
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

  constructor(readonly tokenizer: Tokenizer) { super() }

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

  getActions(stack: Stack, input: InputStream) {
    let actionIndex = 0
    let main: CachedToken | null = null
    let {parser} = stack.cx, {tokenizers} = parser

    for (let i = 0; i < tokenizers.length; i++) {
      if (((1 << i) & parser.stateSlot(stack.state, ParseState.TokenizerMask)) == 0) continue
      let tokenizer = tokenizers[i]
      let token = this.tokens.find(c => c.tokenizer == tokenizer)
      if (!token) this.tokens.push(token = new CachedToken(tokenizer))
      if (tokenizer.contextual || token.start != stack.pos) this.updateCachedToken(token, stack, input)

      let startIndex = actionIndex
      if (token.extended > -1) actionIndex = this.addActions(stack, token.extended, token.end, actionIndex)
      actionIndex = this.addActions(stack, token.value, token.end, actionIndex)
      if (actionIndex > startIndex) {
        main = token
        break
      }
      if (!main || token.value != Term.Err) main = token
    }

    if (this.actions.length > actionIndex) this.actions.length = actionIndex
    this.mainToken = main || dummyToken.asError(stack.pos, input.length)
    return this.actions
  }

  updateCachedToken(token: CachedToken, stack: Stack, input: InputStream) {
    token.clear(stack.pos)
    token.tokenizer.token(input, token, stack)
    if (token.value > -1) {
      let {parser} = stack.cx
      let specIndex = findOffset(parser.data, parser.specializeTable, token.value)
      if (specIndex >= 0) {
        let found = parser.specializations[specIndex][input.read(token.start, token.end)]
        if (found != null) {
          if ((found & 1) == Specialize.Specialize) token.value = found >> 1
          else token.extended = found >> 1
        }
      }
    } else {
      token.asError(stack.pos, input.length)
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
      for (let i = parser.stateSlot(state, set ? ParseState.Skip : ParseState.Actions), next; (next = data[i]) != Seq.End; i += 3) {
        if (next == token || (next == Term.Err && index == 0))
          index = this.putAction(data[i + 1] | (data[i + 2] << 16), token, end, index)
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
  bufferLength?: number
}

export class StackContext {
  reused: Tree[] = []
  tokens = new TokenCache
  constructor(readonly parser: Parser,
              readonly maxBufferLength: number,
              readonly input: InputStream,
              readonly parent: Stack | null = null, 
              readonly wrapType: number = -1) {}
}

/// A parse context can be used for step-by-step parsing. After
/// creating it, you repeatedly call `.advance()` until it returns a
/// tree to indicate it has reached the end of the parse.
export class ParseContext {
  /// @internal
  stacks: Stack[]
  /// @internal
  cache: CacheCursor | null
  /// @internal
  strict: boolean

  /// @internal
  constructor(parser: Parser,
              input: InputStream,
              {cache = undefined, strict = false, bufferLength = DefaultBufferLength}: ParseOptions = {}) {
    this.stacks = [Stack.start(new StackContext(parser, bufferLength, input))]
    this.strict = strict
    this.cache = cache ? new CacheCursor(cache) : null
  }

  private takeStack() {
    // Binary heap pop
    let {stacks} = this, elt = stacks[0], replacement = stacks.pop()!
    if (stacks.length == 0) return elt
    stacks[0] = replacement
    for (let index = 0;;) {
      let childIndex = (index << 1) + 1
      if (childIndex >= stacks.length) break
      let child = stacks[childIndex]
      if (childIndex + 1 < stacks.length && child.compare(stacks[childIndex + 1]) >= 0) {
        child = stacks[childIndex + 1]
        childIndex++
      }
      if (replacement.compare(child) < 0) break
      stacks[childIndex] = replacement
      stacks[index] = child
      index = childIndex
    }
    return elt
  }

  private putStack(stack: Stack, strict = stack.badness < Badness.Stabilizing || stack.badness > Badness.Wild): boolean {
    let stacks = this.stacks
    for (let i = 0; i < stacks.length; i++) {
      let other = stacks[i]
      if ((strict || other.state == stack.state) && other.pos == stack.pos) {
        let diff = stack.badness - other.badness || (stack.badness < Badness.Stabilizing ? 0 : stack.stack.length - other.stack.length)
        if (diff < 0) { stacks[i] = stack; return true }
        else if (diff > 0) return false
      }
    }

    // Binary heap add
    let index = stacks.push(stack) - 1
    while (index > 0) {
      let parentIndex = index >> 1, parent = stacks[parentIndex]
      if (stack.compare(parent) >= 0) break
      stacks[index] = parent
      stacks[parentIndex] = stack
      index = parentIndex
    }
    return true
  }

  /// Execute one parse step. This picks the parse stack that's
  /// currently the least far along, and does the next thing that can
  /// be done with it. This may be:
  ///
  /// - Add a cached node, if a matching one is found.
  /// - Enter a nested grammar.
  /// - Perform all shift or reduce actions that match the current
  ///   token (if there are more than one, this will split the stack)
  /// - Finish the parse
  ///
  /// When the parse is finished, this will return a syntax tree. When
  /// not, it returns `null`.
  advance() {
    let stack = this.takeStack(), start = stack.pos, {input, parser} = stack.cx

    if (this.cache) {
      for (let cached = this.cache.nodeAt(start); cached;) {
        if (!cached.isPartOf(parser.tags)) continue
        let match = parser.getGoto(stack.state, cached.type)
        if (match > -1 && !isFragile(cached)) {
          stack.useNode(cached, match)
          if (verbose) console.log(stack + ` (via reuse of ${parser.getName(cached.type)})`)
          this.putStack(stack)
          return null
        }
        if (cached.children.length == 0 || cached.positions[0] > 0) break
        let inner = cached.children[0]
        if (inner instanceof Tree) cached = inner
        else break
      }
    }

    let nest = parser.startNested(stack.state)
    maybeNest: if (nest > -1) {
      let {grammar, end: endToken, placeholder} = parser.nested[nest]
      let filterEnd = undefined, parseNode = null, nested, wrapType = undefined
      if (typeof grammar == "function") {
        let query = grammar(input, stack)
        if (query.stay) break maybeNest
        ;({parseNode, parser: nested, filterEnd, wrapType} = query)
      } else {
        nested = grammar
      }
      let end = this.scanForNestEnd(stack, endToken, filterEnd)
      let clippedInput = stack.cx.input.clip(end)
      if (parseNode || !nested) {
        let node = parseNode ? parseNode(clippedInput, stack.pos) : Tree.empty
        if (node.length != end - stack.pos) node = new Tree(node.children, node.positions, end - stack.pos, node.tags, node.type)
        if (wrapType != null) node = new Tree([node], [0], node.length, parser.tags, wrapType)
        stack.useNode(node, parser.getGoto(stack.state, placeholder, true))
        this.putStack(stack)
      } else {
        let newStack = Stack.start(new StackContext(nested, stack.cx.maxBufferLength, clippedInput, stack, wrapType), stack.pos)
        if (verbose) console.log(newStack + ` (nested)`)
        this.putStack(newStack)
      }
      return null
    }

    let defaultReduce = parser.stateSlot(stack.state, ParseState.DefaultReduce)
    if (defaultReduce > 0) {
      stack.reduce(defaultReduce)
      this.putStack(stack)
      if (verbose) console.log(stack + ` (via always-reduce ${parser.getName(defaultReduce & Action.ValueMask)})`)
      return null
    }

    let actions = stack.cx.tokens.getActions(stack, input)
    for (let i = 0; i < actions.length;) {
      let action = actions[i++], term = actions[i++], end = actions[i++]
      let localStack = i == actions.length ? stack : stack.split()
      localStack.apply(action, term, end)
      if (verbose)
        console.log(localStack + ` (via ${(action & Action.ReduceFlag) == 0 ? "shift"
                     : `reduce of ${parser.getName(action & Action.ValueMask)}`} for ${
        parser.getName(term)} @ ${start}${localStack == stack ? "" : ", split"})`)
      this.putStack(localStack, (action & Action.ReduceFlag) != 0)
    }
    if (actions.length > 0) return null

    // If we're here, the stack failed to advance normally

    if (start == input.length && (parser.stateFlag(stack.state, StateFlag.Accepting) || this.stacks.length == 0)) {
      while (!parser.stateFlag(stack.state, StateFlag.Accepting) && stack.forceReduce()) {}
      let tree = stack.toTree(), {parent} = stack.cx
      if (parent) {
        // This is a nested parse—add its result to the parent stack and
        // continue with that one.
        let parentParser = parent.cx.parser, info = parentParser.nested[parentParser.startNested(parent.state)]
        tree = new Tree(tree.children, tree.positions.map(p => p - parent!.pos), stack.pos - parent.pos,
                        tree.tags, tree.type)
        if (stack.cx.wrapType > -1) tree = new Tree([tree], [0], tree.length, parentParser.tags, stack.cx.wrapType)
        parent.useNode(tree, parentParser.getGoto(parent.state, info.placeholder, true))
        if (verbose) console.log(parent + ` (via unnest${tree.type & Term.Tagged ? " " + tree.tag.tag : ""})`)
        this.putStack(parent)
        return null
      } else {
        // Actual end of parse
        return stack.toTree()
      }
    }

    let {end, value: term} = stack.cx.tokens.mainToken
    if (!this.strict &&
        !(stack.badness > Badness.Wild && this.stacks.some(s => s.pos >= stack.pos && s.badness <= stack.badness))) {
      let inserted = stack.recoverByInsert(term)
      if (inserted) {
        if (verbose) console.log(inserted + " (via recover-insert)")
        this.putStack(inserted)
      }

      if (end == start) {
        if (start == input.length) return null
        end++
        term = Term.Err
      }
      stack.recoverByDelete(term, end)
      if (verbose) console.log(stack + ` (via recover-delete ${parser.getName(term)})`)
      this.putStack(stack)
    } else if (!this.stacks.length) {
      // Only happens in strict mode
      throw new SyntaxError("No parse at " + start + " with " + parser.getName(term) + " (stack is " + stack + ")")
    }
    return null
  }

  /// The position to which the parse has advanced.
  get pos() { return this.stacks[0].pos }

  /// Force the parse to finish, generating a tree containing the nodes
  /// parsed so far.
  forceFinish() {
    let stack = this.stacks[0].split()
    while (!stack.cx.parser.stateFlag(stack.state, StateFlag.Accepting) && stack.forceReduce()) {}
    return stack.toTree()
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
}

/// A parser holds the parse tables for a given grammar, as generated
/// by `lezer-generator`.
export class Parser {
  /// @internal
  constructor(
    /// The parse states for this grammar @internal
    readonly states: Readonly<Uint32Array>,
    /// A blob of data that the parse states, as well as some
    /// of `Parser`'s fields, point into @internal
    readonly data: Readonly<Uint16Array>,
    /// The goto table. See `computeGotoTable` in
    /// lezer-generator for details on the format @internal
    readonly goto: Readonly<Uint16Array>,
    /// A `TagMap` mapping the node types in this grammar to their tag
    /// names.
    readonly tags: readonly Tag[],
    /// The tokenizer objects used by the grammar @internal
    readonly tokenizers: readonly Tokenizer[],
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
    }[],
    /// Points into this.data at an array of token types that
    /// are specialized @internal
    readonly specializeTable: number,
    /// For each specialized token type, this holds an object mapping
    /// names to numbers, with the first bit indicating whether the
    /// specialization extends or replaces the original token, and the
    /// rest of the bits holding the specialized token type. @internal
    readonly specializations: readonly {[value: string]: number}[],
    /// Points into this.data at an array that holds the
    /// precedence order (higher precedence first) for ambiguous
    /// tokens @internal
    readonly tokenPrecTable: number,
    /// Points at an array of node types that are part of
    /// skip rules @internal
    readonly skippedNodes: number,
    /// An optional object mapping term ids to name strings @internal
    readonly termNames: null | {[id: number]: string} = null
  ) {}

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
  startParse(input: InputStream, options?: ParseOptions) {
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
      for (let i = this.stateSlot(state, set ? ParseState.Skip : ParseState.Actions), next; (next = data[i]) != Seq.End; i += 3) {
        if (next == terminal || next == Term.Err)
          return data[i + 1] | (data[i + 2] << 16)
      }
    }
    return 0
  }

  // Get a recovery action for a given state and terminal, or 0 when
  // none
  ///@internal
  getRecover(state: number, terminal: number) {
    for (let i = this.stateSlot(state, ParseState.Recover), next; (next = this.data[i]) != Seq.End; i += 2)
      if (next == terminal) return this.data[i + 1]
    return 0
  }

  /// @internal
  stateSlot(state: number, slot: number) {
    return this.states[(state << ParseState.Shift) + slot]
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
  anyReduce(state: number) {
    let defaultReduce = this.stateSlot(state, ParseState.DefaultReduce)
    if (defaultReduce > 0) return defaultReduce
    for (let i = this.stateSlot(state, ParseState.Actions);; i += 3) {
      if (this.data[i] == Seq.End) return 0
      let isReduce = this.data[i + 2]
      if (isReduce) return this.data[i + 1] | (isReduce << 16)
    }
  }

  /// Tells you whether a given term is part of the skip rules for the
  /// grammar.
  isSkipped(term: number) {
    for (let i = this.skippedNodes, cur; (cur = this.data[i]) != Seq.End; i++)
      if (cur == term) return true
    return false
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
    return new Parser(this.states, this.data, this.goto, this.tags, this.tokenizers,
                      this.nested.map(obj => {
                        if (!Object.prototype.hasOwnProperty.call(spec, obj.name)) return obj
                        return {name: obj.name, grammar: spec[obj.name], end: obj.end, placeholder: obj.placeholder}
                      }),
                      this.specializeTable, this.specializations, this.tokenPrecTable, this.skippedNodes, this.termNames)
  }

  /// Returns the name associated with a given term. This will only
  /// work for all terms when the parser was generated with the
  /// `--names` option. By default, only the names of tagged terms are
  /// stored.
  getName(term: number): string {
    return this.termNames ? this.termNames[term] : (term & Term.Tagged) && this.tags[term >> 1].tag || String(term)
  }

  /// (Used by the output of the parser generator) @internal
  static deserialize(states: string,
                     stateData: string,
                     goto: string,
                     tags: readonly string[],
                     tokenData: string, tokenizers: (Tokenizer | number)[],
                     nested: [string, null | NestedGrammar, string, number, number][],
                     specializeTable: number, specializations: readonly {[term: string]: number}[],
                     tokenPrec: number,
                     skippedNodes: number,
                     termNames?: {[id: number]: string}) {
    let tokenArray = decodeArray(tokenData)
    return new Parser(decodeArray(states, Uint32Array), decodeArray(stateData),
                      decodeArray(goto), tags.map(tag => new Tag(tag)),
                      tokenizers.map(value => typeof value == "number" ? new TokenGroup(tokenArray, value) : value),
                      nested.map(([name, grammar, endToken, type, placeholder]) =>
                                   ({name, grammar, end: new TokenGroup(decodeArray(endToken), 0), type, placeholder})),
                      specializeTable, specializations.map(withoutPrototype),
                      tokenPrec, skippedNodes, termNames)
  }
}

function findOffset(data: Readonly<Uint16Array>, start: number, term: number) {
  for (let i = start, next; (next = data[i]) != Seq.End; i++)
    if (next == term) return i - start
  return -1
}

// Strip the prototypes from objects, so that they can safely be
// accessed as maps.
function withoutPrototype(obj: {}) {
  if (!(obj instanceof Object)) return obj
  let result: {[key: string]: any} = Object.create(null)
  for (let prop in obj) if (Object.prototype.hasOwnProperty.call(obj, prop)) result[prop] = (obj as any)[prop]
  return result
}

// Checks whether a node starts or ends with an error node, in which
// case we shouldn't reuse it.
function isFragile(node: Tree) {
  let doneStart = false, doneEnd = false, fragile = node.type == Term.Err
  if (!fragile) node.iterate(0, node.length, (_tag, _start, _end, type) => {
    return doneStart || (type == Term.Err ? fragile = doneStart = true : undefined)
  }, type => {
    doneStart = true
  })
  if (!fragile) node.iterate(node.length, 0, (_tag, _start, _end, type) => {
    return doneEnd || (type == Term.Err ? fragile = doneEnd = true : undefined)
  }, type => {
    doneEnd = true
  })
  return fragile
}
