import {Stack, StackContext, BADNESS_WILD, BADNESS_STABILIZING} from "./stack"
import {ParseState, ACTION_VALUE_MASK, REDUCE_FLAG} from "./state"
import {InputStream, StringStream, Tokenizer, TokenGroup} from "./token"
import {TERM_EOF, TERM_ERR, TERM_OTHER, TERM_TAGGED} from "./term"
import {DEFAULT_BUFFER_LENGTH, TERM_ID_MASK, GRAMMAR_ID_MASK, Tree, TreeBuffer, TagMap} from "lezer-tree"
import {decodeArray} from "./decode"

const verbose = typeof process != "undefined" && /\bparse\b/.test(process.env.LOG!)

export const SPECIALIZE = 0, EXTEND = 1

export type NestedGrammar = Parser | ((input: InputStream, stack: Stack) => null | {
  parse?: (input: InputStream) => Tree
  parser?: Parser
  filterEnd?: (endToken: string) => boolean
})

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

class CachedToken {
  start = -1
  end = -1
  term = -1
  extended = -1

  constructor(readonly tokenizer: Tokenizer) {}

  asError(pos: number, eof: number) {
    this.start = pos
    if (pos == eof) {
      this.term = TERM_EOF
      this.end = pos
    } else {
      this.term = TERM_ERR
      this.end = pos + 1
    }
    return this
  }
}

const dummyToken = new CachedToken(null as any)

class TokenCache {
  tokens: CachedToken[] = []
  mainToken: CachedToken = null as any as CachedToken

  actions: number[] = []

  getActions(stack: Stack, input: InputStream) {
    let actionIndex = 0
    let main: CachedToken | null = null
    let {tokenizers} = stack.cx.parser

    for (let i = 0; i < tokenizers.length; i++) {
      if (((1 << i) & stack.state.tokenizerMask) == 0) continue
      let tokenizer = tokenizers[i]
      let token = this.tokens.find(c => c.tokenizer == tokenizer)
      if (!token) this.tokens.push(token = new CachedToken(tokenizer))
      if (tokenizer.contextual || token.start != stack.inputPos) this.updateCachedToken(token, stack, input)

      let startIndex = actionIndex
      if (token.extended > -1) actionIndex = this.addActions(stack, token.extended, token.end, actionIndex)
      actionIndex = this.addActions(stack, token.term, token.end, actionIndex)
      if (actionIndex > startIndex) {
        main = token
        break
      }
      if (!main || token.term != TERM_ERR) main = token
    }

    if (this.actions.length > actionIndex) this.actions.length = actionIndex
    this.mainToken = main || dummyToken.asError(stack.inputPos, input.length)
    return this.actions
  }

  updateCachedToken(token: CachedToken, stack: Stack, input: InputStream) {
    token.extended = -1
    token.tokenizer.token(input.goto(stack.inputPos), stack)
    if (input.token > -1) {
      token.start = stack.inputPos
      token.end = input.tokenEnd
      token.term = input.token
      let {parser} = stack.cx
      let specIndex = findOffset(parser.data, parser.specializeTable, token.term)
      if (specIndex >= 0) {
        let found = parser.specializations[specIndex][input.read(token.start, token.end)]
        if (found != null) {
          if ((found & 1) == SPECIALIZE) token.term = found >> 1
          else token.extended = found >> 1
        }
      }
    } else {
      token.asError(stack.inputPos, input.length)
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
    let {state} = stack, {data} = stack.cx.parser
    for (let set = 0; set < 2; set++) {
      for (let i = set ? state.skip : state.actions, next; (next = data[i]) != TERM_ERR; i += 3) {
        if (next == token || (next == TERM_OTHER && index == 0))
          index = this.putAction(data[i + 1] | (data[i + 2] << 16), token, end, index)
      }
    }
    return index
  }
}

export type ParseOptions = {cache?: Tree | null, strict?: boolean, bufferLength?: number}

export class ParseContext {
  reused: Tree[] = []
  tokens: TokenCache
  stacks: Stack[]
  cache: CacheCursor | null
  strict: boolean

  // @internal
  constructor(parser: Parser,
              input: InputStream,
              {cache = null, strict = false, bufferLength = DEFAULT_BUFFER_LENGTH}: ParseOptions = {}) {
    // FIXME per stack context?
    this.tokens = new TokenCache
    this.stacks = [Stack.start(new StackContext(parser, bufferLength, input))]
    this.strict = strict
    this.cache = cache ? new CacheCursor(cache) : null
  }

  takeStack() {
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

  putStack(stack: Stack, strict = stack.badness < BADNESS_STABILIZING || stack.badness > BADNESS_WILD): boolean {
    let stacks = this.stacks
    for (let i = 0; i < stacks.length; i++) {
      let other = stacks[i]
      if ((strict || other.state == stack.state) && other.inputPos == stack.inputPos) {
        let diff = stack.badness - other.badness || (stack.badness < BADNESS_STABILIZING ? 0 : stack.stack.length - other.stack.length)
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

  get pos() { return this.stacks[0].inputPos }

  forceFinish() {
    let stack = this.stacks[0].split()
    while (!stack.state.accepting && stack.forceReduce()) {}
    return stack.toTree()
  }

  advance() {
    let stack = this.takeStack(), start = stack.inputPos, {input, parser} = stack.cx

    if (this.cache) {
      for (let cached = this.cache.nodeAt(start); cached;) {
        if ((cached.type & GRAMMAR_ID_MASK) != parser.id) continue
        let match = parser.getGoto(stack.state.id, cached.type & TERM_ID_MASK)
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

    let nest = stack.state.startNested
    maybeNest: if (nest > -1) {
      let {grammar, end: endToken, type, placeholder} = parser.nested[nest]
      let filterEnd = undefined, parse = null, nested
      if (!(grammar instanceof Parser)) {
        let query = grammar(input.goto(stack.inputPos), stack)
        if (!query) break maybeNest
        ;({parse, parser: nested, filterEnd} = query)
      } else {
        nested = grammar
      }
      let end = this.scanForNestEnd(stack, endToken, filterEnd)
      let clippedInput = stack.cx.input.clip(end)
      if (parse) {
        let node = parse(clippedInput)
        stack.useNode(new Tree(node.children, node.positions, node.type & TERM_TAGGED ? node.type : type | parser.id, end - stack.inputPos),
                      parser.getGoto(stack.state.id, placeholder, true))
        this.putStack(stack)
      } else {
        let newStack = Stack.start(new StackContext(nested!, stack.cx.maxBufferLength, clippedInput, stack), stack.inputPos)
        if (verbose) console.log(newStack + ` (nested)`)
        this.putStack(newStack)
      }
      return null
    }

    let defaultReduce = stack.state.defaultReduce
    if (defaultReduce > 0) {
      stack.reduce(defaultReduce)
      this.putStack(stack)
      if (verbose) console.log(stack + ` (via always-reduce ${parser.getName(defaultReduce & ACTION_VALUE_MASK)})`)
      return null
    }

    let actions = this.tokens.getActions(stack, input)
    for (let i = 0; i < actions.length;) {
      let action = actions[i++], term = actions[i++], end = actions[i++]
      let localStack = i == actions.length ? stack : stack.split()
      localStack.apply(action, term, end)
      if (verbose)
        console.log(localStack + ` (via ${(action & REDUCE_FLAG) == 0 ? "shift"
                     : `reduce of ${parser.getName(action & ACTION_VALUE_MASK)}`} for ${
        parser.getName(term)} @ ${start}${localStack == stack ? "" : ", split"})`)
      this.putStack(localStack, (action & REDUCE_FLAG) != 0)
    }
    if (actions.length > 0) return null

    // If we're here, the stack failed to advance normally

    if (start == input.length && (stack.state.accepting || this.stacks.length == 0)) {
      while (!stack.state.accepting && stack.forceReduce()) {}
      let tree = stack.toTree(), {parent} = stack.cx
      if (parent) {
        // This is a nested parse—add its result to the parent stack and
        // continue with that one.
        let parentParser = parent.cx.parser, info = parentParser.nested[parent.state.startNested]
        let node = new Tree(tree.children, tree.positions.map(p => p - parent!.inputPos),
                            info.type | parentParser.id, stack.inputPos - parent.inputPos)
        parent.useNode(node, parentParser.getGoto(parent.state.id, info.placeholder, true))
        if (verbose) console.log(parent + ` (via unnest ${parentParser.getName(info.type)})`)
        this.putStack(parent)
        return null
      } else {
        // Actual end of parse
        return stack.toTree()
      }
    }

    let {end, term} = this.tokens.mainToken
    if (!this.strict &&
        !(stack.badness > BADNESS_WILD && this.stacks.some(s => s.pos >= stack.inputPos && s.badness <= stack.badness))) {
      let inserted = stack.recoverByInsert(term, end)
      if (inserted) {
        if (verbose) console.log(inserted + " (via recover-insert)")
        this.putStack(inserted)
      }

      if (end == start) {
        if (start == input.length) return null
        end++
        term = TERM_ERR
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

  scanForNestEnd(stack: Stack, endToken: TokenGroup, filter?: ((token: string) => boolean)) {
    let input = stack.cx.input
    for (let pos = stack.inputPos; pos < input.length; pos++) {
      endToken.token(input.goto(pos), stack)
      if (input.token > -1 && (!filter || filter(input.read(pos, input.tokenEnd)))) return pos
    }
    return input.length
  }
}

let nextGrammarID = 0

export class Parser {
  constructor(readonly id: number,
              readonly states: readonly ParseState[],
              readonly data: Readonly<Uint16Array>,
              readonly goto: Readonly<Uint16Array>,
              readonly tags: TagMap<string>,
              readonly tokenizers: readonly Tokenizer[],
              readonly nested: readonly {grammar: NestedGrammar, end: TokenGroup, type: number, placeholder: number}[],
              readonly specializeTable: number,
              readonly specializations: readonly {[value: string]: number}[],
              readonly tokenPrecTable: number,
              readonly skippedNodes: number,
              readonly termNames: null | {[id: number]: string} = null) {}

  getName(term: number): string {
    return this.termNames ? this.termNames[term] : this.tags.get(term) || String(term)
  }

  parse(input: InputStream | string, options?: ParseOptions) {
    if (typeof input == "string") input = new StringStream(input)
    let cx = new ParseContext(this, input, options)
    for (;;) {
      let done = cx.advance()
      if (done) return done
    }
  }

  startParse(input: InputStream, options?: ParseOptions) {
    return new ParseContext(this, input, options)
  }

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

  hasAction(state: ParseState, terminal: number) {
    let data = this.data
    for (let set = 0; set < 2; set++) {
      for (let i = set ? state.skip : state.actions, next; (next = data[i]) != TERM_ERR; i += 3) {
        if (next == terminal || next == TERM_OTHER)
          return data[i + 1] | (data[i + 2] << 16)
      }
    }
    return 0
  }

  getRecover(state: ParseState, terminal: number) {
    for (let i = state.recover, next; (next = this.data[i]) != TERM_ERR; i += 2)
      if (next == terminal) return this.data[i + 1]
    return 0
  }

  anyReduce(state: ParseState) {
    if (state.defaultReduce > 0) return state.defaultReduce
    for (let i = state.actions;; i += 3) {
      if (this.data[i] == TERM_ERR) return 0
      let isReduce = this.data[i + 2]
      if (isReduce) return this.data[i + 1] | (isReduce << 16)
    }
  }

  isSkipped(term: number) {
    for (let i = this.skippedNodes, cur; (cur = this.data[i]) != TERM_ERR; i++)
      if (cur == term) return true
    return false
  }

  overrides(token: number, prev: number) {
    let iPrev = findOffset(this.data, this.tokenPrecTable, prev)
    return iPrev < 0 || findOffset(this.data, this.tokenPrecTable, token) < iPrev
  }

  tagMap<T>(values: {[name: string]: T}) {
    let content: (T | null)[] = []
    let tagArray = this.tags.grammars[this.id >> 16] || []
    for (let i = 0; i < tagArray.length; i++) {
      let tag = tagArray[i]!
      content.push(
        Object.prototype.hasOwnProperty.call(values, tag) ? values[tag] :
        tag[0] == '"' && Object.prototype.hasOwnProperty.call(values, JSON.parse(tag)) ? values[JSON.parse(tag)] : null)
    }
    let grammars = []
    grammars[this.id >> 16] = content
    return new TagMap<T>(grammars)
  }

  static deserialize(states: string, stateData: string, goto: string, tags: readonly string[],
                     tokenData: string, tokenizers: (Tokenizer | number)[], nested: [NestedGrammar, string, number, number][],
                     specializeTable: number, specializations: readonly {[term: string]: number}[],
                     tokenPrec: number,
                     skippedNodes: number,
                     termNames?: {[id: number]: string}) {
    let arr = decodeArray(states, Uint32Array), stateObjs: ParseState[] = []
    for (let i = 0, id = 0; i < arr.length;)
      stateObjs.push(new ParseState(id++, arr[i++], arr[i++], arr[i++], arr[i++], arr[i++], arr[i++], arr[i++]))
    let tokenArray = decodeArray(tokenData), id = Parser.allocateID()
    return new Parser(id, stateObjs, decodeArray(stateData), decodeArray(goto), TagMap.single(id, tags),
                      tokenizers.map(value => typeof value == "number" ? new TokenGroup(tokenArray, value) : value),
                      nested.map(([grammar, endToken, type, placeholder]) =>
                                   ({grammar, end: new TokenGroup(decodeArray(endToken), 0), type, placeholder})),
                      specializeTable, specializations.map(withoutPrototype),
                      tokenPrec, skippedNodes, termNames)
  }

  // FIXME Horrid module interop kludge needed when consuming parser packages through ts-node
  get default() { return this }

  static allocateID() { return (nextGrammarID++) << 16 }
}

function findOffset(data: Readonly<Uint16Array>, start: number, term: number) {
  for (let i = start, next; (next = data[i]) != TERM_ERR; i++)
    if (next == term) return i - start
  return -1
}

function withoutPrototype(obj: {}) {
  if (!(obj instanceof Object)) return obj
  let result: {[key: string]: any} = Object.create(null)
  for (let prop in obj) if (Object.prototype.hasOwnProperty.call(obj, prop)) result[prop] = (obj as any)[prop]
  return result
}

function isFragile(node: Tree) {
  let doneStart = false, doneEnd = false, fragile = node.type == TERM_ERR
  if (!fragile) node.iterate(0, node.length, type => {
    return doneStart || (type == TERM_ERR ? fragile = doneStart = true : undefined)
  }, type => {
    doneStart = true
  })
  if (!fragile) node.iterate(node.length, 0, type => {
    return doneEnd || (type == TERM_ERR ? fragile = doneEnd = true : undefined)
  }, type => {
    doneEnd = true
  })
  return fragile
}
