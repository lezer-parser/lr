import {Stack, BADNESS_WILD, BADNESS_STABILIZING} from "./stack"
import {ParseState, REDUCE_TERM_MASK, REDUCE_FLAG, ACTION_SKIP} from "./state"
import {InputStream, Tokenizer, TokenGroup} from "./token"
import {TERM_EOF, TERM_ERR} from "./term"
import {DEFAULT_BUFFER_LENGTH, Tree, TreeBuffer, TagMap} from "lezer-tree"
import {decodeArray} from "./decode"

const verbose = typeof process != "undefined" && /\bparse\b/.test(process.env.LOG!)

export const SPECIALIZE = 0, EXTEND = 1

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
}

class TokenCache {
  tokens: CachedToken[] = []
  mainToken!: CachedToken

  actions: number[] = []

  constructor(readonly parser: Parser) {
    this.mainToken = null as any as CachedToken
  }

  getActions(stack: Stack, input: InputStream) {
    let actionIndex = 0
    let main: CachedToken | null = null

    for (let i = 0; i < this.parser.tokenizers.length; i++) {
      if (((1 << i) & stack.state.tokenizerMask) == 0) continue
      let tokenizer = this.parser.tokenizers[i]
      let token = this.tokens.find(c => c.tokenizer == tokenizer)
      if (!token) this.tokens.push(token = new CachedToken(tokenizer))
      if (tokenizer.contextual || token.start != stack.inputPos) this.updateCachedToken(token, stack, input)

      let startIndex = actionIndex
      if (token.extended > -1) actionIndex = this.addActions(stack.state, token.extended, token.end, actionIndex)
      actionIndex = this.addActions(stack.state, token.term, token.end, actionIndex)
      if (actionIndex > startIndex) {
        main = token
        break
      }
      if (!main || token.term != TERM_ERR) main = token
    }

    if (this.actions.length > actionIndex) this.actions.length = actionIndex
    this.mainToken = main!
    return this.actions
  }

  updateCachedToken(token: CachedToken, stack: Stack, input: InputStream) {
    token.start = stack.inputPos
    token.extended = -1
    token.tokenizer.token(input.goto(stack.inputPos), stack)
    if (input.token > -1) {
      token.end = input.tokenEnd
      token.term = input.token
      let specIndex = findOffset(this.parser.data, this.parser.specializeTable, token.term)
      if (specIndex >= 0) {
        let found = this.parser.specializations[specIndex][input.read(token.start, token.end)]
        if (found != null) {
          if ((found & 1) == SPECIALIZE) token.term = found >> 1
          else token.extended = found >> 1
        }
      }
    } else if (stack.inputPos == input.length) {
      token.term = TERM_EOF
      token.end = token.start
    } else {
      token.term = TERM_ERR
      token.end = token.start + 1
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

  addActions(state: ParseState, token: number, end: number, index: number) {
    let data = this.parser.data
    for (let i = state.actions, next; (next = data[i]) != TERM_ERR; i += 3) {
      if (next == token) index = this.putAction(data[i + 1] | (data[i + 2] << 16), token, end, index)
    }
    if (findOffset(data, state.skip, token) > -1) index = this.putAction(ACTION_SKIP, token, end, index)
    return index
  }
}

export type ParseOptions = {cache?: Tree | null, strict?: boolean, bufferLength?: number}

export class ParseContext {
  reused: Tree[] = []
  tokens: TokenCache
  stacks: Stack[]
  cache: CacheCursor | null
  maxBufferLength: number
  strict: boolean

  // @internal
  constructor(readonly parser: Parser,
              readonly input: InputStream,
              {cache = null, strict = false, bufferLength = DEFAULT_BUFFER_LENGTH}: ParseOptions = {}) {
    this.tokens = new TokenCache(parser)
    this.stacks = [Stack.start(this)]
    this.maxBufferLength = bufferLength
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
    let stack = this.takeStack(), start = stack.inputPos

    if (this.cache) {
      for (let cached = this.cache.nodeAt(start); cached;) {
        let match = this.parser.getGoto(stack.state.id, cached.type)
        if (match > -1 && !isFragile(this.parser, cached)) {
          stack.useCached(cached, this.parser.states[match])
          if (verbose) console.log(stack + ` (via reuse of ${this.parser.getName(cached.type)})`)
          this.putStack(stack)
          return null
        }
        if (cached.children.length == 0 || cached.positions[0] > 0) break
        let inner = cached.children[0]
        if (inner instanceof Tree) cached = inner
        else break
      }
    }

    let defaultReduce = stack.state.defaultReduce
    if (defaultReduce > 0) {
      stack.reduce(defaultReduce)
      this.putStack(stack)
      if (verbose) console.log(stack + ` (via always-reduce ${this.parser.getName(defaultReduce & REDUCE_TERM_MASK)})`)
      return null
    }

    let actions = this.tokens.getActions(stack, this.input)
    for (let i = 0; i < actions.length;) {
      let action = actions[i++], term = actions[i++], end = actions[i++]
      let localStack = i == actions.length ? stack : stack.split()
      localStack.apply(action, term, end)
      if (verbose)
        console.log(localStack + ` (via ${(action & REDUCE_FLAG) == 0 ? "shift"
                     : `reduce of ${this.parser.getName(action & REDUCE_TERM_MASK)}`} for ${
        this.parser.getName(term)} @ ${start}${localStack == stack ? "" : ", split"})`)
      this.putStack(localStack, (action & REDUCE_FLAG) != 0)
    }
    if (actions.length > 0) return null

    // If we're here, the stack failed to advance normally

    if (start == this.input.length && (stack.state.accepting || this.stacks.length == 0)) {
      while (!stack.state.accepting && stack.forceReduce()) {}
      return stack.toTree()
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
        if (start == this.input.length) return null
        end++
        term = TERM_ERR
      }
      stack.recoverByDelete(term, end)
      if (verbose) console.log(stack + " (via recover-delete)")
      this.putStack(stack)
    } else if (!this.stacks.length) {
      // Only happens in strict mode
      throw new SyntaxError("No parse at " + start + " with " + this.parser.getName(term) + " (stack is " + stack + ")")
    }
    return null
  }
}

export class Parser {
  constructor(readonly states: readonly ParseState[],
              readonly data: Readonly<Uint16Array>,
              readonly goto: Readonly<Uint16Array>,
              readonly tags: TagMap<string>,
              readonly tokenizers: readonly Tokenizer[],
              readonly maxRepeated: number,
              readonly specializeTable: number,
              readonly specializations: readonly {[value: string]: number}[],
              readonly tokenPrecTable: number,
              readonly skippedNodes: number,
              readonly termNames: null | {[id: number]: string} = null) {}

  getName(term: number): string {
    return this.termNames ? this.termNames[term] : this.tags.get(term) || String(term)
  }

  parse(input: InputStream, options?: ParseOptions) {
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
    for (let i = state.actions, next; (next = data[i]) != TERM_ERR; i += 3) {
      if (next == terminal) return data[i + 1] + (data[i + 2] << 16)
    }
    if (findOffset(this.data, state.skip, terminal) > -1) return ACTION_SKIP
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
    for (let i = 0; i < this.tags.content.length; i++) {
      let tag = this.tags.content[i]!
      content.push(
        Object.prototype.hasOwnProperty.call(values, tag) ? values[tag] :
        tag[0] == '"' && Object.prototype.hasOwnProperty.call(values, JSON.parse(tag)) ? values[JSON.parse(tag)] : null)
    }
    return new TagMap<T>(content)
  }

  static deserialize(states: string, stateData: string, goto: string, tags: readonly string[],
                     tokenData: string, tokenizers: (Tokenizer | number)[],
                     maxRepeated: number,
                     specializeTable: number, specializations: readonly {[term: string]: number}[],
                     tokenPrec: number,
                     skippedNodes: number,
                     termNames?: {[id: number]: string}) {
    let arr = decodeArray(states, Uint32Array), stateObjs: ParseState[] = []
    for (let i = 0, id = 0; i < arr.length;)
      stateObjs.push(new ParseState(id++, arr[i++], arr[i++], arr[i++], arr[i++], arr[i++], arr[i++]))
    let tokenArray = decodeArray(tokenData)
    return new Parser(stateObjs, decodeArray(stateData), decodeArray(goto), new TagMap(tags),
                      tokenizers.map(value => typeof value == "number" ? new TokenGroup(tokenArray, value) : value),
                      maxRepeated, specializeTable, specializations.map(withoutPrototype),
                      tokenPrec, skippedNodes, termNames)
  }

  // FIXME Horrid module interop kludge needed when consuming parser packages through ts-node
  get default() { return this }
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


function isFragile(parser: Parser, node: Tree) {
  let doneStart = false, doneEnd = false, fragile = node.type == TERM_ERR
  if (!fragile) node.iterate(0, node.length, type => {
    return doneStart || (type == TERM_ERR ? fragile = doneStart = true : undefined)
  }, type => {
    if (!parser.isSkipped(type)) doneStart = true
  })
  if (!fragile) node.iterate(node.length, 0, type => {
    return doneEnd || (type == TERM_ERR ? fragile = doneEnd = true : undefined)
  }, type => {
    if (!parser.isSkipped(type)) doneEnd = true
  })
  return fragile
}
