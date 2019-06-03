import {Stack, BADNESS_WILD, DEFAULT_BUFFER_LENGTH, setBufferLength} from "./stack"
import {ParseState, REDUCE_DEPTH_SIZE, ACTION_SKIP} from "./state"
import {InputStream, Tokenizer, TokenGroup} from "./token"
import {TERM_EOF, TERM_ERR, TERM_TAGGED} from "./term"
import {Node, Tree, TreeBuffer, SyntaxTree} from "./tree"
import {decodeArray} from "./decode"

const verbose = typeof process != "undefined" && /\bparse\b/.test(process.env.LOG!)

export const SPECIALIZE = 0, EXTEND = 1

class CacheCursor {
  trees: Tree[]
  start = [0]
  index = [0]
  nextStart: number = 0

  constructor(tree: SyntaxTree) { this.trees = tree instanceof Tree ? [tree] : [] }

  // `pos` must be >= any previously given `pos` for this cursor
  nodeAt(pos: number): Node | null {
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

  constructor(readonly parser: Parser, readonly input: InputStream) {
    this.mainToken = null as any as CachedToken
  }

  getActions(stack: Stack) {
    let actionIndex = 0
    let main: CachedToken | null = null

    for (let i = 0; i < this.parser.tokenizers.length; i++) {
      if (((1 << i) & stack.state.tokenizerMask) == 0) continue
      let tokenizer = this.parser.tokenizers[i]
      let token = this.tokens.find(c => c.tokenizer == tokenizer)
      if (!token) this.tokens.push(token = new CachedToken(tokenizer))
      if (tokenizer.contextual || token.start != stack.inputPos) this.updateCachedToken(token, stack)

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

  updateCachedToken(token: CachedToken, stack: Stack) {
    let input = this.input

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
      if (next == token) index = this.putAction(encode(data[i + 1], data[i + 2]), token, end, index)
    }
    if (findOffset(data, state.skip, token) > -1) index = this.putAction(ACTION_SKIP, token, end, index)
    return index
  }
}

export type ParseOptions = {cache?: SyntaxTree | null, strict?: boolean, bufferLength?: number}

export function parse(input: InputStream, parser: Parser, {
  cache = null,
  strict = false,
  bufferLength = DEFAULT_BUFFER_LENGTH
}: ParseOptions = {}): Tree {
  setBufferLength(bufferLength)
  let parses = [Stack.start(parser)]
  let cacheCursor = cache && new CacheCursor(cache)
  let tokens = new TokenCache(parser, input)

  parse: for (;;) {
    let stack = Stack.take(parses), start = stack.inputPos

    if (cacheCursor) {//  && !stack.state.ambiguous) { // FIXME implement fragility check
      for (let cached = cacheCursor.nodeAt(start); cached;) {
        let match = parser.getGoto(stack.state.id, cached.type)
        if (match > -1) {
          stack.useCached(cached, parser.states[match])
          if (verbose) console.log(stack + ` (via reuse of ${parser.getName(cached.type)})`)
          stack.put(parses)
          continue parse
        }
        if (cached.children.length == 0 || cached.positions[0] > 0) break
        let inner = cached.children[0]
        if (inner instanceof Node) cached = inner
        else break
      }
    }

    let defaultReduce = stack.state.defaultReduce
    if (defaultReduce > 0) {
      stack.reduce(defaultReduce)
      stack.put(parses)
      if (verbose) console.log(stack + ` (via always-reduce ${parser.getName(defaultReduce >> REDUCE_DEPTH_SIZE)})`)
      continue
    }

    let actions = tokens.getActions(stack)
    for (let i = 0; i < actions.length;) {
      let action = actions[i++], term = actions[i++], end = actions[i++]
      let localStack = i == actions.length ? stack : stack.split()
      localStack.apply(action, term, end)
      if (verbose) console.log(localStack + ` (via ${action < 0 ? "shift" : `reduce of ${parser.getName(action >> REDUCE_DEPTH_SIZE)}`} for ${
        parser.getName(term)} @ ${start}${localStack == stack ? "" : ", split"})`)
      localStack.put(parses, action >= 0)
    }
    if (actions.length > 0) continue

    // If we're here, the stack failed to advance normally

    if (start == input.length && (stack.state.accepting || parses.length == 0)) {
      while (!stack.state.accepting && stack.forceReduce()) {}
      return stack.toTree()
    }

    let {end, term} = tokens.mainToken
    if (!strict &&
        !(stack.badness > BADNESS_WILD && parses.some(s => s.pos >= stack.inputPos && s.badness <= stack.badness))) {
      let inserted = stack.recoverByInsert(term, end)
      if (inserted) {
        if (verbose) console.log(inserted + " (via recover-insert)")
        inserted.put(parses)
      }

      if (end == start) {
        if (start == input.length) continue
        end++
        term = TERM_ERR
      }
      stack.recoverByDelete(term, end)
      if (verbose) console.log(stack + " (via recover-delete)")
      stack.put(parses)
    } else if (!parses.length) {
      // Only happens in strict mode
      throw new SyntaxError("No parse at " + start + " with " + parser.getName(term) + " (stack is " + stack + ")")
    }
  }
}

export class Parser {
  constructor(readonly states: readonly ParseState[],
              readonly data: Readonly<Uint16Array>,
              readonly goto: Readonly<Uint16Array>,
              readonly tags: readonly string[],
              readonly tokenizers: readonly Tokenizer[],
              readonly repeatTable: number,
              readonly repeatCount: number,
              readonly specializeTable: number,
              readonly specializations: readonly {[value: string]: number}[],
              readonly tokenPrecTable: number,
              readonly skippedNodes: number,
              readonly termNames: null | {[id: number]: string} = null) {}

  getTag(term: number): string | null {
    return (term & TERM_TAGGED) ? this.tags[term >> 1] : null
  }

  getName(term: number): string {
    return this.termNames ? this.termNames[term] : this.getTag(term) || String(term)
  }

  // Term should be a repeat term
  getRepeat(term: number): number {
    return this.data[this.repeatTable + (term >> 1)]
  }

  parse(input: InputStream, options?: ParseOptions) {
    return parse(input, this, options)
  }

  getGoto(state: number, term: number, loose = false) {
    let table = this.goto
    for (let pos = table[term];;) {
      let groupTag = table[pos++], last = groupTag & 1
      let target = table[pos++]
      if (loose && last) return target
      for (let end = pos + (groupTag >> 1); pos < end; pos++)
        if (table[pos] == state) return target
      if (last) return -1
    }
  }

  hasAction(state: ParseState, terminal: number) {
    let data = this.data
    for (let i = state.actions, next; (next = data[i]) != TERM_ERR; i += 3) {
      if (next == terminal) return encode(data[i + 1], data[i + 2])
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
      let reduce = this.data[i + 1]
      if (reduce > 0) return reduce | (this.data[i + 2] << REDUCE_DEPTH_SIZE)
    }
  }

  repeats(term: number) {
    return (term >> 1) < this.repeatCount
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

  static deserialize(states: string, stateData: string, goto: string, tags: readonly string[],
                     tokenData: string, tokenizers: (Tokenizer | number)[],
                     repeatTable: number, repeatCount: number,
                     specializeTable: number, specializations: readonly {[term: string]: number}[],
                     tokenPrec: number,
                     skippedNodes: number,
                     termNames?: {[id: number]: string}) {
    let arr = decodeArray(states, Uint32Array), stateObjs: ParseState[] = []
    for (let i = 0, id = 0; i < arr.length;)
      stateObjs.push(new ParseState(id++, arr[i++], arr[i++], arr[i++], arr[i++], arr[i++], arr[i++]))
    let tokenArray = decodeArray(tokenData)
    return new Parser(stateObjs, decodeArray(stateData), decodeArray(goto), tags,
                      tokenizers.map(value => typeof value == "number" ? new TokenGroup(tokenArray, value) : value),
                      repeatTable, repeatCount, specializeTable, specializations.map(withoutPrototype),
                      tokenPrec, skippedNodes, termNames)
  }

  // FIXME Horrid module interop kludge needed when consuming parser packages through ts-node
  get default() { return this }
}

function encode(reduce: number, value: number) {
  return reduce ? reduce | (value << REDUCE_DEPTH_SIZE) : -value
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
