import {Stack, BADNESS_WILD, DEFAULT_BUFFER_LENGTH, setBufferLength} from "./stack"
import {ParseState, REDUCE_DEPTH_SIZE} from "./state"
import {InputStream, Tokenizer} from "./token"
import {TERM_EOF, TERM_ERR, TERM_TAGGED} from "./term"
import {Node, Tree, TreeBuffer, SyntaxTree} from "./tree"

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

    for (let tokenizer of stack.state.tokenizers) {
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
      let specIndex = this.parser.specialized.indexOf(token.term)
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
    for (let i = 0; i < state.actions.length; i += 2) if (state.actions[i] == token)
      index = this.putAction(state.actions[i + 1], token, end, index)
    for (let i = 0; i < state.skip.length; i += 2) if (state.skip[i] == token)
      index = this.putAction(state.skip[i + 1], token, end, index)
    return index
  }
}

export type ParseOptions = {cache?: SyntaxTree | null, strict?: boolean, bufferLength?: number}

export function parse(input: InputStream, parser: Parser, {
  cache = null,
  strict = false,
  bufferLength = DEFAULT_BUFFER_LENGTH
}: ParseOptions = {}): SyntaxTree {
  setBufferLength(bufferLength)
  let parses = [Stack.start(parser)]
  let cacheCursor = cache && new CacheCursor(cache)
  let tokens = new TokenCache(parser, input)

  parse: for (;;) {
    let stack = Stack.take(parses), start = stack.inputPos

    if (cacheCursor) {//  && !stack.state.ambiguous) { // FIXME implement fragility check
      for (let cached = cacheCursor.nodeAt(start); cached;) {
       let match = parser.getGoto(stack.state.id, cached.tag)
        if (match) {
          stack.useCached(cached, parser.states[match])
          if (verbose) console.log(stack + ` (via reuse of ${parser.getName(cached.tag)})`)
          stack.put(parses)
          continue parse
        }
        if (cached.children.length == 0 || cached.positions[0] > 0) break
        let inner = cached.children[0]
        if (inner instanceof Node) cached = inner
        else break
      }
    }

    if (stack.state.defaultReduce > 0) {
      stack.reduce(stack.state.defaultReduce)
      stack.put(parses)
      if (verbose) console.log(stack + " (via always-reduce)")
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

    // FIXME proper end condition check?
    if (start == input.length && (stack.state.accepting || parses.length == 0)) return stack.toTree()

    let {end, term} = tokens.mainToken
    if (!strict &&
        !(stack.badness > BADNESS_WILD && parses.some(s => s.pos >= stack.inputPos && s.badness <= stack.badness))) {
      let inserted = stack.recoverByInsert(term, end)
      if (inserted) {
        if (verbose) console.log(inserted + " (via recover-insert)")
        inserted.put(parses)
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
  readonly specializations: readonly {[value: string]: number}[]

  constructor(readonly states: readonly ParseState[],
              readonly tags: readonly string[],
              readonly repeats: readonly number[],
              readonly goto: Readonly<Uint16Array>,
              readonly specialized: readonly number[],
              specializations: readonly {[value: string]: number}[],
              readonly tokenPrec: number[],
              readonly termNames: null | {[id: number]: string} = null) {
    this.specializations = specializations.map(withoutPrototype)
  }

  getTag(term: number): string | null {
    return (term & TERM_TAGGED) ? this.tags[term >> 1] : null
  }

  getName(term: number): string {
    return this.termNames ? this.termNames[term] : this.getTag(term) || String(term)
  }

  // Term should be a repeat term
  getRepeat(term: number): number {
    return this.repeats[term >> 1]
  }

  parse(input: InputStream, options?: ParseOptions) {
    return parse(input, this, options)
  }

  getGoto(state: number, term: number) {
    let table = this.goto
    for (let pos = table[term];; pos += 2) {
      let next = table[pos]
      if (next == state || next == 0xffff) return table[pos + 1]
    }
  }
}

function withoutPrototype(obj: {}) {
  if (!(obj instanceof Object)) return obj
  let result: {[key: string]: any} = Object.create(null)
  for (let prop in obj) if (Object.prototype.hasOwnProperty.call(obj, prop)) result[prop] = (obj as any)[prop]
  return result
}
