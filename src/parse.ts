import {Stack, BADNESS_WILD, DEFAULT_BUFFER_LENGTH, setBufferLength} from "./stack"
import {ParseState} from "./state"
import {Tokenizer, InputStream} from "./token"
import {TERM_EOF, TERM_ERR, TERM_TAGGED} from "./term"
import {Node, Tree, TreeBuffer, SyntaxTree} from "./tree"

const verbose = typeof process != "undefined" && /\bparse\b/.test(process.env.LOG!)

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

// FIXME use flat array
class Token {
  public start = 0
  public end = 0
  public term = -1
  public specialized = -1
}

class TokenCache {
  tokens: Token[] = [new Token, new Token, new Token, new Token]
  tokenizers: Tokenizer[] = []
  actions: number[] = []
  pos = 0
  index = 0

  skipPos = 0
  skipTo = 0
  skipContent: number[] = []
  skipType: ((input: InputStream) => number) | null = null

  actionsFor(parser: Parser, state: ParseState, input: InputStream, pos: number) {
    if (pos > this.pos) { this.index = 0; this.pos = pos }
    let actionIndex = 0
    if (pos == input.length) {
      actionIndex = this.addActions(state, TERM_EOF, pos, actionIndex)
    } else for (let tokenizer of state.tokenizers) {
      let token
      for (let i = 0; i < this.index; i++) if (this.tokenizers[i] == tokenizer) token = this.tokens[i]
      if (!token) {
        if (this.tokens.length <= this.index) this.tokens.push(token = new Token)
        else token = this.tokens[this.index]
        this.tokenizers[this.index++] = tokenizer

        tokenizer(input.goto(pos))
        token.start = pos
        token.specialized = -1
        if (input.token < 0) {
          token.term = TERM_ERR
          token.end = pos + 1
        } else {
          token.term = input.token
          token.end = input.tokenEnd
          let specIndex = parser.specialized.indexOf(token.term)
          if (specIndex >= 0) {
            let found = parser.specializations[specIndex][input.read(pos, token.end)]
            if (found != null) token.specialized = found
          }
        }
      }
      if (token.specialized > -1) {
        let initialIndex = actionIndex
        actionIndex = this.addActions(state, token.specialized, token.end, actionIndex)
        if (actionIndex > initialIndex) continue
      }
      if (token.term != TERM_ERR)
        actionIndex = this.addActions(state, token.term, token.end, actionIndex)
    }
    if (this.actions.length > actionIndex) this.actions.length = actionIndex
    return this.actions
  }

  addActions(state: ParseState, token: number, end: number, index: number) {
    for (let i = 0; i < state.actions.length; i += 2) if (state.actions[i] == token) {
      this.actions[index++] = state.actions[i + 1]
      this.actions[index++] = token
      this.actions[index++] = end
    }
    return index
  }

  updateSkip(skip: Tokenizer, input: InputStream, pos: number): number {
    if (pos == this.skipPos && skip == this.skipType) return this.skipTo
    this.skipType = skip
    this.skipPos = pos
    this.skipContent.length = 0
    for (;;) {
      // FIXME it's awkward that the tokenizer gets called again until
      // it doesn't advance—that'll usually duplicate work. Reconsider
      // skip grammars.
      skip(input.goto(pos))
      if (input.token < 0 || input.tokenEnd <= pos) return this.skipTo = pos
      if (input.token & TERM_TAGGED) this.skipContent.push(pos, input.tokenEnd, input.token)
      pos = input.tokenEnd
    }
  }

  some() {
    for (let i = 0; i < this.index; i++) if (this.tokens[i].term != TERM_ERR) return this.tokens[i]
    return this.tokens[0]
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
  let tokens = new TokenCache

  parse: for (;;) {
    let stack = Stack.take(parses)
    let start = tokens.updateSkip(stack.state.skip, input, stack.pos)

    if (cacheCursor) {//  && !stack.state.ambiguous) { // FIXME implement fragility check
      for (let cached = cacheCursor.nodeAt(start); cached;) {
        let match = parser.getGoto(stack.state.id, cached.tag)
        if (match) {
          stack.useCached(cached, start, parser.states[match])
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

    let actions = tokens.actionsFor(parser, stack.state, input, start)
    for (let i = 0; i < actions.length;) {
      let action = actions[i++], term = actions[i++], end = actions[i++]
      let localStack = i == actions.length ? stack : stack.split()
      localStack.apply(action, term, start, end, tokens.skipContent)
      if (verbose) console.log(localStack + ` (via ${action < 0 ? "shift" : "reduce"} for ${
        parser.getName(term)} @ ${start}${localStack == stack ? "" : ", split"})`)
      localStack.put(parses, action < 0)
    }
    if (actions.length > 0) continue

    // If we're here, the stack failed to advance normally

    let token = tokens.some()
    let term = token.specialized > -1 ? token.specialized : token.term
    // FIXME proper end condition check
    if (start == input.length && (stack.stack.length == 3 || parses.length == 0)) {
      stack.shiftSkipped(tokens.skipContent)
      return stack.toTree()
    }

    if (!strict &&
        !(stack.badness > BADNESS_WILD && parses.some(s => s.pos >= stack.pos && s.badness <= stack.badness))) {
      let inserted = stack.recoverByInsert(term, token.start, token.end)
      if (inserted) {
        if (verbose) console.log(inserted + " (via recover-insert)")
        inserted.put(parses)
      }

      stack.recoverByDelete(term, token.start, token.end, tokens.skipContent)
      if (verbose) console.log(stack + " (via recover-delete)")
      stack.put(parses)
    } else if (!parses.length) {
      // Only happens in strict mode
      throw new SyntaxError("No parse at " + token.start + " with " + parser.getName(term) + " (stack is " + stack + ")")
    }
  }
}

export class Parser {
  readonly specializations: readonly {[value: string]: number}[]

  constructor(readonly states: readonly ParseState[],
              readonly tags: readonly string[],
              readonly repeats: readonly number[],
              readonly taggedGoto: readonly (readonly number[])[],
              readonly untaggedGoto: readonly (readonly number[])[],
              readonly specialized: readonly number[],
              specializations: readonly {[value: string]: number}[],
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
    let table = (term & TERM_TAGGED ? this.taggedGoto : this.untaggedGoto)[term >> 1]
    for (let i = 0; i < table.length; i += 2) {
      if (table[i] < 0 || table[i] == state) return table[i + 1]
    }
    throw new Error(`Missing goto table entry for ${this.getName(term)} and state ${state}`)
  }
}

function withoutPrototype(obj: {}) {
  if (!(obj instanceof Object)) return obj
  let result: {[key: string]: any} = Object.create(null)
  for (let prop in obj) if (Object.prototype.hasOwnProperty.call(obj, prop)) result[prop] = (obj as any)[prop]
  return result
}
