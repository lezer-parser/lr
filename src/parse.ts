import {Stack, BADNESS_WILD} from "./stack"
import {Parser, ParseOptions, ParseState, Tokenizer, InputStream,
        TERM_EOF, TERM_ERR, ANON_TERM} from "./parser"
import {Node, Tree, TreeBuffer, SyntaxTree, DEFAULT_BUFFER_LENGTH, setBufferLength} from "./tree"

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
  tokens: Token[] = []
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
    for (let tokenizer of state.tokenizers) {
      let token
      for (let i = 0; i < this.index; i++) if (this.tokenizers[i] == tokenizer) token = this.tokens[i]
      if (!token) {
        if (this.tokens.length <= this.index) this.tokens.push(token = new Token)
        else token = this.tokens[this.index]
        this.tokenizers[this.index++] = tokenizer

        let result = tokenizer(input.goto(pos))
        token.start = pos
        token.specialized = -1
        if (result < 0) {
          token.term = TERM_ERR
          token.end = pos + 1
        } else {
          token.term = result
          token.end = input.pos
          let specIndex = parser.specialized.indexOf(result)
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
    if (pos == input.length)
      actionIndex = this.addActions(state, TERM_EOF, pos, actionIndex)
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
    input.goto(pos)
    for (;;) {
      let start = input.pos, result = skip(input)
      if (result < 0 || input.pos == start) return this.skipTo = start
      if ((result & ANON_TERM) == 0) this.skipContent.push(start, input.pos, result)
    }
  }

  some() {
    for (let i = 0; i < this.index; i++) if (this.tokens[i].term != TERM_ERR) return this.tokens[i]
    return this.tokens[0]
  }
}

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
        let match = stack.state.getGoto(cached.tag)
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

    if (stack.state.alwaysReduce >= 0) {
      stack.reduce(stack.state.alwaysReduce)
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
    let  term = token.specialized > -1 ? token.specialized : token.term
    if (start == input.length) {
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
