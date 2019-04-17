import {Stack, BADNESS_WILD} from "./stack"
import {Parser, ParseOptions, ParseState, Tokenizer, InputStream,
        TERM_EOF, TERM_ERR, MAX_TAGGED_TERM} from "./parser"
import {Node, Tree, TreeBuffer, SyntaxTree, DEFAULT_BUFFER_LENGTH, setBufferLength} from "./tree"

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

class Token {
  public start = 0
  public end = 0
  public term = -1
  public specialized = -1
}

class TokenCache {
  tokens: Token[] = []
  tokenizers: Tokenizer[] = []
  pos = 0
  index = 0

  skipPos = 0
  skipTo = 0
  skipContent: number[] = []
  skipType: ((input: InputStream) => number) | null = null

  update(parser: Parser, tokenizers: ReadonlyArray<Tokenizer>, input: InputStream, pos: number) {
    if (pos > this.pos) { this.index = 0; this.pos = pos }
    tokenize: for (let tokenizer of tokenizers) {
      for (let i = 0; i < this.index; i++) if (this.tokenizers[i] == tokenizer) continue tokenize
      let token
      if (this.tokens.length <= this.index) this.tokens.push(token = new Token)
      else token = this.tokens[this.index]
      this.tokenizers[this.index++] = tokenizer

      let result = tokenizer(input.goto(pos))
      token.start = pos
      token.end = input.pos
      token.specialized = -1
      if (result >= 0) {
        token.term = result
        let specIndex = parser.specialized.indexOf(result)
        if (specIndex >= 0) {
          let found = parser.specializations[specIndex][input.read(pos, token.end)]
          if (found != null) token.specialized = found
        }
      } else if (input.next() < 0) {
        token.term = TERM_EOF
      } else {
        token.end++
        token.term = TERM_ERR
      }
    }
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
      if (result <= MAX_TAGGED_TERM) this.skipContent.push(start, input.pos, result)
    }
  }

  hasOtherMatch(state: ParseState, tokenIndex: number, sawEof: boolean) {
    for (let i = tokenIndex; i < this.index; i++) {
      let token = this.tokens[i]
      if (token.term == TERM_ERR || token.term == TERM_EOF && sawEof) continue
      if (token.specialized > -1 && state.hasAction(token.specialized) || state.hasAction(token.term))
        return true
    }
    return false
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
      stack.reduce(stack.state.alwaysReduce).put(parses)
      continue
    }

    tokens.update(parser, stack.state.tokenizers, input, start)

    let sawEof = false, state = stack.state, advanced = false
    for (let i = 0; i < tokens.index; i++) {
      let token = tokens.tokens[i]
      if (token.term == TERM_ERR) continue
      if (sawEof && token.term == TERM_EOF) continue
      for (let j = token.specialized ? 1 : 0; j >= 0; j--) {
        let term = j ? token.specialized : token.term
        for (let k = 0, actions = state.actions; k < actions.length; k += 2) if (actions[k] == term) {
          let action = actions[k + 1]
          if (term == TERM_EOF) sawEof = true
          advanced = true
          let localStack = stack
          if (stack.state.hasAction(token.term, k + 2) ||
              token.specialized >= 0 && stack.state.hasAction(token.specialized, k + 2) ||
              tokens.hasOtherMatch(state, i + 1, sawEof))
            localStack = stack.split()
          localStack.apply(action, term, token.start, token.end, tokens.skipContent).put(parses, action < 0)
          j = 0 // Don't try a non-specialized version of a token when the specialized one matches
        }
      }
    }
    if (advanced) continue

    // If we're here, the stack failed to advance normally

    let token = tokens.some()
    let  term = token.specialized > -1 ? token.specialized : token.term
    if (term == TERM_EOF) {
      stack.shiftSkipped(tokens.skipContent)
      return stack.toTree()
    }

    if (!strict &&
        !(stack.badness > BADNESS_WILD && parses.some(s => s.pos >= stack.pos && s.badness <= stack.badness))) {
      let inserted = stack.recoverByInsert(term, token.start, token.end)
      if (inserted) inserted.put(parses)

      stack.recoverByDelete(term, token.start, token.end, tokens.skipContent)
      stack.put(parses)
    } else if (!parses.length) {
      // Only happens in strict mode
      throw new SyntaxError("No parse at " + token.start + " with " + parser.getName(term) + " (stack is " + stack + ")")
    }
  }
}
