import {Term, termTable, Grammar} from "../grammar/grammar"
import {Token, Tokenizer, State as TokState, InputStream} from "../grammar/token"
import {State, Shift, Reduce} from "../grammar/automaton"
import log from "../log"
import {takeFromHeap, addToHeap} from "./heap"


class CacheCursor {
  trees: Tree[]
  start = [0]
  index = [0]
  nextStart: number = 0

  constructor(tree: SyntaxTree) { this.trees = tree instanceof Tree ? [tree] : [] }

  // `pos` must be >= any previously given `pos` for this cursor
  nodeAt(pos: number) {
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


class StringInput implements InputStream {
  pos = 0

  constructor(readonly string: string) {}

  next(): number {
    if (this.pos == this.string.length) return -1
    return this.string.charCodeAt(this.pos)
  }
  
  adv(n: number) {
    this.pos += (n < 0x10000 ? 1 : 2)
  }

  goto(n: number) { this.pos = n }

  read(from: number, to: number): string { return this.string.slice(from, to) }
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

  update(grammar: Grammar, tokenizers: ReadonlyArray<Tokenizer>, input: StringInput) {
    if (input.pos > this.pos) { this.index = 0; this.pos = input.pos }
    tokenize: for (let tokenizer of tokenizers) {
      for (let i = 0; i < this.index; i++) if (this.tokenizers[i] == tokenizer) continue tokenize
      let token
      if (this.tokens.length <= this.index) this.tokens.push(token = new Token)
      else token = this.tokens[this.index]
      this.tokenizers[this.index++] = tokenizer
      if (!tokenizer.simulate(input, token, grammar.terms.terms)) {
        if (input.next() < 0) {
          token.end = token.start
          token.term = grammar.terms.eof
        } else {
          token.end = token.start + 1
          token.term = grammar.terms.error
        }
      }
    }
  }

  updateSkip(grammar: Grammar, skip: (input: InputStream) => number, input: InputStream): number {
    if (input.pos == this.skipPos && skip == this.skipType) return this.skipTo
    this.skipType = skip
    this.skipPos = input.pos
    this.skipContent.length = 0
    for (;;) {
      let start = input.pos, result = skip(input)
      if (result < 0 || input.pos == start) return this.skipTo = start
      let term = grammar.terms.terms[result]
      if (term.tag) this.skipContent.push(start, input.pos, term.id)
    }
  }

  hasOtherMatch(state: State, tokenIndex: number, sawEof: boolean) {
    for (let i = tokenIndex; i < this.index; i++) {
      let token = this.tokens[i]
      if (token.term.error || token.term.eof && sawEof) continue
      if (token.specialized && state.terminals.some(a => a.term == token.specialized) ||
          state.terminals.some(a => a.term == token.term)) return true
    }
    return false
  }

  some() {
    for (let i = 0; i < this.index; i++) if (!this.tokens[i].term.error) return this.tokens[i]
    return this.tokens[0]
  }
}

function hasOtherMatchInState(state: State, actionIndex: number, token: Token) {
  for (let i = actionIndex; i < state.terminals.length; i++) {
    let term = state.terminals[i].term
    if (term == token.term || term == token.specialized) return true
  }
  return false
}

export type ParseOptions = {cache?: SyntaxTree | null, strict?: boolean, bufferLength?: number}

export function parse(input: string, grammar: Grammar, {cache = null, strict = false, bufferLength}: ParseOptions = {}): SyntaxTree {
  if (bufferLength != null) return withBufferLength(bufferLength, () => parseInner(input, grammar, cache, strict))
  return parseInner(input, grammar, cache, strict)
}


export function parseInner(inputText: string, grammar: Grammar, cache: SyntaxTree | null, strict: boolean): SyntaxTree {
  let verbose = log.parse
  let parses = [Stack.start(grammar)]
  let cacheCursor = cache && new CacheCursor(cache)
  let input = new StringInput(inputText)

  let tokens = new TokenCache

  parse: for (;;) {
    let stack = takeFromHeap(parses, compareStacks), pos = stack.pos
    input.goto(pos)

    let tokenizers = grammar.tokenTable[stack.state.id]
    if (tokenizers.length && tokenizers[0].skip) pos = tokens.updateSkip(grammar, tokenizers[0].skip, input)

    if (cacheCursor && !stack.state.ambiguous) { // FIXME this isn't robust
      for (let cached = cacheCursor.nodeAt(pos); cached;) {
        let match = stack.state.getGoto(cached.name!)
        if (match) {
          if (verbose) console.log("REUSE " + cached)
          stack.useCached(cached, pos, match.target)
          addStack(parses, stack)
          continue parse
        }
        if (cached.children.length == 0 || cached.positions[0] > 0) break
        let inner = cached.children[0]
        if (inner instanceof Node) cached = inner
        else break
      }
    }

    input.goto(pos)
    tokens.update(grammar, tokenizers, input)

    let sawEof = false, state = stack.state, advanced = false
    for (let i = 0; i < tokens.index; i++) {
      let token = tokens.tokens[i]
      if (token.term == grammar.terms.error) continue
      if (sawEof && token.term == grammar.terms.eof) continue
      for (let j = token.specialized ? 1 : 0; j >= 0; j--) {
        let term = j ? token.specialized : token.term
        for (let k = 0, actions = state.terminals; k < actions.length; k++) {
          let action = actions[k]
          if (action.term != term) continue
          if (term.eof) sawEof = true
          advanced = true
          let localStack = stack
          if (hasOtherMatchInState(state, k + 1, token) || tokens.hasOtherMatch(state, i + 1, sawEof))
            localStack = localStack.split()
          localStack.apply(action, term, token.start, token.end, tokens.skipContent)
          if (verbose) console.log(`${localStack} (via ${term} ${action}${localStack == stack ? "" : ", split"})`)
          addStack(parses, localStack, action instanceof Shift)
          j = 0 // Don't try a non-specialized version of a token when the specialized one matches
        }
      }
    }
    if (advanced) continue

    // If we're here, the stack failed to advance normally

    let token = tokens.some(), term = token.specialized || token.term
    if (term.eof) {
      stack.shiftSkipped(tokens.skipContent)
      return stack.toTree()
    }

    if (!strict &&
        !(stack.badness > BADNESS_WILD && parses.some(s => s.pos >= stack.pos && s.badness <= stack.badness))) {
      let inserted = stack.recoverByInsert(term, token.start, token.end)
      if (inserted) {
        if (verbose) console.log("insert to " + inserted)
        addStack(parses, inserted)
      }
      stack.recoverByDelete(term, token.start, token.end, tokens.skipContent)
      if (verbose) console.log("delete token " + term + ": " + stack)
      addStack(parses, stack)
    }
    if (!parses.length)
      throw new SyntaxError("No parse at " + token.start + " with " + term + " (stack is " + stack + ")")
  }
}

function withBufferLength<T>(len: number, f: () => T): T {
  let prev = MAX_BUFFER_LENGTH
  MAX_BUFFER_LENGTH = len
  try { return f() }
  finally { MAX_BUFFER_LENGTH = prev }
}
