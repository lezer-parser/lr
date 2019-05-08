import {Stack, BADNESS_WILD, DEFAULT_BUFFER_LENGTH, setBufferLength} from "./stack"
import {ParseState} from "./state"
import {TokenGroup, InputStream, token} from "./token"
import {TERM_EOF, TERM_ERR, TERM_TAGGED} from "./term"
import {Node, Tree, TreeBuffer, SyntaxTree} from "./tree"

const verbose = typeof process != "undefined" && /\bparse\b/.test(process.env.LOG!)

export const SPECIALIZE = 0, REPLACE = 1, EXTEND = 2

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

class TokenCache {
  cachedGroup = -1
  pos = 0
  start = 0
  end = 0
  // Latest read base token
  term = 0
  // Potential specialization for .term
  specialized = 0
  // Latest token, with specialization applied, used for recovery
  token = 0
  skipContent: number[] = []

  actions: number[] = []

  constructor(readonly parser: Parser, readonly input: InputStream) {}

  updateToken(stack: Stack) {
    let group = stack.state.tokenGroup, pos = stack.pos
    if (this.pos == pos && this.cachedGroup == group) return

    this.pos = pos
    this.cachedGroup = group
    this.specialized = -1
    if (this.skipContent.length) this.skipContent.length = 0

    for (;;) {
      if (pos == this.input.length) { // FIXME do call external tokenizers
        this.term = this.token = TERM_EOF
        this.start = this.end = pos
      } else {
        token(this.parser.tokenizer, this.input.goto(pos), stack)
        if (this.input.token < 0) {
          this.term = this.token = TERM_ERR
          this.start = pos
          this.end = pos + 1
        } else if (this.parser.tokenGroups[group].skip.includes(this.input.token)) {
          if (this.input.token & TERM_TAGGED)
            this.skipContent.push(pos, this.input.tokenEnd, this.input.token)
          pos = this.input.tokenEnd
          continue
        } else {
          this.term = this.token = this.input.token
          this.start = pos
          this.end = this.input.tokenEnd
          let specIndex = this.parser.specialized.indexOf(this.term)
          if (specIndex >= 0) {
            // FIXME use .term differently or define a new prop so that recovery uses the specialized token?
            let found = this.parser.specializations[specIndex][this.input.read(pos, this.end)]
            if (found != null) this.specialized = found
          }
        }
      }
      break
    }
  }

  actionsFor(stack: Stack) {
    let actionIndex = 0, state = stack.state
    maybeSpec: {
      if (this.specialized > -1) {
        let initialIndex = actionIndex
        actionIndex = this.addActions(state, this.specialized >> 2, this.end, actionIndex)
        let type = this.specialized & 3
        if (type == REPLACE || (type == SPECIALIZE && actionIndex > initialIndex)) {
          this.token = this.specialized >> 2
          break maybeSpec
        }
      }
      if (this.term != TERM_ERR)
        actionIndex = this.addActions(state, this.term, this.end, actionIndex)
    }
    if (this.actions.length > actionIndex) this.actions.length = actionIndex
    return this.actions
  }

  addActions(state: ParseState, token: number, end: number, index: number) {
    for (let i = 0; i < state.actions.length; i += 2) if (state.actions[i] == token) {
      this.actions[index++] = state.actions[i + 1]
      this.actions[index++] = token
      this.actions[index++] = end // FIXME always the same as this.end?
    }
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
    let stack = Stack.take(parses)
    tokens.updateToken(stack)
    let start = tokens.start

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

    let actions = tokens.actionsFor(stack)
    for (let i = 0; i < actions.length;) {
      let action = actions[i++], term = actions[i++], end = actions[i++]
      let localStack = i == actions.length ? stack : stack.split()
      localStack.apply(action, term, start, end, tokens.skipContent)
      if (verbose) console.log(localStack + ` (via ${action < 0 ? "shift" : "reduce"} for ${
        parser.getName(term)} @ ${start}${localStack == stack ? "" : ", split"})`)
      localStack.put(parses, action >= 0)
    }
    if (actions.length > 0) continue

    // If we're here, the stack failed to advance normally

    // FIXME proper end condition check
    if (start == input.length && (stack.stack.length == 3 || parses.length == 0)) {
      stack.shiftSkipped(tokens.skipContent)
      return stack.toTree()
    }

    if (!strict &&
        !(stack.badness > BADNESS_WILD && parses.some(s => s.pos >= stack.pos && s.badness <= stack.badness))) {
      let inserted = stack.recoverByInsert(tokens.token, start, tokens.end)
      if (inserted) {
        if (verbose) console.log(inserted + " (via recover-insert)")
        inserted.put(parses)
      }

      stack.recoverByDelete(tokens.token, start, tokens.end, tokens.skipContent)
      if (verbose) console.log(stack + " (via recover-delete)")
      stack.put(parses)
    } else if (!parses.length) {
      // Only happens in strict mode
      throw new SyntaxError("No parse at " + start + " with " + parser.getName(tokens.token) + " (stack is " + stack + ")")
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
              readonly tokenizer: readonly (readonly number[])[],
              readonly tokenPrec: number[],
              readonly tokenGroups: readonly TokenGroup[],
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
