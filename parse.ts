import {Term, termTable, Grammar} from "../grammar/grammar"
import {Token, Tokenizer, State as TokState, InputStream} from "../grammar/token"
import {State, Shift, Reduce} from "../grammar/automaton"
import log from "../log"
import {takeFromHeap, addToHeap} from "./heap"

const BADNESS_DELETE = 100, BADNESS_RECOVER = 90
const BADNESS_STABILIZING = 50, BADNESS_WILD = 150 // Limits in between which stacks are less agressively pruned

const VALUE_INDEX_SIZE = 15, VALUE_INDEX_MASK = 2**VALUE_INDEX_SIZE - 1

let MAX_BUFFER_LENGTH = 2048
// The size after which a buffer must be split on the next shift or
// childless reduce (to avoid overflowing the value index bits)
const MAX_BUFFER_SIZE = VALUE_INDEX_MASK << 1

const BALANCE_BRANCH_FACTOR = 5

interface ChangedRange {
  fromA: number
  toA: number
  fromB: number
  toB: number
}

// (FIXME: this will go out of date before I know it, revisit at some
// point)
//
// Badness is a measure of how off-the-rails a given parse is. It is
// bumped when a recovery strategy is applied, and then reduced (by
// multiplication with a constant < 1) for every successful (real)
// token shifted.
//
// Stacks with a low badness are relatively credible parses that have
// shift matching the input in their recent history. Stacks with a
// high badness are deeply in the weeds and likely wrong. For each of
// these, we prune agressively by dropping stacks when another stack
// at the same position is looking better.
//
// For those in the BADNESS_STABILIZING - BADNESS_WILD range, we
// assume that they are in the process of trying to recover and allow
// a bunch of them to continue alongside each other to see which one
// works out better.
//
// Stacks with the same low badness score are likely to be valid GLR
// parsing branches, so in that case it's often a good idea to let
// both continue.
//
// When a stack fails to find an advancing action, recovery is only
// applied when its badness is < BADNESS_WILD, or no better parse
// exists at that point.

class Stack {
  constructor(readonly grammar: Grammar,
              // Holds state, pos, value stack pos (15 bits array index, 15 bits buffer index) triplets for all but the top state
              readonly stack: number[],
              public state: State,
              public pos: number,
              readonly values: (Node | number[])[],
              readonly valueInfo: number[],
              public badness: number) {}

  toString() {
    return "[" + this.stack.filter((_, i) => i % 3 == 0).concat(this.state.id).join(",") + "] " +
      this.values.map((v, i) => Array.isArray(v) ? Tree.fromBuffer(v, this.valueInfo[i], 0) : v).join(",")
  }

  static start(grammar: Grammar) {
    return new Stack(grammar, [], grammar.table[0], 0, [], [], 0)
  }

  reduceValue(stackIndex: number, stackOffset: number, start: number): Tree {
    let children: (Node | TreeBuffer)[] = [], positions: number[] = []
    for (let i = stackIndex; i < this.values.length; i++) {
      let value = this.values[i], valueInfo = this.valueInfo[i]
      if (Array.isArray(value)) {
        let startIndex = i == stackIndex ? stackOffset << 2 : 0
        if (startIndex < value.length)
          TreeBuffer.build(value, valueInfo, startIndex, start, children, positions)
        if (startIndex > 0) value.length = startIndex
      } else {
        children.push(value)
        positions.push(valueInfo - start)
      }
    }
    let newLen = stackIndex + (stackOffset ? 1 : 0)
    if (this.values.length > newLen)
      this.values.length = this.valueInfo.length = newLen
    return new Tree(children, positions)
  }

  pushState(state: State, start: number) {
    let valueIndex
    if (this.values.length > 0) {
      let last = this.values[this.values.length - 1]
      valueIndex = last instanceof Node ? this.values.length : this.values.length - 1 + ((last.length >> 2) << VALUE_INDEX_SIZE)
    } else {
      valueIndex = 0
    }
    this.stack.push(this.state.id, start, valueIndex)
    this.state = state
  }

  reduce(depth: number, name: Term) {
    if (depth == 0) {
      this.pushState(this.state.getGoto(name)!.target, this.pos)
      return
    }

    let base = this.stack.length - ((depth - 1) * 3)
    let start = this.stack[base - 2]
    if (name.tag) {
      let valueIndex = this.stack[base - 1]
      let stackIndex = valueIndex & VALUE_INDEX_MASK, stackOffset = valueIndex >> VALUE_INDEX_SIZE
      let top = this.values.length - 1
      let length = this.pos - start, pushed = false
      push: if (length <= MAX_BUFFER_LENGTH && stackIndex >= top) {
        let needNew = stackIndex > top, childCount = 0, buffer!: number[]
        if (!needNew) {
          buffer = this.values[top] as number[]
          if (!Array.isArray(buffer)) break push
          childCount = (buffer.length >> 2) - stackOffset
          if (buffer.length >= MAX_BUFFER_SIZE || this.valueInfo[top] != name.tableID) {
            if (childCount) break push
            needNew = true
          }
        }
        if (needNew) {
          this.values.push(buffer = [])
          this.valueInfo.push(name.tableID)
        }
        buffer.push(name.id, start, this.pos, childCount)
        pushed = true
      }
      if (!pushed) {
        this.values.push(this.reduceValue(stackIndex, stackOffset, start).toNode(name, length))
        this.valueInfo.push(start)
      }
    } else if (name.repeats && this.pos - start > MAX_BUFFER_LENGTH) {
      let valueIndex = this.stack[base - 1]
      let balanced = this.reduceValue(valueIndex & VALUE_INDEX_MASK, valueIndex >> VALUE_INDEX_SIZE, start).balance(name.repeats)
      this.values.push(balanced)
      this.valueInfo.push(start)
    }
    let baseState = this.grammar.table[this.stack[base - 3]]
    this.state = baseState.getGoto(name)!.target
    if (depth > 1) this.stack.length = base
  }

  shiftValue(term: Term, start: number, end: number, childCount = 0) {
    let last!: number[]
    if (this.values.length == 0 ||
        !Array.isArray(last = this.values[this.values.length - 1] as number[]) ||
        last.length >= MAX_BUFFER_SIZE ||
        this.valueInfo[this.valueInfo.length - 1] != term.tableID) {
      this.values.push(last = [])
      this.valueInfo.push(term.tableID)
    }
    if (term.error && last.length && last[last.length - 4] == term.id &&
        (start == end || last[last.length - 3] == start)) return
    last.push(term.id, start, end, childCount)
  }

  apply(action: Shift | Reduce, next: Term, nextStart: number, nextEnd: number, skipped: number[]) {
    if (action instanceof Reduce) {
      this.reduce(action.rule.parts.length, action.rule.name)
    } else { // Shift
      this.shiftSkipped(skipped)
      this.pushState(action.target, nextStart)
      this.pos = nextEnd
      if (next.tag) this.shiftValue(next, nextStart, nextEnd)
      this.badness = (this.badness >> 1) + (this.badness >> 2) // (* 0.75)
    }
  }

  useCached(value: Node, start: number, next: State) {
    this.pushState(next, start)
    this.values.push(value)
    this.valueInfo.push(start)
    this.pos = start + value.length
    this.badness >> 1 // FIXME
  }

  split() {
    return new Stack(this.grammar, this.stack.slice(), this.state, this.pos,
                     this.values.map(v => Array.isArray(v) ? v.slice() : v),
                     this.valueInfo.slice(), this.badness)
  }

  shiftSkipped(skipped: number[]) {
    for (let i = 0; i < skipped.length; i += 3)
      this.shiftValue(this.grammar.terms.terms[skipped[i + 2]], skipped[i], skipped[i + 1])
  }

  recoverByDelete(next: Term, nextStart: number, nextEnd: number, skipped: number[]) {
    this.shiftSkipped(skipped)
    if (next.tag) this.shiftValue(next, nextStart, nextEnd)
    this.shiftValue(this.grammar.terms.error, nextStart, nextEnd, next.tag ? 1 : 0)
    this.pos = nextEnd
    this.badness += BADNESS_DELETE
  }

  canRecover(next: Term) {
    // Scan for a state that has either a direct action or a recovery
    // action for next, without actually building up a new stack
    // FIXME this can continue infinitely without the i < 100 limit
    for (let top = this.state, rest = this.stack, offset = rest.length, i = 0; i < 100; i++) {
      if (top.terminals.some(a => a.term == next) ||
          top.recover.some(a => a.term == next)) return true
      // Find a way to reduce from here
      let term, n
      let direct = top.terminals.find(a => a instanceof Reduce) as Reduce, pos
      if (direct) {
        term = direct.rule.name
        n = direct.rule.parts.length
      } else if (pos = top.set.find(p => p.pos > 0)) { // FIXME store this in the run-time states
        term = pos.rule.name
        n = pos.pos
      } else {
        return false
      }
      if (n == 0) { // FIXME
        rest = rest.slice()
        rest.push(top.id, 0, 0)
        offset += 3
      } else {
        offset -= (n - 1) * 3
      }
      let goto = this.grammar.table[rest[offset - 3]].getGoto(term)
      if (!goto) return false
      top = goto.target
    }
    return false
  }

  recoverByInsert(next: Term, nextStart: number, nextEnd: number): Stack | null {
    if (!this.canRecover(next)) return null
    // Now that we know there's a recovery to be found, run the
    // reduces again, the expensive way, updating the stack

    let result = this.split()
    result.badness += BADNESS_RECOVER
    for (;;) {
      for (;;) {
        if (result.state.terminals.some(a => a.term == next)) return result
        let recover = result.state.recover.find(a => a.term == next)
        if (!recover) break
        let pos = result.pos
        result.pushState(recover.target, pos)
        result.shiftValue(this.grammar.terms.error, pos, pos)
      }
      
      let direct = result.state.terminals.find(a => a instanceof Reduce) as Reduce, pos
      if (direct) {
        result.reduce(direct.rule.parts.length, direct.rule.name)
      } else if (pos = result.state.set.find(p => p.pos > 0)) {
        // Force a reduce using this position
        result.shiftValue(this.grammar.terms.error, result.pos, result.pos)
        result.reduce(pos.pos, pos.rule.name)
      }
    }
  }

  toTree(): SyntaxTree {
    return this.reduceValue(0, 0, 0)
  }
}

function addStack(parses: Stack[], stack: Stack, strict = stack.badness < BADNESS_STABILIZING || stack.badness > BADNESS_WILD): boolean {
  for (let i = 0; i < parses.length; i++) {
    let other = parses[i]
    if ((strict || other.state == stack.state) && other.pos == stack.pos) {
      let diff = stack.badness - other.badness || (stack.badness < BADNESS_STABILIZING ? 0 : stack.stack.length - other.stack.length)
      if (diff < 0) { parses[i] = stack; return true }
      else if (diff > 0) return false
    }
  }
  addToHeap(parses, stack, compareStacks)
  return true
}

export class Tree {
  constructor(readonly children: (Node | TreeBuffer)[],
              readonly positions: number[]) {}

  toString(detailed?: boolean) {
    return this.children.map(c => c.toString(detailed)).join()
  }

  toNode(name: Term, length: number) {
    return new Node(name, length, this.children, this.positions)
  }

  get length() {
    let last = this.children.length - 1
    return last < 0 ? 0 : this.positions[last] + this.children[last].length
  }

  balanceRange(name: Term, from: number, to: number): Node {
    let start = this.positions[from], length = (this.positions[to - 1] + this.children[to - 1].length) - start
    if (from == to - 1 && start == 0) {
      let first = this.children[from]
      if (first instanceof Node) return first
    }
    let children = [], positions = []
    if (length <= MAX_BUFFER_LENGTH) {
      for (let i = from; i < to; i++) {
        let child = this.children[i]
        children.push(child)
        positions.push(this.positions[i] - start)
      }
    } else {
      let maxChild = Math.max(MAX_BUFFER_LENGTH, Math.ceil(length / BALANCE_BRANCH_FACTOR))
      for (let i = from; i < to;) {
        let groupFrom = i, groupStart = this.positions[i]
        i++
        for (; i < to; i++) {
          let nextEnd = this.positions[i] + this.children[i].length
          if (nextEnd - groupStart > maxChild) break
        }
        if (i == groupFrom + 1) {
          let only = this.children[groupFrom]
          if (!(only instanceof Node) || only.name != name) only = new Node(name, only.length, [only], [0])
          children.push(only)
        } else {
          children.push(this.balanceRange(name, groupFrom, i))
        }
        positions.push(groupStart - start)
      }
    }
    return new Node(name, length, children, positions)
  }

  balance(name: Term): Node {
    return this.balanceRange(name, 0, this.children.length)
  }

  partial(start: number, end: number, offset: number, children: (Node | TreeBuffer)[], positions: number[]) {
    for (let i = 0; i < this.children.length; i++) {
      let from = this.positions[i]
      if (from >= end) break
      let child = this.children[i], to = from + child.length
      if (to > start) child.partial(start - from, end - from, offset + from, children, positions)
    }
  }

  unchanged(changes: ReadonlyArray<ChangedRange>) {
    if (changes.length == 0) return this
    let children: (Node | TreeBuffer)[] = [], positions: number[] = []
    for (let i = 0, pos = 0, off = 0;; i++) {
      let next = i == changes.length ? null : changes[i]
      let nextPos = next ? next.fromA : this.length
      if (nextPos > pos) this.partial(pos, nextPos - 1 /* FIXME need a full token here */, off, children, positions)
      if (!next) break
      pos = next.toA
      off += (next.toB - next.fromB) - (next.toA - next.fromA)
    }
    return new Tree(children, positions)
  }

  static fromBuffer(buffer: number[], tableID: number, start: number) {
    let children: (Node | TreeBuffer)[] = [], positions: number[] = []
    TreeBuffer.build(buffer, tableID, 0, start, children, positions)
    return new Tree(children, positions)
  }

  static empty = new Tree([], [])

  get cursor() { return new NodeCursor(this) }
}

export type SyntaxTree = TreeBuffer | Tree

export class Node extends Tree {
  constructor(readonly name: Term,
              private _length: number,
              children: (Node | TreeBuffer)[],
              positions: number[]) {
    super(children, positions)
  }

  get length() { return this._length } // Because super class already has a getter

  toString(detailed?: boolean) {
    let name = this.name.tag || (detailed ? this.name.name : null)
    let children: string = this.children.map(c => c.toString(detailed)).join()
    return !name ? children : name + (children.length ? "(" + children + ")" : "")
  }

  partial(start: number, end: number, offset: number, children: (Node | TreeBuffer)[], positions: number[]) {
    if (start <= 0 && end >= this.length) {
      children.push(this)
      positions.push(offset)
    } else {
      super.partial(start, end, offset, children, positions)
    }
  }
}

// Tree buffers contain type,start,end,childCount quads for each node.
// The nodes are built in postfix order (with parent nodes being
// written after child nodes), but converted to prefix order when
// wrapped in a TreeBuffer.
export class TreeBuffer {
  constructor(readonly tableID: number, readonly buffer: Uint16Array) {}

  get nodeCount() { return this.buffer.length >> 2 }

  get length() { return this.buffer[this.buffer.length - 2] }

  static copy(source: number[], tableID: number, startIndex: number, endIndex: number, startOffset: number): TreeBuffer {
    let buffer = new Uint16Array(endIndex - startIndex)
    let i = buffer.length, pos = endIndex
    function build() {
      let count = source[--pos], to = source[--pos], from = source[--pos], tag = source[--pos]
      let toPos = pos - (count << 2)
      while (pos > toPos) build()
      buffer[--i] = count; buffer[--i] = to - startOffset; buffer[--i] = from - startOffset; buffer[--i] = tag
    }
    while (pos > startIndex) build()
    return new TreeBuffer(tableID, buffer)
  }

  static build(source: number[], tableID: number,
               startIndex: number, startOffset: number,
               children: (Node | TreeBuffer)[], positions: number[]) {
    let startI = children.length
    for (let pos = source.length; pos > startIndex;) {
      let partIndex = pos, partOffset!: number, partEnd = source[pos - 2]
      for (;;) {
        let count = source[partIndex - 1], newIndex = partIndex - 4 - (count << 2)
        let start = source[partIndex - 3]
        if ((newIndex < startIndex || partEnd - start > MAX_BUFFER_LENGTH) && partIndex < pos) break
        partOffset = start
        partIndex = newIndex
      }
      children.push(TreeBuffer.copy(source, tableID, partIndex, pos, partOffset))
      positions.push(partOffset - startOffset)
      pos = partIndex
    }
    // The above loop adds the new children in inverse order, to this reverses them
    for (let i = startI, j = children.length - 1; j > i; i++, j--) {
      let tmp = children[i]; children[i] = children[j]; children[j] = tmp
      let tmp2 = positions[i]; positions[i] = positions[j]; positions[j] = tmp2
    }
  }

  toString(detailed?: boolean) {
    let pos = 0
    let next = () => {
      let tag = this.buffer[pos], count = this.buffer[pos+3]
      pos += 4
      let children = "", end = pos + (count << 2)
      while (pos < end) children += (children ? "," : "") + next()
      return termTable[this.tableID].terms[tag].tag! + (children ? "(" + children + ")" : "")
    }
    let result = ""
    while (pos < this.buffer.length) result += (result ? "," : "") + next()
    return detailed ? "[" + result + "]" : result
  }

  partial(start: number, end: number, offset: number, children: (Node | TreeBuffer)[], positions: number[]) {
    if (start <= 0 && end >= this.length) {
      children.push(this)
      positions.push(offset)
    }    
  }

  unchanged(changes: ReadonlyArray<ChangedRange>) {
    return changes.length ? Tree.empty : this
  }

  get cursor() { return new NodeCursor(this) }
}

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

class NodeCursor {
  trees: Tree[] = []
  offset = [0]
  index = [0]
  leaf: TreeBuffer | null = null
  leafOffset = 0
  leafIndex = 0

  // Output properties
  start!: number
  end!: number
  tag!: Term

  constructor(tree: SyntaxTree) {
    if (tree instanceof Tree) this.trees.push(tree)
    else this.leaf = tree
  }

  next(): boolean {
    for (;;) {
      if (this.leaf) {
        let index = this.leafIndex, buf = this.leaf.buffer
        if (index == buf.length) {
          this.leaf = null
          continue
        } else {
          this.tag = termTable[this.leaf.tableID].terms[buf[index++]]
          this.start = this.leafOffset + buf[index++]
          this.end = this.leafOffset + buf[index++]
          this.leafIndex += 4
          return true
        }
      }
      let last = this.trees.length - 1
      if (last < 0) return false
      let top = this.trees[last], index = this.index[last]
      if (index == top.children.length) {
        this.trees.pop()
        this.offset.pop()
        this.index.pop()
        continue
      }
      let next = top.children[index]
      let start = this.offset[last] + top.positions[index]
      if (next instanceof TreeBuffer) {
        this.leaf = next
        this.leafIndex = 0
        this.leafOffset = start
        this.index[last]++
      } else {
        this.index[last]++
        this.trees.push(next)
        this.offset.push(start)
        this.index.push(0)
        if (next.name.tag) {
          this.tag = next.name
          this.start = start
          this.end = start + next.length
          return true
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

function compareStacks(a: Stack, b: Stack) {
  return a.pos - b.pos || a.badness - b.badness
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
