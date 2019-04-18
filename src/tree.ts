import {ANON_TERM} from "./term"
import {Parser} from "./parse"

export interface ChangedRange {
  fromA: number
  toA: number
  fromB: number
  toB: number
}

export const DEFAULT_BUFFER_LENGTH = 2048
export let MAX_BUFFER_LENGTH = DEFAULT_BUFFER_LENGTH

export function setBufferLength(len: number) { MAX_BUFFER_LENGTH = len }

// The size after which a buffer must be split on the next shift or
// childless reduce (to avoid overflowing the value index bits)
export const MAX_BUFFER_SIZE = 2**16 - 1

const BALANCE_BRANCH_FACTOR = 5

export class Tree {
  constructor(readonly children: (Node | TreeBuffer)[],
              readonly positions: number[]) {}

  toString(parser?: Parser) {
    return this.children.map(c => c.toString(parser)).join()
  }

  toNode(tag: number, length: number) {
    return new Node(tag, length, this.children, this.positions)
  }

  get length() {
    let last = this.children.length - 1
    return last < 0 ? 0 : this.positions[last] + this.children[last].length
  }

  // FIXME this should unwrap nodes with the matching tag to prevent repeated reuse from creating an unbalanced tree
  balanceRange(tag: number, from: number, to: number): Node {
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
          if (!(only instanceof Node) || only.tag != tag) only = new Node(tag, only.length, [only], [0])
          children.push(only)
        } else {
          children.push(this.balanceRange(tag, groupFrom, i))
        }
        positions.push(groupStart - start)
      }
    }
    return new Node(tag, length, children, positions)
  }

  balance(tag: number): Node {
    return this.balanceRange(tag, 0, this.children.length)
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

  static fromBuffer(buffer: number[], start: number) {
    let children: (Node | TreeBuffer)[] = [], positions: number[] = []
    TreeBuffer.build(buffer, 0, start, children, positions)
    return new Tree(children, positions)
  }

  static empty = new Tree([], [])

  cursor(parser: Parser) { return new NodeCursor(this, parser) }
}

export type SyntaxTree = TreeBuffer | Tree

export class Node extends Tree {
  constructor(readonly tag: number,
              private _length: number,
              children: (Node | TreeBuffer)[],
              positions: number[]) {
    super(children, positions)
  }

  get length() { return this._length } // Because super class already has a getter

  toString(parser?: Parser) {
    let name = (this.tag & ANON_TERM) ? null : parser ? parser.getTag(this.tag) : this.tag
    let children: string = this.children.map(c => c.toString(parser)).join()
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
  constructor(readonly buffer: Uint16Array) {}

  get nodeCount() { return this.buffer.length >> 2 }

  get length() { return this.buffer[this.buffer.length - 2] }

  static copy(source: number[], startIndex: number, endIndex: number, startOffset: number): TreeBuffer {
    let buffer = new Uint16Array(endIndex - startIndex)
    let i = buffer.length, pos = endIndex
    function build() {
      let count = source[--pos], to = source[--pos], from = source[--pos], tag = source[--pos]
      let toPos = pos - (count << 2)
      while (pos > toPos) build()
      buffer[--i] = count; buffer[--i] = to - startOffset; buffer[--i] = from - startOffset; buffer[--i] = tag
    }
    while (pos > startIndex) build()
    return new TreeBuffer(buffer)
  }

  static build(source: number[],
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
      children.push(TreeBuffer.copy(source, partIndex, pos, partOffset))
      positions.push(partOffset - startOffset)
      pos = partIndex
    }
    // The above loop adds the new children in inverse order, to this reverses them
    for (let i = startI, j = children.length - 1; j > i; i++, j--) {
      let tmp = children[i]; children[i] = children[j]; children[j] = tmp
      let tmp2 = positions[i]; positions[i] = positions[j]; positions[j] = tmp2
    }
  }

  toString(parser?: Parser) {
    let pos = 0
    let next = () => {
      let tag = this.buffer[pos], count = this.buffer[pos+3]
      pos += 4
      let children = "", end = pos + (count << 2)
      while (pos < end) children += (children ? "," : "") + next()
      return (parser ? parser.getTag(tag) : tag) + (children ? "(" + children + ")" : "")
    }
    let result = ""
    while (pos < this.buffer.length) result += (result ? "," : "") + next()
    return result
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

  cursor(parser: Parser) { return new NodeCursor(this, parser) }
}

export class NodeCursor {
  trees: Tree[] = []
  offset = [0]
  index = [0]
  leaf: TreeBuffer | null = null
  leafOffset = 0
  leafIndex = 0

  // Output properties
  start!: number
  end!: number
  tag!: string

  constructor(tree: SyntaxTree, readonly parser: Parser) {
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
          this.tag = this.parser.getTag(buf[index++])!
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
        if ((next.tag & ANON_TERM) == 0) {
          this.tag = this.parser.getTag(next.tag)!
          this.start = start
          this.end = start + next.length
          return true
        }
      }
    }
  }
}
