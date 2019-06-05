import {TERM_TAGGED} from "./term"
import {Parser} from "./parse"

export interface ChangedRange {
  fromA: number
  toA: number
  fromB: number
  toB: number
}

export abstract class Subtree {
  abstract parent: Subtree | null

  abstract type: number
  abstract start: number
  abstract end: number

  get depth() {
    let d = 0
    for (let p = this.parent; p; p = p.parent) d++
    return d
  }

  get root(): Tree {
    let cx = this as Subtree
    while (cx.parent) cx = cx.parent
    return cx as Tree
  }

  abstract toString(parser?: Parser): string

  abstract iterate(from: number, to: number,
                   enter: (type: number, start: number, end: number) => any,
                   leave?: (type: number, start: number, end: number) => void): void

  abstract resolve(pos: number): Subtree

  abstract childBefore(pos: number): Subtree | null
  abstract childAfter(pos: number): Subtree | null
}

// Only the top-level object of this class is directly exposed to
// client code. Inspecting subtrees is done by allocating Subtree
// instances.
export class Tree extends Subtree {
  type!: number
  parent!: null

  constructor(readonly children: (Node | TreeBuffer)[],
              readonly positions: number[]) {
    super()
  }

  get start() { return 0 }

  toString(parser?: Parser) {
    return this.children.map(c => c.toString(parser)).join()
  }

  get length() {
    let last = this.children.length - 1
    return last < 0 ? 0 : this.positions[last] + this.children[last].length
  }

  get end() { return this.length }

  partial(start: number, end: number, offset: number, children: (Node | TreeBuffer)[], positions: number[]) {
    for (let i = 0; i < this.children.length; i++) {
      let from = this.positions[i]
      if (from >= end) break
      let child = this.children[i], to = from + child.length
      if (to > start) child.partial(start - from, end - from, offset + from, children, positions)
    }
  }

  unchanged(changes: readonly ChangedRange[]) {
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

  static empty = new Tree([], [])

  iterate(from: number, to: number,
          enter: (type: number, start: number, end: number) => any,
          leave?: (type: number, start: number, end: number) => void) {
    this.iterInner(from, to, 0, enter, leave)
  }

  // @internal
  iterInner(from: number, to: number, offset: number,
            enter: (type: number, start: number, end: number) => any,
            leave?: (type: number, start: number, end: number) => void) {
    for (let i = 0; i < this.children.length; i++) {
      let child = this.children[i], start = this.positions[i] + offset, end = start + child.length
      if (start > to) break
      if (end < from) continue
      child.iterInner(from, to, start, enter, leave)
    }
  }

  resolve(pos: number): Subtree {
    return this.resolveInner(pos, 0, this)
  }

  childBefore(pos: number): Subtree | null {
    return this.findChild(pos, -1, 0, this)
  }

  childAfter(pos: number): Subtree | null {
    return this.findChild(pos, 1, 0, this)
  }

  // @internal
  findChild(pos: number, side: number, start: number, parent: Subtree): Subtree | null {
    for (let i = 0; i < this.children.length; i++) {
      let childStart = this.positions[i] + start, select = -1
      if (childStart >= pos) {
        if (side < 0 && i > 0) select = i - 1
        else if (side > 0) select = i
        else break
      }
      if (select < 0 && (childStart + this.children[i].length > pos || side < 0 && i == this.children.length - 1))
        select = i
      if (select >= 0) {
        let child = this.children[select], childStart = this.positions[select] + start
        if (child.length == 0 && childStart == pos) continue
        if (child instanceof Node) {
          if (child.type & TERM_TAGGED) return new NodeSubtree(child, childStart, parent)
          return child.findChild(pos, side, childStart, parent)
        } else {
          let found = child.findIndex(pos, side, childStart, 0, child.buffer.length)
          if (found > -1) return new BufferSubtree(child, childStart, found, parent)
        }
      }
    }
    return null
  }

  // @internal
  resolveInner(pos: number, start: number, parent: Subtree): Subtree {
    let found = this.findChild(pos, 0, start, parent)
    return found ? found.resolve(pos) : parent
  }
}

Tree.prototype.type = -1
Tree.prototype.parent = null

export type SyntaxTree = TreeBuffer | Tree

export class Node extends Tree {
  constructor(readonly type: number,
              private _length: number,
              children: (Node | TreeBuffer)[],
              positions: number[]) {
    super(children, positions)
  }

  get length() { return this._length } // Because super class already has a getter

  toString(parser?: Parser) {
    let name = (this.type & TERM_TAGGED) == 0 ? null : parser ? parser.getTag(this.type) : this.type
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

  iterInner(from: number, to: number, offset: number,
            enter: (type: number, start: number, end: number) => any,
            leave?: (type: number, start: number, end: number) => void) {
    if ((this.type & TERM_TAGGED) == 0 ||
        enter(this.type, offset, offset + this.length) !== false) {
      super.iterInner(from, to, offset, enter, leave)
      if (leave && (this.type & TERM_TAGGED)) leave(this.type, offset, offset + this.length)
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

  toString(parser?: Parser) {
    let parts: string[] = []
    for (let index = 0; index < this.buffer.length;)
      index = this.childToString(index, parts, parser)
    return parts.join(",")
  }

  childToString(index: number, parts: string[], parser?: Parser): number {
    let type = this.buffer[index], count = this.buffer[index + 3]
    let result = parser ? parser.getTag(type)! : String(type)
    index += 4
    if (count) {
      let children: string[] = []
      for (let end = index + (count << 2); index < end;)
        index = this.childToString(index, children, parser)
      result += "(" + children.join(",") + ")"
    }
    parts.push(result)
    return index
  }

  partial(start: number, end: number, offset: number, children: (Node | TreeBuffer)[], positions: number[]) {
    if (start <= 0 && end >= this.length) {
      children.push(this)
      positions.push(offset)
    }    
  }

  unchanged(changes: readonly ChangedRange[]) {
    return changes.length ? Tree.empty : this
  }

  iterInner(from: number, to: number, offset: number,
            enter: (type: number, start: number, end: number) => any,
            leave?: (type: number, start: number, end: number) => void) {
    for (let index = 0; index < this.buffer.length;)
      index = this.iterChild(from, to, offset, index, enter, leave)
  }

  iterChild(from: number, to: number, offset: number, index: number,
            enter: (type: number, start: number, end: number) => any,
            leave?: (type: number, start: number, end: number) => void): number {
    let type = this.buffer[index++], start = this.buffer[index++] + offset,
        end = this.buffer[index++] + offset, count = this.buffer[index++]
    let endIndex = index + (count << 2)
    if (start > to) return this.buffer.length
    if (end >= from && enter(type, start, end) !== false) {
      while (index < endIndex) this.iterChild(from, to, offset, index, enter, leave)
    }
    return endIndex
  }

  findIndex(pos: number, side: number, start: number, from: number, to: number) {
    let lastI = -1
    for (let i = from, buf = this.buffer; i < to;) {
      let start1 = buf[i + 1] + start, end1 = buf[i + 2] + start
      let ignore = start1 == end1 && start1 == pos
      if (start1 >= pos) {
        if (side > 0 && !ignore) return i
        break
      }
      if (end1 > pos) return i
      if (!ignore) lastI = i
      i += 4 + (buf[i + 3] << 2)
    }
    return side < 0 ? lastI : -1
  }
}

class NodeSubtree extends Subtree {
  constructor(readonly node: Node,
              readonly start: number,
              readonly parent: Subtree) {
    super()
  }

  get type() { return this.node.type }

  get end() { return this.start + this.node.length }

  resolve(pos: number): Subtree {
    if (pos <= this.start || pos >= this.end)
      return this.parent.resolve(pos)
    return this.node.resolveInner(pos, this.start, this)
  }

  childBefore(pos: number): Subtree | null {
    return this.node.findChild(pos, -1, this.start, this)
  }

  childAfter(pos: number): Subtree | null {
    return this.node.findChild(pos, 1, this.start, this)
  }

  toString(parser?: Parser) { return this.node.toString(parser) }

  iterate(from: number, to: number,
          enter: (type: number, start: number, end: number) => any,
          leave?: (type: number, start: number, end: number) => void) {
    return this.node.iterInner(from, to, this.start, enter, leave)
  }
}

class BufferSubtree extends Subtree {
  constructor(readonly buffer: TreeBuffer,
              readonly bufferStart: number,
              readonly index: number,
              readonly parent: Subtree) {
    super()
  }

  get type() { return this.buffer.buffer[this.index] }
  get start() { return this.buffer.buffer[this.index + 1] + this.bufferStart }
  get end() { return this.buffer.buffer[this.index + 2] + this.bufferStart }

  private get endIndex() { return this.index + 4 + (this.buffer.buffer[this.index + 3] << 2) }

  childBefore(pos: number): Subtree | null {
    let index = this.buffer.findIndex(pos, -1, this.bufferStart, this.index + 4, this.endIndex)
    return index < 0 ? null : new BufferSubtree(this.buffer, this.bufferStart, index, this)
  }

  childAfter(pos: number): Subtree | null {
    let index = this.buffer.findIndex(pos, 1, this.bufferStart, this.index + 4, this.endIndex)
    return index < 0 ? null : new BufferSubtree(this.buffer, this.bufferStart, index, this)
  }

  iterate(from: number, to: number,
          enter: (type: number, start: number, end: number) => any,
          leave?: (type: number, start: number, end: number) => void) {
    this.buffer.iterChild(from, to, this.bufferStart, this.index, enter, leave)
  }

  resolve(pos: number): Subtree {
    if (pos <= this.start || pos >= this.end) return this.parent.resolve(pos)
    let found = this.buffer.findIndex(pos, 0, this.bufferStart, this.index + 4, this.endIndex)
    return found < 0 ? this : new BufferSubtree(this.buffer, this.bufferStart, found, this).resolve(pos)
  }

  toString(parser?: Parser) {
    let result: string[] = []
    this.buffer.childToString(this.index, result, parser)
    return result.join("")
  }
}
