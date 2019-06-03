import {TERM_TAGGED} from "./term"
import {Parser} from "./parse"

export interface ChangedRange {
  fromA: number
  toA: number
  fromB: number
  toB: number
}

export class Tree {
  constructor(readonly children: (Node | TreeBuffer)[],
              readonly positions: number[]) {}

  toString(parser?: Parser) {
    return this.children.map(c => c.toString(parser)).join()
  }

  get length() {
    let last = this.children.length - 1
    return last < 0 ? 0 : this.positions[last] + this.children[last].length
  }

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

  iterate(from: number, to: number,
          enter: (type: number, start: number, end: number) => any,
          leave?: (type: number, start: number, end: number) => void) {
    this.iterInner(from, to, 0, enter, leave)
  }

  resolve(pos: number): TreeContext {
    return new ArrayContext(this, 0, null).resolve(pos)
  }
}

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
    let pos = 0
    let next = () => {
      let type = this.buffer[pos], count = this.buffer[pos + 3]
      pos += 4
      let children = "", end = pos + (count << 2)
      while (pos < end) children += (children ? "," : "") + next()
      return (parser ? parser.getTag(type) : type) + (children ? "(" + children + ")" : "")
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

  unchanged(changes: readonly ChangedRange[]) {
    return changes.length ? Tree.empty : this
  }

  iterInner(from: number, to: number, offset: number,
            enter: (type: number, start: number, end: number) => any,
            leave?: (type: number, start: number, end: number) => void) {
    let pos = 0, buf = this.buffer
    let scan = () => {
      let type = buf[pos++], start = buf[pos++] + offset, end = buf[pos++] + offset, count = buf[pos++]
      let endPos = pos + (count << 2)
      if (end < from) {
        pos += count << 2
      } else if (start <= to) {
        if (enter(type, start, end) !== false) {
          while (pos < endPos) scan()
          if (leave) leave(type, start, end)
        } else {
          pos = endPos
        }
      } else {
        pos = buf.length
      }
    }
    while (pos < buf.length) scan()
  }
}

export abstract class TreeContext {
  _children: readonly TreeContext[] | null = null

  constructor(readonly parent: TreeContext | null) {}

  abstract type: number
  abstract start: number
  abstract end: number

  get depth() {
    let d = 0
    for (let p = this.parent; p; p = p.parent) d++
    return d
  }

  get root(): Tree {
    let cx = this as TreeContext
    while (cx.parent) cx = cx.parent
    return (cx as ArrayContext).tree
  }

  abstract resolve(pos: number): TreeContext

  // @internal
  abstract collectChildren(): TreeContext[]

  get children() {
    return this._children || (this._children = this.collectChildren())
  }

  abstract childBefore(pos: number): TreeContext | null
  abstract childAfter(pos: number): TreeContext | null
}

class ArrayContext extends TreeContext {
  constructor(readonly tree: Tree, // Might be a node, when not at the top
              readonly start: number,
              parent: TreeContext | null) {
    super(parent)
  }

  get type() { return this.tree instanceof Node ? this.tree.type : -1 }

  get end() { return this.start + this.tree.length }

  resolve(pos: number): TreeContext {
    if (pos <= this.start || pos >= this.end)
      return this.parent ? this.parent.resolve(pos) : this
    return ArrayContext.resolve(pos, this.tree, this.start, this)
  }

  collectChildren(): TreeContext[] {
    return ArrayContext.collect(this.tree, [], this.start, this)
  }

  childBefore(pos: number): TreeContext | null {
    return ArrayContext.findChild(pos, -1, this.tree, this.start, this)
  }

  childAfter(pos: number): TreeContext | null {
    return ArrayContext.findChild(pos, 1, this.tree, this.start, this)
  }

  static findChild(pos: number, side: number, tree: Tree, start: number, parent: TreeContext): TreeContext | null {
    for (let i = 0; i < tree.children.length; i++) {
      let childStart = tree.positions[i] + start, select = -1
      if (childStart >= pos) {
        if (side < 0 && i > 0) select = i - 1
        else if (side > 0) select = i
        else break
      }
      if (select < 0 && (childStart + tree.children[i].length > pos || side < 0 && i == tree.children.length - 1))
        select = i
      if (select >= 0) {
        let child = tree.children[select], childStart = tree.positions[select] + start
        if (child.length == 0 && childStart == pos) continue
        if (child instanceof Node) {
          if (child.type & TERM_TAGGED) return new ArrayContext(child, childStart, parent)
          return ArrayContext.findChild(pos, side, child, childStart, parent)
        } else {
          let found = BufferContext.findIndex(pos, side, child, childStart, 0, child.buffer.length)
          if (found > -1) return new BufferContext(child, childStart, found, parent)
        }
      }
    }
    return null
  }

  static resolve(pos: number, tree: Tree, start: number, parent: TreeContext): TreeContext {
    let found = ArrayContext.findChild(pos, 0, tree, start, parent)
    return found ? found.resolve(pos) : parent
  }

  static collect(tree: Tree, result: TreeContext[], treeStart: number, parent: TreeContext | null) {
    for (let i = 0; i < tree.children.length; i++) {
      let child = tree.children[i], start = tree.positions[i] + treeStart
      if (child instanceof TreeBuffer)
        BufferContext.collect(child, 0, child.buffer.length, result, start, parent)
      else if ((child.type & TERM_TAGGED) > 0)
        result.push(new ArrayContext(child, start, parent))
      else // Repeat node
        ArrayContext.collect(child, result, start, parent)
    }
    return result
  }
}

class BufferContext extends TreeContext {
  constructor(readonly buffer: TreeBuffer,
              readonly bufferStart: number,
              readonly index: number,
              parent: TreeContext | null) {
    super(parent)
  }

  get type() { return this.buffer.buffer[this.index] }
  get start() { return this.buffer.buffer[this.index + 1] + this.bufferStart }
  get end() { return this.buffer.buffer[this.index + 2] + this.bufferStart }

  private get endIndex() { return this.index + 4 + (this.buffer.buffer[this.index + 3] << 2) }

  childBefore(pos: number): TreeContext | null {
    let index = BufferContext.findIndex(pos, -1, this.buffer, this.bufferStart, this.index + 4, this.endIndex)
    return index < 0 ? null : new BufferContext(this.buffer, this.bufferStart, index, this)
  }

  childAfter(pos: number): TreeContext | null {
    let index = BufferContext.findIndex(pos, 1, this.buffer, this.bufferStart, this.index + 4, this.endIndex)
    return index < 0 ? null : new BufferContext(this.buffer, this.bufferStart, index, this)
  }

  resolve(pos: number): TreeContext {
    if (pos <= this.start || pos >= this.end)
      return this.parent ? this.parent.resolve(pos) : this
    let found = BufferContext.findIndex(pos, 0, this.buffer, this.bufferStart, this.index + 4, this.endIndex)
    return found < 0 ? this : new BufferContext(this.buffer, this.bufferStart, found, this).resolve(pos)
  }

  collectChildren(): TreeContext[] {
    return BufferContext.collect(this.buffer, this.index + 4, this.endIndex,
                                 [], this.bufferStart, this)
  }

  static findIndex(pos: number, side: number, buffer: TreeBuffer, start: number, from: number, to: number) {
    let lastI = -1
    for (let i = from, buf = buffer.buffer; i < to;) {
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

  static collect(buffer: TreeBuffer, from: number, to: number, result: TreeContext[], bufferStart: number, parent: TreeContext | null) {
    for (let i = from; i < to;) {
      let count = buffer.buffer[i + 3]
      result.push(new BufferContext(buffer, bufferStart, i, parent))
      i += 4 + (count << 2)
    }
    return result
  }
}
  
export class TagMap<T> {
  private content: (T | null)[] = []

  constructor(parser: Parser, values: {[name: string]: T}) {
    for (let i = 0; i < parser.tags.length; i++) {
      let tag = parser.tags[i]
      let found =
        Object.prototype.hasOwnProperty.call(values, tag) ? values[tag] :
        tag[0] == '"' && Object.prototype.hasOwnProperty.call(values, JSON.parse(tag)) ? values[JSON.parse(tag)] : null
      this.content.push(found)
    }
  }

  get(tag: number): T | null { return tag & 1 ? this.content[tag >> 1] : null }

  static empty = new TagMap<any>({tags: []} as any, {})
}
