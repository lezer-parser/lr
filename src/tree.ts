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
            enter: (tag: number, start: number, end: number) => any,
            leave?: (tag: number, start: number, end: number) => void) {
    for (let i = 0; i < this.children.length; i++) {
      let child = this.children[i], start = this.positions[i] + offset, end = start + child.length
      if (start > to) break
      if (end < from) continue
      child.iterInner(from, to, start, enter, leave)
    }
  }

  iterate(from: number, to: number,
          enter: (tag: number, start: number, end: number) => any,
          leave?: (tag: number, start: number, end: number) => void) {
    this.iterInner(from, to, 0, enter, leave)
  }

  resolve(pos: number): Context {
    return new TreeContext(this, 0, null).resolve(pos)
  }
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
    let name = (this.tag & TERM_TAGGED) == 0 ? null : parser ? parser.getTag(this.tag) : this.tag
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
            enter: (tag: number, start: number, end: number) => any,
            leave?: (tag: number, start: number, end: number) => void) {
    if ((this.tag & TERM_TAGGED) == 0 ||
        enter(this.tag, offset, offset + this.length) !== false) {
      super.iterInner(from, to, offset, enter, leave)
      if (leave && (this.tag & TERM_TAGGED)) leave(this.tag, offset, offset + this.length)
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
      let tag = this.buffer[pos], count = this.buffer[pos + 3]
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

  unchanged(changes: readonly ChangedRange[]) {
    return changes.length ? Tree.empty : this
  }

  iterInner(from: number, to: number, offset: number,
            enter: (tag: number, start: number, end: number) => any,
            leave?: (tag: number, start: number, end: number) => void) {
    let pos = 0, buf = this.buffer
    let scan = () => {
      let tag = buf[pos++], start = buf[pos++] + offset, end = buf[pos++] + offset, count = buf[pos++]
      let endPos = pos + (count << 2)
      if (end < from) {
        pos += count << 2
      } else if (start <= to) {
        if (enter(tag, start, end) !== false) {
          while (pos < endPos) scan()
          if (leave) leave(tag, start, end)
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

export abstract class Context {
  _children: readonly Context[] | null = null

  constructor(readonly parent: Context | null) {}

  abstract type: number
  abstract start: number
  abstract end: number

  get depth() {
    let d = 0
    for (let p = this.parent; p; p = p.parent) d++
    return d
  }

  abstract resolve(pos: number): Context

  // @internal
  abstract collectChildren(): Context[]

  get children() {
    return this._children || (this._children = this.collectChildren())
  }
}

class TreeContext extends Context {
  constructor(readonly tree: Tree, // Might be a node, when not at the top
              readonly start: number,
              parent: Context | null) {
    super(parent)
  }

  get type() { return this.tree instanceof Node ? this.tree.tag : -1 }

  get end() { return this.start + this.tree.length }

  resolve(pos: number): Context {
    if (pos <= this.start || pos >= this.end)
      return this.parent ? this.parent.resolve(pos) : this
    return TreeContext.resolve(pos, this.tree, this.start, this)
  }

  collectChildren(): Context[] {
    return TreeContext.collect(this.tree, [], this.start, this)
  }

  static resolve(pos: number, tree: Tree, start: number, parent: Context): Context {
    for (let i = 0; i < tree.children.length; i++) {
      let childStart = tree.positions[i] + start
      if (childStart >= pos) break
      let child = tree.children[i], childEnd = childStart + child.length
      if (childEnd > pos) {
        if (child instanceof Node) {
          if (child.tag & TERM_TAGGED) return new TreeContext(child, childStart, parent).resolve(pos)
          return TreeContext.resolve(pos, child, childStart, parent)
        } else {
          let found = BufferContext.resolveIndex(pos, child, childStart, 0, child.buffer.length)
          if (found > -1) return new BufferContext(child, childStart, found, parent).resolve(pos)
        }
      }
    }
    return parent
  }

  // @internal
  static collect(tree: Tree, result: Context[], treeStart: number, parent: Context | null) {
    for (let i = 0; i < tree.children.length; i++) {
      let child = tree.children[i], start = tree.positions[i] + treeStart
      if (child instanceof TreeBuffer)
        BufferContext.collect(child, 0, child.buffer.length, result, start, parent)
      else if ((child.tag & TERM_TAGGED) > 0)
        result.push(new TreeContext(child, start, parent))
      else // Repeat node
        TreeContext.collect(child, result, start, parent)
    }
    return result
  }
}

class BufferContext extends Context {
  constructor(readonly buffer: TreeBuffer,
              readonly bufferStart: number,
              readonly index: number,
              parent: Context | null) {
    super(parent)
  }

  get type() { return this.buffer.buffer[this.index] }
  get start() { return this.buffer.buffer[this.index + 1] }
  get end() { return this.buffer.buffer[this.index + 2] }

  private get endIndex() { return this.index + 4 + (this.buffer.buffer[this.index + 3] << 2) }

  resolve(pos: number): Context {
    if (pos <= this.start || pos >= this.end)
      return this.parent ? this.parent.resolve(pos) : this
    let found = BufferContext.resolveIndex(pos, this.buffer, this.bufferStart, this.index + 4, this.endIndex)
    return found < 0 ? this : new BufferContext(this.buffer, this.bufferStart, found, this).resolve(pos)
  }

  collectChildren(): Context[] {
    return BufferContext.collect(this.buffer, this.index + 4, this.endIndex,
                                 [], this.bufferStart, this)
  }

  // @internal
  static resolveIndex(pos: number, buffer: TreeBuffer, start: number, from: number, to: number) {
    for (let i = from, buf = buffer.buffer; i < to;) {
      let start1 = buf[i + 1] + start
      if (start1 >= pos) break
      let end1 = buf[i + 2] + start
      if (end1 > pos) return i
      i += 4 + (buf[i + 3] << 2)
    }
    return -1
  }

  // @internal
  static collect(buffer: TreeBuffer, from: number, to: number, result: Context[], bufferStart: number, parent: Context | null) {
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
