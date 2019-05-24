// See lezer-generator/src/encode.ts for comments about the encoding
// used here

const BIG_VAL = 0xffff, BIG_VAL_CODE = 126
const START = 32, GAP1 = 34 /* '"' */, GAP2 = 92 /* "\\" */
const BASE = 46 // (126 - 32 - 2) / 2

export function decodeArray<T extends {[i: number]: number} = Uint16Array>(input: string, Type: {new (n: number): T} = Uint16Array as any): T {
  let array: T | null = null
  for (let pos = 0, out = 0; pos < input.length;) {
    let value = 0
    for (;;) {
      let next = input.charCodeAt(pos++), stop = false
      if (next == BIG_VAL_CODE) { value = BIG_VAL; break }
      if (next >= GAP2) next--
      if (next >= GAP1) next--
      let digit = next - START
      if (digit >= BASE) { digit -= BASE; stop = true }
      value += digit
      if (stop) break
      value *= BASE
    }
    if (array) array[out++] = value
    else array = new Type(value)
  }
  return array!
}
