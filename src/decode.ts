// See lezer-generator/src/encode.ts for comments about the encoding
// used here

import {Encode} from "./constants"

export function decodeArray<T extends {[i: number]: number} = Uint16Array>(
  input: string | T,
  Type: {new (n: number): T} = Uint16Array as any
): T {
  if (typeof input != "string") return input
  let array: T | null = null
  for (let pos = 0, out = 0; pos < input.length;) {
    let value = 0
    for (;;) {
      let next = input.charCodeAt(pos++), stop = false
      if (next == Encode.BigValCode) { value = Encode.BigVal; break }
      if (next >= Encode.Gap2) next--
      if (next >= Encode.Gap1) next--
      let digit = next - Encode.Start
      if (digit >= Encode.Base) { digit -= Encode.Base; stop = true }
      value += digit
      if (stop) break
      value *= Encode.Base
    }
    if (array) array[out++] = value
    else array = new Type(value)
  }
  return array!
}
