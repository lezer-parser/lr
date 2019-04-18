// Odd numbers are anonymous terms. Even numbers have tags
export const ANON_TERM = 1

export const TERM_ERR = 0, TERM_EOF = 1

// Repeat terms get assigned a high id so that they are easy to recognize
export const FIRST_REPEAT_TERM = 2**16 + 1
