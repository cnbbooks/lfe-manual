# Summary

Type specifiers transform bit syntax from "here are some bits" to "here are some bits that mean this specific thing in this specific format." They're the difference between having a pile of LEGOs and having an instruction manual for assembling them into something recognizable.

Most of the time, you'll use a subset of these capabilities:

- `(size N) integer` for fixed-width integer fields
- `float` for floating-point values
- `binary` for including other binaries
- `bitstring` when you need non-byte-aligned data

The full range of possibilities exists for the edge cases, for the protocols designed by committees, and for the situations where you absolutely must handle some exotic binary format that was designed in 1987 and can never be changed because it's embedded in hardware on satellites that we can't exactly update remotely.

The good news is that once you understand the system, it handles all these cases uniformly. The bad news is that you have to understand the system. But we're working on that, which is why you're reading this chapter instead of directly consulting the specification, which is accurate but reads like it was translated from Vogon through three other languages and then back into English by someone whose native language is actually mathematics.
