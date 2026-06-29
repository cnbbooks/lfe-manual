# When Endianness Matters

Endianness is relevant **only** for multi-byte data types:

- **Integers** (when size > 8 bits)
- **Floats** (which are always multi-byte)
- **UTF-16 and UTF-32** (multi-byte character encodings)

It does **not** matter for:

- Single-byte integers
- UTF-8 (which is byte-oriented)
- Plain binaries (which are just sequences of bytes)

## Floating Point and Endianness

IEEE 754 floating-point numbers are also subject to endianness concerns:

```lfe
lfe> (set pi 3.14159265359)
3.14159265359

lfe> (binary ((pi float)))
#B(64 9 33 251 84 68 45 24)  ; Big-endian double

lfe> (binary ((pi float little)))
#B(24 45 68 84 251 33 9 64)  ; Little-endian double
```
