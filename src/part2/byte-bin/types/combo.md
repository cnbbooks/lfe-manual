# Combining Specifiers

You can combine these specifiers (except where combinations don't make sense):

```lfe
(binary (value (size N) (unit U) type sign endian))
```

For example:

```lfe
lfe> (binary (1000 (size 16) integer unsigned big))
#B(3 232)  ; 1000 = 0x03E8 in big-endian
lfe> (binary (1000 (size 16) integer unsigned little))
#B(232 3)  ; Same value in little-endian
lfe> (binary (-1000 (size 16) integer signed big))
#B(252 24)  ; -1000 in two's complement, big-endian
```

The order of specifiers doesn't matter (the parser sorts it out), but convention is:

1. Size specification
2. Unit (if needed)
3. Type
4. Sign (for integers)
5. Endianness (for multi-byte integers and floats)

This ordering makes the specifications read naturally: "16 bits of unsigned big-endian integer" becomes `(size 16) integer unsigned big`.

Of course, you can omit defaults:

```lfe
(binary (value (size 16)))  ; Equivalent to (size 16) integer unsigned big
```
