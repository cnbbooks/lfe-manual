# Sign

Integers can be signed or unsigned:

## unsigned

The default. All bits contribute to the magnitude:

```lfe
lfe> (let (((binary (x unsigned)) #B(255)))
    x)
255
```

Eight bits can represent 0 through 255 when unsigned, which is the full range of byte values and why bytes are often thought of as "0 to 255" rather than "-128 to 127," even though both ranges contain exactly 256 distinct values and are therefore equivalent in information-theoretic terms if not in convenience-of-interpretation terms.

## signed

The first bit becomes the sign bit (0 for positive, 1 for negative), using two's complement representation:

```lfe
lfe> (let (((binary (x signed)) #B(255)))
    x)
-1
```

The same bit pattern `11111111` means 255 when unsigned and -1 when signed. This is because two's complement representation uses that pattern for -1, which makes arithmetic simpler at the hardware level but means that interpreting integers requires knowing whether they're signed, which is one of those bits of context that must be maintained externally to the data itself.

This sign specification only matters for pattern matching (extracting values). When constructing binaries, the bit pattern is determined by the value and size:

```lfe
lfe> (binary (-1 (size 8)))
#B(255)  ; Two's complement representation of -1 in 8 bits
```
