# Creating Bitstrings

Creating bitstrings is syntactically identical to creating binaries; you simply specify bit counts that don't sum to a multiple of 8:

```lfe
lfe> (binary ((42 (size 6))))
#B(42:6)

lfe> (binary ((1 (size 1)) (0 (size 1)) (1 (size 1))))
#B(5:3)  ; Binary 101 = decimal 5 in 3 bits

lfe> (binary ((15 (size 4)) (7 (size 3))))
#B(127:7)  ; 1111 111 in binary = 127 in 7 bits
```

## Why Non-Alignment Happens

Non-byte-aligned data occurs in the wild for several reasons:

1. **Compression schemes** often work at bit-level precision
2. **Hardware protocols** may pack flags and values without regard for byte boundaries
3. **Video codecs** use variable-length codes that don't align
4. **Huffman encoding** produces bit sequences of varying length

These are all perfectly valid reasons to abandon the comfortable world of byte-alignment, rather like how there are valid reasons to swim in the ocean at night despite it being dark and full of things that might nibble.
