# Appending Bitstrings

One of the more useful operations is appending bitstrings, which works even when neither is byte-aligned:

```lfe
lfe> (binary ((#B(7:3) bitstring) (#B(15:4) bitstring)))
#B(127:7)  ; 111 + 1111 = 1111111 (7 bits total)

lfe> (binary ((#B(1:1) bitstring) (#B(0:1) bitstring) (#B(1:1) bitstring)))
#B(5:3)  ; 1 + 0 + 1 = 101 (3 bits total)
```

This is remarkably useful when building up variable-length encoded data, where each component might be any number of bits.
