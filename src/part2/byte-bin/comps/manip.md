# Bit String Manipulation

Binary comprehensions work seamlessly with bitstrings (non-byte-aligned binary data):

```lfe
lfe> (set bitstring #B(#b10101100 #b11110000))  ; Two bytes
#B(172 240)

lfe> (list-comp ((<<(bit (size 1))>> (binary-gen (<= bitstring))))
                bit)
(1 0 1 0 1 1 0 0 1 1 1 1 0 0 0 0)  ; All 16 bits extracted
```

This extracts each individual bit. We can then flip them:

```lfe
lfe> (binary-comp ((<<(bit (size 1))>> (binary-gen (<= bitstring))))
                  (- 1 bit))
#B(83 15)  ; Bits flipped: 01010011 00001111
```

The byte values change from 172,240 to 83,15 because we've inverted every bit. This is the binary equivalent of a photographic negative.
