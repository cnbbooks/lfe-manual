# bit-size: The Precise Measurer

For the exact number of bits:

```lfe
(bit-size bitstring)
```

This works on both binaries and bitstrings:

```lfe
lfe> (bit-size #B(1 2 3))
24
lfe> (let ((bs (binary (1 (size 9)))))
    (bit-size bs))
9
```

The distinction matters when you're working with protocols or formats that pack data at non-byte boundaries. Which is to say, nearly all modern compression formats and most networking protocols designed by people who believed that saving a single bit per packet, when multiplied by billions of packets, might add up to something worth optimizing.
