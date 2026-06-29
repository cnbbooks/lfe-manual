# Size Queries

Two BIFs let you interrogate bitstrings about their dimensions:

```lfe
lfe> (set bits #B(1 2 3))
#B(1 2 3)
lfe> (bit_size bits)
24
lfe> (byte_size bits)
3

lfe> (set awkward #B(255:7))
#B(127:7)
lfe> (bit_size awkward)
7
lfe> (byte_size awkward)
1  ; Rounds up to nearest byte
```

Note that `byte_size` rounds up to the nearest byte. A 7-bit bitstring occupies 1 byte in memory (because memory is byte-addressable), but conceptually it's still 7 bits. This is similar to how ordering one egg at a restaurant might still result in being charged for a minimum order, despite only wanting one egg.
