# Explicit Size Specifications

Of course, sometimes the defaults are as inappropriate as a Vogon's poetry reading at a wedding. When you need to specify exactly how many bits to use, the syntax is delightfully straightforward:

```lfe
Value:Size
```

Where `Size` is the number of bits (not bytes—we're working at the bit level now, where things get properly interesting).

## Size in Action

```lfe
lfe> (set tiny (binary (42 (size 4))))
#B(10 4)  ; That's 42 crammed into 4 bits, stored as 10100000 in binary
lfe> (bit_size tiny)
4

lfe> (set rgb (binary ((2 (size 5)) (61 (size 6)) (20 (size 5)))))
#B(23 180)  ; RGB color: 5 bits red, 6 bits green, 5 bits blue
lfe> (bit_size rgb)
16
```

Notice how `42` in 4 bits becomes something entirely different when unpacked as a full byte. This is because we're only using the lower 4 bits, and the binary syntax helpfully pads or aligns as needed. It's not quite as dramatic as a matter transference beam accident, but the principle of "things change when you squeeze them" remains valid.
