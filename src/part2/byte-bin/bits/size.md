# Size Specifications

The size specification tells the system how many bits (or bytes, depending on the unit) a segment should occupy:

```lfe
(value (size N))
```

```lfe
lfe> (binary (42 (size 16)))
#B(0 42)
```

Here we're storing the number 42 in 16 bits (2 bytes). In big-endian order (which is the default, and we'll discuss endianness shortly), this becomes `0000000000101010` in binary, or `0x00 0x2A` in hexadecimal, which the REPL kindly displays as `#B(0 42)`.

The size can be larger than strictly necessary:

```lfe
lfe> (binary (42 (size 32)))
#B(0 0 0 42)
```

Four bytes for the number 42, because you asked for 32 bits and 32 bits you shall receive. The system pads with zeros in the most significant positions, because that's how you represent small positive integers in larger spaces without changing their value—unlike padding your résumé, where additional zeros in unexpected places tend to have different effects on value.

If you specify a size smaller than required, the system will truncate:

```lfe
lfe> (binary (258 (size 8)))  ; 258 = 0b100000010, needs 9 bits
#B(2)  ; Only the lower 8 bits: 0b00000010 = 2
```

The top bits are simply discarded, like baggage exceeding the weight limit on a particularly strict airline. Whether this is what you intended or a subtle bug that will manifest six months later in production is between you and whatever testing regime you've implemented. The system neither knows nor cares about your intentions—it simply does what you asked, which is all we can reasonably expect from deterministic computational systems.
