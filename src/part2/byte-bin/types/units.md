# The Unit Specifier

The unit specifier modifies how size is interpreted:

```lfe
(value (size N) (unit U))
```

The actual size in bits is `N * U`. The unit must be from 1 to 256:

```lfe
lfe> (binary (1 (size 2) (unit 8)))  ; 2 * 8 = 16 bits
#B(0 1)
lfe> (binary (1 (size 16) (unit 1)))  ; 16 * 1 = 16 bits
#B(0 1)
```

Both produce the same result, because `2 * 8` equals `16 * 1`. The unit specifier is primarily useful for:

1. **Enforcing alignment**: Binary segments default to `unit:8`, meaning sizes must be multiples of 8 bits
2. **Working with specific bit widths**: Video codecs often work with pixels or samples of specific bit widths
3. **Documentation**: `(size 2) (unit 8)` might be clearer than `(size 16)` if you're thinking in terms of "2 bytes"

The default unit values are:

- `integer`, `float`, `bitstring`: unit 1 (arbitrary bit sizes)
- `binary`, `bytes`: unit 8 (must be byte-aligned)

If you try to violate alignment constraints (like specifying a binary with a non-multiple-of-8 bit count), the system will object:

```lfe
lfe> (binary (#B(1 2) (size 15) binary))  ; 15 is not divisible by 8
; Error: the value Binary in size specification of binary segment is expected to be of type integer
```

The error message is trying to be helpful. It's not always successful at being helpful, but the attempt is appreciated.
