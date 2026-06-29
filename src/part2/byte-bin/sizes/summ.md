# Summary

| Type      | Default Size | Default Unit | Typical Usage                |
|-----------|--------------|--------------|------------------------------|
| integer   | 8            | 1            | Bit-level integer packing    |
| float     | 64           | 1            | IEEE 754 double precision    |
| binary    | all          | 8            | Byte-aligned binary data     |
| bitstring | all          | 1            | Arbitrary-length bit strings |

Remember: **Total bits = Size × Unit**, and for binaries, this product must be divisible by 8. It's not quite as strict as "always know where your towel is," but it's a rule you'll want to internalize nonetheless.
