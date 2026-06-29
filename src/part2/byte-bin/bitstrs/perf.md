# Performance Characteristics

Bitstrings have some interesting performance properties:

1. **Creation**: Fast, similar to binary creation
2. **Appending**: Efficient when building up bitstrings sequentially
3. **Pattern matching**: Very efficient, compiled to optimized VM instructions
4. **Conversion to bytes**: Requires padding, which is a copy operation

In general, if your final output must be byte-aligned (as most I/O is), consider whether working in bitstrings throughout is necessary, or if you can work in bytes and only use bitstrings for specific sub-operations.
