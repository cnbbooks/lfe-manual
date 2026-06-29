# Performance Characteristics

Bitwise operations are:

- **Extremely fast**: Usually single CPU cycle instructions
- **Memory efficient**: No allocation needed
- **Deterministic**: O(1) time complexity

They're faster than:

- Arithmetic operations (though modern CPUs make these nearly as fast)
- Function calls
- Pattern matching
- Pretty much everything except register moves

This makes them ideal for:

- Hot paths in performance-critical code
- Bit flag manipulation
- Checksums and hash functions
- Low-level protocol parsing
