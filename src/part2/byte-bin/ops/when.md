# When to Use Bitwise Operators

Use bitwise operators when:

- Implementing bit flags or permission systems
- Optimizing arithmetic by powers of two
- Interfacing with hardware registers
- Implementing checksums (XOR is common)
- Creating bitmasks

Don't use them when:

- Regular arithmetic would be clearer
- The performance gain is negligible
- Code readability would suffer significantly

Remember: premature optimization is the root of all evil, but knowing when bit twiddling is appropriate is a sign of wisdom. Sometimes shifting left by 3 instead of multiplying by 8 matters. Most times it doesn't. The art lies in knowing which is which.
