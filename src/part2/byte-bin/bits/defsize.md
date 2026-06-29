# Default Sizes

If you don't specify a size, the system assumes defaults based on the type:

- Integers: 8 bits (1 byte)
- Floats: 64 bits (8 bytes)
- Binaries/bitstrings: The entire binary

```lfe
lfe> (binary 42)  ; Same as (binary (42 (size 8)))
#B(42)
lfe> (binary 1.5)  ; 64-bit float
#B(63 248 0 0 0 0 0 0)
```

That floating-point representation looks like line noise, doesn't it? That's IEEE 754 binary64 format, which is how computers have agreed to represent floating-point numbers ever since the IEEE standardization committee decided that "whatever hack your hardware happens to implement" was an insufficient specification for portable floating-point arithmetic. The details are fascinating if you're into that sort of thing, and utterly terrifying if you're trying to understand why `0.1 + 0.2` doesn't exactly equal `0.3` in most programming languages.
