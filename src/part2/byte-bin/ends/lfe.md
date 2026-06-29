# The Endianness Specifiers in LFE

LFE provides three endianness specifiers:

```lfe
big      ; Big-endian (default)
little   ; Little-endian
native   ; Whatever your CPU prefers (determined at runtime)
```

## Default Behavior

If you don't specify endianness, LFE assumes **big-endian**. This is because network protocols universally use big-endian ordering, making it the sensible default for a language designed with telecommunications in mind.

```lfe
lfe> (binary ((12345 (size 16))))
#B(48 57)  ; Default: big-endian

lfe> (binary ((12345 (size 16) big)))
#B(48 57)  ; Explicit big-endian (same result)

lfe> (binary ((12345 (size 16) little)))
#B(57 48)  ; Little-endian (reversed)
```
