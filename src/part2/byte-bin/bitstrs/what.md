# What Bitstrings Are

A **bitstring** is a sequence of bits whose length is not necessarily divisible by 8. It is to a binary what a dolphin is to a submarine—related, existing in the same medium, but operating on fundamentally different principles.

Technically speaking:

- A **binary** has a bit length that is a multiple of 8 (it's byte-aligned)
- A **bitstring** has an arbitrary bit length (not necessarily byte-aligned)
- All binaries are bitstrings, but not all bitstrings are binaries

This is rather like how all squares are rectangles, but not all rectangles are squares—a geometric truth that becomes relevant in unexpected contexts, such as when you're trying to fit luggage into a vehicle or bits into a protocol.

## Identification in the REPL

The Erlang VM (and thus LFE) provides predicates to distinguish these entities:

```lfe
lfe> (set byte-aligned (binary (1 2 3)))
#B(1 2 3)
lfe> (is_binary byte-aligned)
true
lfe> (is_bitstring byte-aligned)
true  ; All binaries are bitstrings

lfe> (set not-aligned (binary ((1 (size 7)))))
#B(1:7)  ; Only 7 bits
lfe> (is_binary not-aligned)
false  ; Not byte-aligned
lfe> (is_bitstring not-aligned)
true   ; But still a valid bitstring
```

The notation `#B(1:7)` means "a bitstring containing the value 1 using 7 bits." This is how LFE indicates non-byte-aligned data—with a colon and the bit count.
