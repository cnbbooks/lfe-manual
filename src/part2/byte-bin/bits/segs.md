# Segments: The Atomic Unit

A segment is a sequence of bits in a binary. These sequences need not be aligned on byte boundaries—you can have a 3-bit segment followed by a 5-bit segment, and the system will pack them tightly together because it's not 1970 anymore and we have the technology to handle bits that don't come in groups of eight.

```lfe
lfe> (binary (5 (size 4)) (5 (size 4)))
#B(85)
```

Wait, what? Where did 85 come from?

Let's examine this with a magnifying glass and possibly a stiff drink: The integer 5 in binary is `0101`. We've asked for two 4-bit segments, each containing 5. So we get: `0101 0101`, which when interpreted as a byte is... 85 in decimal. Or if you're feeling hexadecimal, that's `0x55`. Or if you're feeling ASCII, that's "U".

The REPL, in its infinite helpfulness, notices that 85 is a printable ASCII character and shows us `<<"U">>` (in Erlang notation) or `#"U"` (in LFE notation). The bits haven't changed—only our perception of them has been subject to the REPL's well-meaning interference.

```lfe
lfe> (list-to-binary (integer-to-list 85))
#"U"
```

Yes, indeed. 85 is "U". This is the kind of thing you either find fascinating or deeply troubling, depending on your relationship with the arbitrary nature of character encodings and numeric representations.
