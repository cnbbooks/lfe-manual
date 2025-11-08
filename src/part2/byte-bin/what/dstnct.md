# The Binary/Bitstring Distinction

As mentioned in our introduction, binaries are the special case of bitstrings where the number of bits is evenly divisible by 8. A bitstring with 16 bits? That's a binary. A bitstring with 15 bits? That's still a bitstring, but it's not quite respectable enough to join the binary club. It's the data structure equivalent of showing up to a formal dinner in jeans and a t-shirt—perfectly valid, but people will notice.

The important point is that *most operations work identically on both*. Pattern matching doesn't care if you're byte-aligned or wandering off into the wilderness of arbitrary bit boundaries. The system handles both with equal grace, which is more than can be said for most programming languages' relationship with non-byte-aligned data.

You can verify what you're dealing with using the predicates `(binary? x)` and `(bitstring? x)`:

```lfe
lfe> (binary? #B(1 2 3))
true
lfe> (bitstring? #B(1 2 3))
true
lfe> (let ((b (binary (1 (size 9)))))  ; 9 bits
      (tuple (binary? b) (bitstring? b)))
#(false true)
```

Note that all binaries are bitstrings, but not all bitstrings are binaries. It's a classic is-a relationship, the kind that object-oriented programmers spend their entire careers thinking about but which here just... works, without requiring seventeen layers of inheritance and a PhD in type theory.
