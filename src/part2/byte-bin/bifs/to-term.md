# binary-to-term: The Reanimator

The inverse of `term-to-binary`:

```lfe
(binary-to-term binary-containing-encoded-term)
```

This takes a binary in the Erlang external term format and reconstructs the original term:

```lfe
lfe> (let ((bin (term-to-binary '#(cat dog))))
    (binary-to-term bin))
#(cat dog)
```

The term that emerges is not "the same" as the original in any referential sense—it's a new term, freshly allocated, with all the same structure and values. But for all practical purposes, it's equivalent. It's the teleportation of data: the original is scanned, destroyed (well, garbage collected eventually), and an identical copy is created elsewhere. Philosophers might object. Data structures do not care.

It's worth noting that `binary-to-term` will cheerfully reconstruct any valid Erlang term, including atoms. If you're decoding binaries from untrusted sources, you might want to be careful about this, as atoms are never garbage collected and creating too many of them will eventually exhaust the atom table. The function has options to prevent atom creation during decoding, but that's a security discussion beyond our current scope, which is primarily concerned with the mechanics of how this all works rather than all the exciting ways it can go wrong if you're not paying attention.
