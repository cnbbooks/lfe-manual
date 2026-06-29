# The Fundamental Symmetry

One of the more elegant properties of LFE's binary syntax is its perfect symmetry: the same syntax used to *construct* a binary can be used to *deconstruct* it through pattern matching. This is rather like how a proper towel can both dry you off and serve as a makeshift sail, blanket, or weapon, depending on which end you grab.

## Construction vs. Deconstruction

```lfe
; Construction: Building a binary
lfe> (set data (binary ((42 (size 8)) (314159 (size 32)))))
#B(42 0 4 203 47)

; Deconstruction: Pattern matching to extract values
lfe> (binary ((answer (size 8)) (pi-ish (size 32))) data)
#B(42 0 4 203 47)
lfe> answer
42
lfe> pi-ish
314159
```

The pattern `(binary ((answer (size 8)) (pi-ish (size 32))) data)` says: "Take the binary in `data`, extract 8 bits into `answer`, then extract 32 bits into `pi-ish`." The pattern matching will either succeed (binding the variables) or fail (crashing with appropriate alarm), much like attempting to teleport through a device that might not be entirely reliable.
