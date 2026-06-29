# Pattern Matching with Guards

You can combine binary pattern matching with guards for additional validation:

```lfe
(defun validate-packet
  ; Valid version 2 packet
  ([(binary ((#xDEAD (size 16) big)
             (2 (size 8))
             rest binary))]
   (when (>= (byte_size rest) 4))
   `#(ok ,rest))

  ; Valid version 1 packet (deprecated)
  ([(binary ((#xDEAD (size 16) big)
             (1 (size 8))
             rest binary))]
   `#(deprecated ,rest))

  ; Invalid packet
  ([_]
   '#(error invalid_packet)))
```

The guard `(when (>= (byte_size rest) 4))` ensures that version 2 packets have at least 4 bytes of payload—a sensible requirement that prevents confusion later.
