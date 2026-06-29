# The Dual Nature of Size Variables

Here's a subtle but crucial point: in pattern matching, size specifications can use variables that were *previously bound* in the pattern:

```lfe
; This works - 'len' is bound before use:
(binary ((len (size 8)) (data (size len) binary)) input)

; This doesn't work - 'len' not yet bound:
(binary ((data (size len) binary) (len (size 8))) input)
```

The pattern matcher evaluates left to right, binding variables as it goes. You can only use a variable as a size specifier if it's been bound earlier in the same pattern. This is less of a limitation and more of a sensible design decision, preventing temporal paradoxes of the sort that plague time travelers and quantum physicists.
