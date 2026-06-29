# The Constraint of Divisibility by Eight

Here's a crucial cosmic law: when working with binaries (as opposed to bitstrings), the total bit count **must be evenly divisible by 8**. The universe, or at least the Erlang VM, insists on this.

```lfe
lfe> (binary ((42 (size 3) (unit 8))))
#B(0 0 42)  ; 3 × 8 = 24 bits = 3 bytes ✓

lfe> (binary ((42 (size 3) (unit 7))))
; exception error: badarg  ; 3 × 7 = 21 bits ✗ not divisible by 8
```
