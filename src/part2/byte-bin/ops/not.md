# Bitwise NOT

NOT flips all bits. Due to two's complement representation of signed integers, `(bnot x)` equals `(- (- x) 1)`.

```lfe
lfe> (bnot #b00001111)
-16  ; #b11110000 in two's complement

lfe> (bnot 0)
-1

lfe> (bnot (bnot 42))
42  ; Double negation returns original
```

The result being negative is due to how integers are represented in memory. The leftmost bit indicates sign in two's complement, so inverting a small positive number produces a negative number.

## Practical Use: Creating Masks

```lfe
(defun create-mask (bit-position)
  "Create a mask with all bits set except bit-position."
  (bnot (bsl 1 bit-position)))

(defun clear-bit (byte bit-position)
  "Clear the bit at bit-position to 0."
  (band byte (create-mask bit-position)))
```

Testing:

```lfe
lfe> (create-mask 3)
-9  ; All bits set except bit 3

lfe> (clear-bit #b11111111 3)
247  ; #b11110111
```
