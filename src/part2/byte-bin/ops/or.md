# Bitwise OR

The OR operation returns 1 if either bit is 1. It's the optimist, seeing potential everywhere and requiring only minimal commitment.

```lfe
lfe> (bor #b1010 #b1100)
14  ; #b1110 in binary

lfe> (bor 170 85)
255  ; 10101010 OR 01010101 = 11111111
```

## Practical Use: Setting Bits

OR is used to set specific bits to 1:

```lfe
(defun set-bit (byte bit-position)
  "Set the bit at bit-position to 1."
  (bor byte (bsl 1 bit-position)))

(defun combine-flags (flag-list)
  "Combine multiple flag bits."
  (lists:foldl (lambda (flag acc) (bor flag acc)) 0 flag-list))
```

Testing:

```lfe
lfe> (set-bit #b00000000 3)
8  ; #b00001000

lfe> (combine-flags '(#b0001 #b0100 #b1000))
13  ; #b1101
```
