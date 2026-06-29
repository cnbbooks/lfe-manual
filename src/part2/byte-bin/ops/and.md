# Bitwise AND

The AND operation returns 1 only when both bits are 1. It's the pessimist of Boolean operations, assuming the worst and only accepting perfection.

```lfe
lfe> (band #b1010 #b1100)
8  ; #b1000 in binary

lfe> (band 170 85)
0  ; 10101010 AND 01010101 = 00000000
```

## Practical Use: Masking

AND is commonly used to mask out specific bits:

```lfe
(defun get-low-nibble (byte)
  "Extract the lower 4 bits of a byte."
  (band byte #b00001111))

(defun is-even? (n)
  "Check if number is even by testing lowest bit."
  (=:= 0 (band n 1)))
```

Testing:

```lfe
lfe> (get-low-nibble #xA7)
7  ; Keep only 0111

lfe> (is-even? 42)
true
lfe> (is-even? 43)
false
```

The even/odd test works because even numbers have a 0 in the least significant bit, while odd numbers have a 1. Masking with `(band n 1)` extracts just that bit.
