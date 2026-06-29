# Bit Shift Left

Shifting left by N positions is equivalent to multiplying by 2^N. This is one of the fastest arithmetic operations available.

```lfe
lfe> (bsl 1 0)
1  ; 1 * 2^0 = 1

lfe> (bsl 1 3)
8  ; 1 * 2^3 = 8

lfe> (bsl 5 4)
80  ; 5 * 16 = 80

lfe> (bsl #b0011 2)
12  ; #b1100
```

## Practical Use: Scaling and Positioning

```lfe
(defun create-bitmask (n)
  "Create a bitmask with n bits set."
  (- (bsl 1 n) 1))

(defun pack-rgb (r g b)
  "Pack 8-bit RGB values into 24-bit integer."
  (bor (bsl r 16) (bor (bsl g 8) b)))
```

Testing:

```lfe
lfe> (create-bitmask 4)
15  ; #b1111

lfe> (pack-rgb 255 128 64)
16744512  ; #xFF8040 in hex
```
