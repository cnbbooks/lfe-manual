# Basic Pattern Matching Examples

## Fixed-Size Fields

The simplest case involves extracting fixed-size segments:

```lfe
(defun parse-rgb (pixel)
  "Extract RGB components from a 24-bit pixel."
  (binary ((r (size 8)) (g (size 8)) (b (size 8))) pixel)
  `#m(red ,r green ,g blue ,b))
```

Testing this function:

```lfe
lfe> (parse-rgb #B(255 0 128))
#m(red 255 green 0 blue 128)
```

## Variable-Length Fields

Things become more interesting when the size of one field determines the size of another—a common pattern in length-prefixed data structures:

```lfe
(defun parse-length-value (data)
  "Parse a length-prefixed value (Pascal string style)."
  (binary ((len (size 8)) (value (size len) binary) rest binary) data)
  `#(,value ,rest))
```

Note the delightful sequence of events: first we extract `len` (8 bits), then immediately use that value to determine how many bits to extract into `value`. This is rather like opening a package that contains instructions for how big the package is—recursive, slightly mind-bending, but ultimately quite useful.

```lfe
lfe> (set msg (binary ((5 (size 8)) (#"Hello" binary) (#" World" binary))))
#B(5 72 101 108 108 111 32 87 111 114 108 100)

lfe> (parse-length-value msg)
#(#B(72 101 108 108 111) #B(32 87 111 114 108 100))
```
