# The Unit Specification

Now we arrive at the `unit` specification, which is where things become delightfully mathematical. The unit specifies a *multiplier* for the size, allowing you to work in convenient chunks:

```lfe
Value:Size/TypeSpec-unit:N
```

The total number of bits used is: **Size × Unit**

## Default Units

- **integer**, **float**, **bitstring**: unit of 1 (counting individual bits)
- **binary**: unit of 8 (counting bytes, as civilized beings do)

## Why Units Matter

Consider the case of reading 4-byte chunks of data:

```lfe
lfe> (defun read-4byte-chunks (bin)
       (binary-comp
         ((<< (chunk (size 4) binary) >> (binary-gen (<= bin))))
         chunk))

lfe> (read-4byte-chunks #B(1 2 3 4 5 6 7 8 9 10 11 12))
(#B(1 2 3 4) #B(5 6 7 8) #B(9 10 11 12))
```

Or, more explicitly using unit:

```lfe
lfe> (set msg #B(72 101 108 108 111))  ; "Hello"
lfe> (binary ((msg (size 5) (unit 8))))  ; 5 units of 8 bits = 40 bits total
#B(72 101 108 108 111)
```
