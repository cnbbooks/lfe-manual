# Bit Shift Right

Shifting right by N positions is equivalent to integer division by 2^N, rounding down.

```lfe
lfe> (bsr 8 1)
4  ; 8 / 2 = 4

lfe> (bsr 10 1)
5  ; 10 / 2 = 5 (rounds down)

lfe> (bsr 255 4)
15  ; 255 / 16 = 15

lfe> (bsr #b1100 2)
3  ; #b0011
```
