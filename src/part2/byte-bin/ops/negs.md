# A Note on Negative Numbers

Due to two's complement representation, bitwise operations on negative numbers can produce surprising results:

```lfe
lfe> (band -1 255)
255  ; -1 is all bits set, so AND with 255 gives 255

lfe> (bsr -16 2)
-4  ; Arithmetic shift preserves sign
```

In general, bitwise operations are most intuitive when working with unsigned interpretations of integers. If you need to work with negative values, be aware that the sign bit will participate in the operation.
