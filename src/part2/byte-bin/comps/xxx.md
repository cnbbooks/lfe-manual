# Bit-Level Manipulation

Remember that we're working with *bits*, not just bytes. Let's extract alternating 4-bit nibbles:

```lfe
lfe> (let ((data #B(#xAB #xCD #xEF)))
       (list-comp ((<<(nibble (size 4))>> (binary-gen (<= data))))
                  nibble))
(10 11 12 13 14 15)  ; 0xA, 0xB, 0xC, 0xD, 0xE, 0xF
```

Each 4-bit nibble is extracted in sequence. This is particularly useful when dealing with BCD (Binary-Coded Decimal) or similar packed formats.
