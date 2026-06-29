# Extracting Bits from Bytes

A common operation is extracting individual bits from bytes:

```lfe
(defun byte-to-bits (byte)
  "Convert a byte to a list of 8 bits."
  (list-comp ((<<(bit (size 1))>> (binary-gen (<= (binary (byte))))))
             bit))

(defun bits-to-byte (bits)
  "Convert a list of 8 bits to a byte."
  (binary-comp ((<<bit>> (list-gen (<- bits))))
               bit))
```

Testing:

```lfe
lfe> (byte-to-bits 170)  ; 0b10101010
(1 0 1 0 1 0 1 0)

lfe> (bits-to-byte '(1 1 0 0 1 1 0 0))
#B(204)  ; 0b11001100
```

This is useful for manipulating bitmasks, implementing bit-banging protocols, or simply debugging binary data.
