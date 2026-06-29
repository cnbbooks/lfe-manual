# The Alignment Problem

While bitstrings are powerful, there's a practical constraint: most I/O operations expect byte-aligned data. You can't write a 7-bit bitstring directly to a file or socket. The data must be padded to a byte boundary first.

## Padding to Byte Alignment

```lfe
(defun pad-to-bytes (bitstring)
  "Pad bitstring to next byte boundary with zeros."
  (let* ((bit-size (bit_size bitstring))
         (padding-needed (rem (- 8 (rem bit-size 8)) 8)))
    (if (=:= padding-needed 0)
        bitstring
        (binary ((bitstring bitstring)
                 ((0 (size padding-needed))))))))
```

Testing:

```lfe
lfe> (set bits #B(127:7))
#B(127:7)
lfe> (pad-to-bytes bits)
#B(254)  ; Padded with one 0 bit: 1111111 → 11111110

lfe> (is_binary (pad-to-bytes bits))
true  ; Now it's a proper binary
```

The 7-bit value `1111111` becomes the 8-bit value `11111110` (decimal 254) when padded.
