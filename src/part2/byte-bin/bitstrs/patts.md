# Pattern Matching Bitstrings

Pattern matching works identically with bitstrings as with binaries, you simply specify non-byte-aligned patterns:

```lfe
(defun decode-flags (bitstring)
  "Decode a 7-bit flag field."
  (binary ((flag1 (size 1))
           (flag2 (size 1))
           (flag3 (size 1))
           (counter (size 4)))
          bitstring)
  `#m(flag1 ,(=/= flag1 0)
      flag2 ,(=/= flag2 0)
      flag3 ,(=/= flag3 0)
      counter ,counter))
```

Testing:

```lfe
lfe> (decode-flags #B(#b1010111:7))
#m(flag1 true flag2 false flag3 true counter 7)
```

The bit pattern `1010111` is decoded as: flag1=1, flag2=0, flag3=1, counter=0111 (decimal 7).
