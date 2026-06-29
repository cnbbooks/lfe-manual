# Bit-Level Pattern Matching

Remember that LFE works at the *bit* level, not just byte level. You can match on arbitrary bit boundaries:

```lfe
(defun decode-flags (byte)
  "Decode individual flag bits."
  (binary ((verbose (size 1))
           (debug (size 1))
           (production (size 1))
           (experimental (size 1))
           (_ (size 4)))           ; Skip reserved bits
          (binary (byte)))
  `#m(verbose ,(=/= verbose 0)
      debug ,(=/= debug 0)
      production ,(=/= production 0)
      experimental ,(=/= experimental 0)))
```

Testing with a flags byte where bits 0, 1, and 3 are set:

```lfe
lfe> (decode-flags #B(11010000))
#m(verbose true debug true production false experimental true)
```
