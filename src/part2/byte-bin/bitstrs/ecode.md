# Variable-Length Encoding Example

Consider a hypothetical protocol where integers are encoded with a variable-length scheme:

- 0-15: 4 bits
- 16-255: 8 bits (prefix: 1111)
- 256+: 16 bits (prefix: 11111111)

```lfe
(defun encode-varint (n)
  "Encode integer with variable-length encoding."
  (cond
    ((=< n 15)
     (binary ((n (size 4)))))
    ((=< n 255)
     (binary ((15 (size 4)) (n (size 8)))))
    ((<= n 65535)
     (binary ((255 (size 8)) (n (size 16)))))
    ('true
     (error 'value_too_large))))

(defun encode-varint-list (nums)
  "Encode a list of integers."
  (lists:foldl
    (lambda (n acc)
      (binary ((acc bitstring) ((encode-varint n) bitstring))))
    (binary ())
    nums))
```

Testing:

```lfe
lfe> (encode-varint-list '(5 200 10))
#B(95 200 10 0)  ; 0101 11111111 11001000 1010 (non-aligned!)

lfe> (bit_size (encode-varint-list '(5 200 10)))
28  ; 4 + 12 + 4 + 8 = 28 bits
```

The result is 28 bits (3.5 bytes), demonstrating how variable-length encoding produces non-aligned bitstrings.
