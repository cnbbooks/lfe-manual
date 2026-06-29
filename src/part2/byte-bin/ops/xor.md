# Bitwise XOR

XOR (exclusive OR) returns 1 when bits *differ*. It's the contrarian, celebrating difference and rejecting conformity.

```lfe
lfe> (bxor #b1010 #b1100)
6  ; #b0110 in binary

lfe> (bxor 170 85)
255  ; 10101010 XOR 01010101 = 11111111
```

## Practical Use: Toggling and Encryption

XOR has unique properties that make it useful for:

- **Toggling bits**: `(bxor x mask)` flips bits where mask is 1
- **Simple encryption**: `(bxor data key)` encrypts, `(bxor encrypted key)` decrypts
- **Comparison**: XOR finds differences between bit patterns

```lfe
(defun toggle-bit (byte bit-position)
  "Toggle the bit at bit-position."
  (bxor byte (bsl 1 bit-position)))

(defun simple-cipher (data key)
  "XOR cipher (symmetric: same function encrypts and decrypts)."
  (bxor data key))

(defun find-differences (a b)
  "Return bitmask showing where two values differ."
  (bxor a b))
```

Testing:

```lfe
lfe> (let ((original #b10101010))
       (toggle-bit (toggle-bit original 0) 0))
170  ; Toggle twice returns to original

lfe> (let* ((plaintext 42)
            (key 128)
            (encrypted (simple-cipher plaintext key))
            (decrypted (simple-cipher encrypted key)))
       `#(encrypted ,encrypted decrypted ,decrypted))
#(encrypted 170 decrypted 42)

lfe> (find-differences #b11110000 #b11001100)
60  ; #b00111100 - bits differ at positions 2,3,4,5
```

Note: The XOR "cipher" is only secure for one-time pads with truly random keys. Using it with short or predictable keys is approximately as secure as writing your password on a post-it note—which is to say, not at all.
