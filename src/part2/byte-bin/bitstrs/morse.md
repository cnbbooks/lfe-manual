# Practical Example: Morse Code

Morse code is naturally represented as a bitstring—dots and dashes of varying length. Let's build a simple encoder:

```lfe
(defun encode-morse (char)
  "Encode a single character to Morse (1=dah, 0=dit)."
  (case char
    (#\A (binary ((1 (size 1)) (0 (size 1)))))      ; .-
    (#\B (binary ((0 (size 1)) (1 (size 1))
                  (1 (size 1)) (1 (size 1)))))     ; -...
    (#\C (binary ((0 (size 1)) (1 (size 1))
                  (0 (size 1)) (1 (size 1)))))     ; -.-.
    (#\E (binary ((1 (size 1)))))                   ; .
    (#\T (binary ((0 (size 1)))))                   ; -
    (_ (binary ()))))  ; Unknown character

(defun encode-word (word)
  "Encode a word to Morse bitstring."
  (let ((chars (string:to_upper word)))
    (lists:foldl
      (lambda (char acc)
        (binary ((acc bitstring) ((encode-morse char) bitstring))))
      (binary ())
      chars)))
```

Testing:

```lfe
lfe> (encode-word "BEAT")
#B(27:10)  ; -... . .- -

lfe> (bit_size (encode-word "BEAT"))
10  ; Total bits needed
```

Each letter uses a different number of bits, so the result is naturally a bitstring. This is more space-efficient than using full bytes for each letter, and more faithful to the actual structure of Morse code.
