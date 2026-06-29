# The Pattern Matching Constraint

In pattern matching, there's an additional rule that makes perfect sense once you think about it: you can only use *bound variables* as size specifiers, or literal integers. The pattern matcher needs to know how many bits to extract, and it can't very well guess.

```lfe
; This works - N is bound before use:
(defun extract-n-bytes (n data)
  (binary ((header (size n) binary) payload binary) data)
  (tuple 'extracted header payload))

; This won't work - N is unbound:
(defun mysterious-extraction (data)
  (binary ((header (size n) binary) payload binary) data)  ; Error!
  (tuple 'extracted header payload))
```
