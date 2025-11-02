# Binding Macros

| Macro | Description |
|-------|-------------|
| `let*` | Sequential let (each binding sees previous) |
| `flet` | Local function definitions (like let-function) |
| `fletrec` | Recursive local functions (like letrec-function) |
| `flet*` | Sequential function definitions |

**Examples**:

```lisp
;; let* - each binding sees previous
(let* ((x 1)
       (y (+ x 1))
       (z (+ y 1)))
  (list x y z))  ; → (1 2 3)

;; flet - local functions
(flet ((double (x) (* x 2))
       (triple (x) (* x 3)))
  (+ (double 5) (triple 3)))  ; → 19

;; fletrec - mutually recursive
(fletrec ((even? (n)
            (if (== n 0) 'true (odd? (- n 1))))
          (odd? (n)
            (if (== n 0) 'false (even? (- n 1)))))
  (even? 42))  ; → true
```
