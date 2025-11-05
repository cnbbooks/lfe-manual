# Function Types

| Type | Description |
|------|-------------|
| **Function** | First-class function value |
| **Closure** | Function with captured environment |
| **Module Function** | Reference to module function |

**Function Values**:

```lisp
;; Lambda creates function value
(let ((f (lambda (x) (* x 2))))
  (funcall f 5))  ; → 10

;; Function reference
(let ((f (function lists reverse 1)))
  (funcall f '(1 2 3)))  ; → (3 2 1)

;; Closure (captures x)
(let ((x 10))
  (lambda (y) (+ x y)))
```
