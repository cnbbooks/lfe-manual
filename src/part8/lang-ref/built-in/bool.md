# Boolean and Logical

| Function | Syntax | Description |
|----------|--------|-------------|
| `not` | `(not expr)` | Logical NOT |
| `and` | `(and e1 e2 ...)` | Logical AND (all evaluated) |
| `or` | `(or e1 e2 ...)` | Logical OR (all evaluated) |
| `andalso` | `(andalso e1 e2 ...)` | Short-circuit AND |
| `orelse` | `(orelse e1 e2 ...)` | Short-circuit OR |
| `xor` | `(xor e1 e2)` | Logical XOR |

**Short-Circuit Evaluation**:

```lisp
;; andalso stops at first false
(andalso (< x 10) (expensive-check x))

;; orelse stops at first true
(orelse (is-cached x) (compute-expensive x))

;; Regular and/or evaluate all arguments
(and (< x 10) (expensive-check x))  ; Always calls expensive-check
```
