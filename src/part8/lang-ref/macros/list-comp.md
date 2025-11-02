# List Comprehensions

| Macro | Description |
|-------|-------------|
| `lc` | List comprehension |
| `bc` | Binary comprehension |
| `qlc` | Query list comprehension (for databases) |

**List Comprehension**:

```lisp
;; Basic
(lc ((<- x '(1 2 3 4 5)))
  (* x 2))
; → (2 4 6 8 10)

;; With filter
(lc ((<- x '(1 2 3 4 5 6))
     (> x 3))
  x)
; → (4 5 6)

;; Multiple generators
(lc ((<- x '(1 2 3))
     (<- y '(a b)))
  (tuple x y))
; → ((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))

;; Pattern matching in generator
(lc ((<- (tuple 'ok value) (list (tuple 'ok 1) (tuple 'error 2) (tuple 'ok 3))))
  value)
; → (1 3)
```

**Binary Comprehension**:

```lisp
;; Create binary from list
(bc ((<- x '(65 66 67)))
  (x (size 8)))
; → <<"ABC">>

;; Transform binary
(let ((bin <<"hello">>))
  (bc ((<= (c (size 8)) bin))
    ((- c 32) (size 8))))
; → <<"HELLO">>
```
