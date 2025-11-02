# Tuple Operations

| Function | Syntax | Description |
|----------|--------|-------------|
| `tuple` | `(tuple e1 ...)` | Create tuple |
| `tuple_size` | `(tuple_size t)` | Tuple size |
| `element` | `(element n t)` | Get element (1-indexed) |
| `setelement` | `(setelement n t v)` | Set element (returns new tuple) |

**Examples**:

```lisp
(let ((t (tuple 'ok 42 "hello")))
  (tuple_size t))              ; → 3

(element 2 (tuple 'ok 42))     ; → 42

(setelement 2 (tuple 'ok 0) 42)  ; → {ok, 42}
```
