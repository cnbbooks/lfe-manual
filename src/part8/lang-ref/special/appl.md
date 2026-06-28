# Function Application

| Form | Syntax | Description |
|------|--------|-------------|
| `call` | `(call module function arg ...)` | Remote call |
| `funcall` | `(funcall func arg ...)` | Apply function |
| `:` | `(: module function arg ...)` | Remote call (syntax sugar) |
| `function` | `(function name arity)` | Function reference |
| `function` | `(function module name arity)` | Remote function reference |

**Call Forms**:

```lisp
;; Direct call
(lists:map (lambda (x) (* x 2)) '(1 2 3))

;; call form (dynamic module/function)
(call 'lists 'map (lambda (x) (* x 2)) '(1 2 3))

;; : syntax (compile-time binding)
(: lists map (lambda (x) (* x 2)) '(1 2 3))

;; funcall (higher-order)
(funcall (lambda (x) (* x 2)) 5)  ; → 10

;; Function references
(function square 1)           ; Local function
#'square/1                     ; Syntax sugar
(function lists reverse 1)     ; Remote function
#'lists:reverse/1              ; Syntax sugar
```

**Dynamic Dispatch**:

```lisp
;; Module/function computed at runtime
(let ((mod 'lists)
      (func 'reverse))
  (call mod func '(1 2 3)))  ; → (3 2 1)

;; Function value
(let ((f (function lists reverse 1)))
  (funcall f '(1 2 3)))  ; → (3 2 1)
```
