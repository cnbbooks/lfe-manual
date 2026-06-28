# Arithmetic and Comparison

| Function | Syntax | Description |
|----------|--------|-------------|
| `+` | `(+ a b ...)` | Addition |
| `-` | `(- a b ...)` | Subtraction |
| `*` | `(* a b ...)` | Multiplication |
| `/` | `(/ a b)` | Division (float result) |
| `div` | `(div a b)` | Integer division |
| `rem` | `(rem a b)` | Remainder |
| `abs` | `(abs n)` | Absolute value |
| `==` | `(== a b)` | Equal (type coercion) |
| `/=` | `(/= a b)` | Not equal (type coercion) |
| `=:=` | `(=:= a b)` | Exactly equal |
| `=/=` | `(=/= a b)` | Exactly not equal |
| `<` | `(< a b)` | Less than |
| `>` | `(> a b)` | Greater than |
| `=<` | `(=< a b)` | Less than or equal |
| `>=` | `(>= a b)` | Greater than or equal |

**Comparison Semantics**:

```lisp
;; Type coercion
(== 1 1.0)    ; → true
(=:= 1 1.0)   ; → false

;; Ordering across types
(< 1 'atom)   ; → true (numbers < atoms)
(< 'atom "string")  ; → true (atoms < references < funs < ports < pids < tuples < maps < lists < binaries)
```

**Arithmetic Examples**:

```lisp
(+ 1 2 3 4)           ; → 10
(- 10 5 2)            ; → 3
(* 2 3 4)             ; → 24
(/ 10 3)              ; → 3.3333...
(div 10 3)            ; → 3
(rem 10 3)            ; → 1

;; Mixed integer/float
(+ 1 2.5)             ; → 3.5
(* 2 3.0)             ; → 6.0
```
