# Data Construction

| Form | Syntax | Description | Example |
|------|--------|-------------|---------|
| `quote` | `(quote expr)` or `'expr` | Return expr unevaluated | `'(a b c)` → `(a b c)` |
| `cons` | `(cons head tail)` | Construct cons cell | `(cons 1 '(2 3))` → `(1 2 3)` |
| `car` | `(car list)` | Get first element | `(car '(a b c))` → `a` |
| `cdr` | `(cdr list)` | Get tail | `(cdr '(a b c))` → `(b c)` |
| `list` | `(list e1 e2 ...)` | Construct list | `(list 1 2 3)` → `(1 2 3)` |
| `tuple` | `(tuple e1 e2 ...)` | Construct tuple | `(tuple 'ok 42)` → `{ok, 42}` |
| `binary` | `(binary seg1 seg2 ...)` | Construct binary | `(binary (42 (size 8)))` → `<<42>>` |
| `map` | `(map k1 v1 k2 v2 ...)` | Construct map | `(map 'a 1 'b 2)` → `#{a => 1, b => 2}` |

**Notes**:

- `cons`, `car`, `cdr` are Lisp traditional names
- Also available as `hd`, `tl` (Erlang style)
- Lists are linked lists (Erlang lists)
- Tuples are fixed-size arrays (Erlang tuples)
- Maps are hash maps (Erlang maps, OTP 17+)
