# Control Flow Macros

| Macro | Description | Example |
|-------|-------------|---------|
| `cond` | Multi-branch conditional | `(cond (test1 result1) ...)` |
| `when` | Conditional execution | `(when test body)` |
| `unless` | Inverted conditional | `(unless test body)` |
| `do` | Iteration macro | `(do ((var init update)) test body)` |

**Cond**:

```lisp
(cond
  ((< x 0) 'negative)
  ((> x 0) 'positive)
  ('true 'zero))

;; With multiple forms per branch
(cond
  ((is_atom x)
   (io:format "Atom: ~p~n" (list x))
   (process-atom x))
  ((is_list x)
   (io:format "List: ~p~n" (list x))
   (process-list x)))
```

**When/Unless**:

```lisp
(when (> x 10)
  (io:format "Big number: ~p~n" (list x))
  (process-big x))

(unless (is-empty list)
  (process list))
```

**Do Loop**:

```lisp
;; Count from 1 to 10
(do ((i 1 (+ i 1)))
    ((> i 10) 'done)
  (io:format "~p~n" (list i)))

;; Multiple variables
(do ((i 0 (+ i 1))
     (sum 0 (+ sum i)))
    ((> i 10) sum)
  (io:format "i=~p sum=~p~n" (list i sum)))
```
