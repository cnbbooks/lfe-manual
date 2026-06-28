# Control Flow

| Form | Syntax | Description |
|------|--------|-------------|
| `progn` | `(progn expr ...)` | Sequence expressions, return last |
| `if` | `(if test then else)` | Conditional (3-argument only) |
| `case` | `(case expr (pattern body) ...)` | Pattern matching |
| `receive` | `(receive (pattern body) ... (after timeout body))` | Message receiving |
| `try` | `(try expr (case ...) (catch ...) (after ...))` | Exception handling |
| `catch` | `(catch expr)` | Catch exceptions |

**If Form**:

```lisp
;; Must have exactly 3 arguments
(if (> x 10)
    'big
    'small)

;; For multiple branches, use cond macro
(cond
  ((< x 0) 'negative)
  ((> x 0) 'positive)
  ('true 'zero))
```

**Case Form**:

```lisp
;; Pattern matching
(case (tuple 'ok 42)
  ((tuple 'ok n) n)              ; Match and bind
  ((tuple 'error _) 0)           ; Match, ignore
  (_ 'unknown))                  ; Catch-all

;; With guards
(case x
  (n (when (> n 10)) 'big)
  (n (when (> n 0)) 'small)
  (_ 'other))

;; Nested patterns
(case (list 1 (tuple 'ok 42) 3)
  ((list _ (tuple 'ok value) _)
   value))  ; → 42
```

**Receive Form**:

```lisp
;; Simple receive
(receive
  ((tuple 'msg data)
   (io:format "Got: ~p~n" (list data))))

;; With timeout
(receive
  ((tuple 'stop) 'stopped)
  ((tuple 'msg data) data)
  (after 5000
    'timeout))

;; Pattern matching in receive
(receive
  ((tuple from (tuple 'request id data))
   (! from (tuple 'response id (process data))))
  (after 1000
    'timeout))
```

**Try Form**:

```lisp
;; Full try form
(try
  (risky-operation)
  (case result
    ((tuple 'ok value) value)
    ((tuple 'error _) 'failed))
  (catch
    ((tuple 'error reason stack)
     (log-error reason stack)
     'error)
    ((tuple 'throw value stack)
     (handle-throw value)
     'thrown))
  (after
    (cleanup-resources)))

;; Simplified catch
(catch (risky-operation))  ; Returns {'EXIT', Reason} on error
```
