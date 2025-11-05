# Guards in Patterns

Guards provide additional constraints on patterns:

```lisp
;; Simple guard
(case x
  (n (when (> n 10)) 'big)
  (n (when (> n 0)) 'small)
  (_ 'other))

;; Multiple conditions
(case x
  (n (when (> n 0) (< n 100))
   'in-range))

;; Guard functions
(case x
  (x (when (is_atom x)) 'atom)
  (x (when (is_list x)) 'list)
  (_ 'other))

;; Complex guards
(case (tuple x y)
  ((tuple a b) (when (> a 0) (> b 0) (== (+ a b) 10))
   'valid))
```

**Allowed in Guards**:

- Type tests: `is_atom/1`, `is_list/1`, etc.
- Comparisons: `==`, `<`, `>`, etc.
- Arithmetic: `+`, `-`, `*`, `div`, `rem`, etc.
- Boolean: `and`, `or`, `not`, `andalso`, `orelse`
- BIFs: `abs/1`, `element/2`, `tuple_size/1`, etc.

**Not Allowed in Guards**:

- User-defined functions
- Send/receive operations
- Most BIFs (only guard-safe ones allowed)
