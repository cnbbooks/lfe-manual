# Procedural Macros

**Definition**:

```lisp
(define-macro name (arg1 arg2 ... $ENV)
  body)
```

**Special argument `$ENV`**: The macro receives the **environment** at the call site, enabling hygiene and context-aware transformation.

**Example**:

```lisp
(define-macro unless (test . body)
  `(if ,test 'false (progn ,@body)))

;; Usage:
(unless (< x 0)
  (print "positive")
  (inc x))

;; Expands to:
(if (< x 0)
    'false
    (progn
      (print "positive")
      (inc x)))
```

**Quasiquotation** (backquote):

```lisp
`(list ,x ,@xs)

;; With x=42, xs=[1,2,3]:
[list, 42, 1, 2, 3]
```

**Backquote operators**:

- `` ` `` (backquote): Quote with holes
- `,` (comma / unquote): Insert value
- `,@` (comma-at / unquote-splicing): Splice list

**Example of manual hygiene**:

```lisp
;; CAREFUL: This macro can capture variables
(define-macro my-swap (a b)
  `(let ((temp ,a))
     (set ,a ,b)
     (set ,b temp)))  ; 'temp' might collide with user code

;; Better: use a very unlikely variable name
(define-macro my-swap (a b)
  `(let ((___swap_temp_internal___ ,a))
     (set ,a ,b)
     (set ,b ___swap_temp_internal___)))
```
