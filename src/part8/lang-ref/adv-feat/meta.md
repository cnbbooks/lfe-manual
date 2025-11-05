# Metaprogramming

**Quasiquotation** (backquote):

```lisp
;; Backquote
`(a b c)  ; → (a b c)

;; Unquote
(let ((x 42))
  `(a ,x c))  ; → (a 42 c)

;; Unquote-splicing
(let ((l '(b c d)))
  `(a ,@l e))  ; → (a b c d e)

;; Nested quasiquote
(let ((x 10))
  `(let ((y ,x))
     (+ y ,(* x 2))))
; → (let ((y 10)) (+ y 20))
```

**Macro Definition**:

```lisp
;; Simple macro
(defmacro when-positive (x body)
  `(if (> ,x 0) ,body 'undefined))

;; Pattern-based macro
(defmacro my-cond args
  `(cond ,@args))

;; Recursive macro
(defmacro my-let
  ([() body] body)
  ([((cons (list var val) rest)) body]
   `(let ((,var ,val))
      (my-let ,rest ,body))))
```
