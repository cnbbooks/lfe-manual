# Types and Specs

**Type Definitions**:

```lisp
;; Simple type alias
(deftype byte () (integer 0 255))

;; Parameterized type
(deftype list (a) (list a))

;; Union type
(deftype number () (union integer float))

;; Record type
(deftype person ()
  (tuple 'person
         string    ; name
         integer)) ; age
```

**Function Specs**:

```lisp
;; Simple spec
(defun add
  {(spec [[integer integer] integer])}
  ([x y] (+ x y)))

;; Multiple clauses
(defun process
  {(spec [[atom] ok]
         [[atom any] {ok any}])}
  ([cmd] ...)
  ([cmd arg] ...))

;; With type variables
(defun id
  {(spec [[a] a])}
  ([x] x))
```
