# Definition Macros

| Macro | Syntax | Expands To |
|-------|--------|------------|
| `defmodule` | `(defmodule name ...)` | `(define-module name ...)` |
| `defun` | `(defun name args body)` | `(define-function name ...)` |
| `defmacro` | `(defmacro name args body)` | `(define-macro name ...)` |
| `defrecord` | `(defrecord name fields)` | `(define-record name ...)` |
| `defstruct` | `(defstruct fields)` | `(define-struct ...)` |

**Module Definition**:

```lisp
(defmodule mymod
  (export (add 2) (sub 2))
  (export-macro when-positive)
  (import (from lists map filter)))

;; Function definition
(defun add (x y)
  "Add two numbers."
  (+ x y))

;; Multi-clause function
(defun factorial
  ([0] 1)
  ([n] (when (> n 0))
   (* n (factorial (- n 1)))))

;; Macro definition
(defmacro when-positive (x body)
  `(if (> ,x 0) ,body 'undefined))
```
