# Documentation

**Module Documentation**:

```lisp
(defmodule mymod
  (doc "This module does amazing things."))
```

**Function Documentation**:

```lisp
(defun add (x y)
  "Add two numbers together.

  Returns the sum of x and y."
  (+ x y))
```

**Access Documentation**:

```lisp
;; In shell
(doc 'mymod)              ; Module docs
(doc 'mymod 'add)         ; Function docs
(doc 'mymod 'add 2)       ; Specific arity
```
