# C-style Accessors

LFE provides the traditional Lisp `c*r` family of list accessors:

```lisp
(caar x)   ; (car (car x))
(cadr x)   ; (car (cdr x))
(cdar x)   ; (cdr (car x))
(cddr x)   ; (cdr (cdr x))
(caaar x)  ; (car (car (car x)))
...
; Up to 4 levels: caaaaar, etc.
```
