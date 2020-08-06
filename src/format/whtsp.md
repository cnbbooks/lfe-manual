# Vertical White Space

You should include one blank line between top-level forms, such as function definitions. Exceptionally, blank lines can be omitted between simple, closely related defining forms of the same kind, such as a group of related type declarations or constant definitions.

```lisp
(defun my-pi () 3.14)
(defun my-e () 2.72)

(defun factorial (n)
  (factorial n 1))

(defun factorial
  ((0 acc) acc)
  ((n acc) (when (> n 0))
   (factorial (- n 1) (* n acc))))
```

# Horizontal White Space

Do not include extra horizontal whitespace before or after parentheses or around symbols.

Furthermore, do not place right parentheses by themselves on a line. A set of consecutive trailing parentheses must appear on the same line.

Very bad:

```text
( defun factorial
  (
    ( 0 acc)
    acc
  )
  (
    ( n acc)
      ( when ( > n 0)
  )
  ( factorial ( - n 1)
    ( * n acc
       )
     )
  )
)
```

Much better:
```lisp
(defun factorial
  ((0 acc) acc)
  ((n acc) (when (> n 0))
   (factorial (- n 1) (* n acc))))
```

You should use only one space between forms.

You should not use spaces to vertically align forms in the middle of consecutive lines. An exception is made when the code possesses an important yet otherwise not visible symmetry that you want to emphasize.

Bad:
```text
(let* ((low    1)
       (high   2)
       (sum    (+ (* low low) (* high high))))
  ...)
```

Better:
```lisp
(let* ((low 1)
       (high 2)
       (sum (+ (* low low) (* high high))))
  ...))
```

You should align nested forms if they occur across more than one line.

Bad:
```text
(defun munge (a b c)
(* (+ a b)
c))
```

Better:
```lisp
(defun munge (a b c)
  (* (+ a b)
     c))
```
