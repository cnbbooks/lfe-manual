# Flow Control

## `if` Branches

Large conditional expressions and deeply nested blocks of code are harder to read, so should be factored out into functions.

For example, this:

```lisp
(if (and (fuelled? rocket)
         (lists:all #'strapped-in?
                    (crew rocket))
         (sensors-working? rocket))
  (launch rocket)
  (! pid `#(err "Aborting launch.")))
```

Should be refactored to something like this:

```lisp
(defun rocket-ready? (rocket)
  (and (fuelled? rocket)
       (lists:all #'strapped-in?
                  (crew rocket))
       (sensors-working? rocket)))

(if (rocket-ready-p rocket)
  (launch rocket)
  (! pid `#(err "Aborting launch.")))
```

## `case` Branches

Don't write complex `case` statements with deeply nested branching. Instead, split these into functions, too, pattern-matching in the function heads.
