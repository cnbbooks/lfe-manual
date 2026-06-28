# Evaluation: : `slurp` and `unslurp`

LFE provides a unique and powerful way to evaluate file contents directly in the REPL through the `slurp` and `unslurp` commands. This feature allows you to temporarily import function and macro definitions from files into your current REPL session.

## Using `slurp`

The `slurp` function reads an LFE source file and makes all functions and macros defined in that file available directly in the shell, without requiring module prefixes:

```lisp
lfe> (slurp "examples/my-functions.lfe")
#(ok -no-mod-)
lfe> $ENV
;; Shows all the new function and macro definitions from the file
```

Key characteristics of `slurp`:

* Only one file can be slurped at a time
* Slurping a new file automatically removes all data from the previously slurped file
* Functions and macros become available without module prefixes
* The code is evaluated in the current REPL environment
* Variable bindings from the file are added to your current session

## Using `unslurp`

The `unslurp` command reverts the REPL back to the state before the last slurp, removing all function and macro definitions that were imported:

```lisp
lfe> (unslurp)
ok
lfe> $ENV
;; Back to the original environment state
```

This is particularly useful when experimenting with different versions of functions or when you want to clean up your REPL environment.

## Practical Example

Let's say you have a file called `math-utils.lfe`:

```lisp
(defun double (x)
  (* x 2))

(defun square (x)
  (* x x))

(defmacro when-positive (x body)
  `(if (> ,x 0) ,body 'not-positive))
```

After slurping this file:

```lisp
lfe> (slurp "math-utils.lfe")
#(ok -no-mod-)
lfe> (double 21)
42
lfe> (square 6)
36
lfe> (when-positive 5 'yes)
yes
lfe> (when-positive -1 'yes)
not-positive
```
