# Checking the Input

In LFE there are several ways in which you can perform checks on values:

* the `if` form
* the `cond` form
* the `case` form
* pattern-matching and/or guards in function heads

The last one is commonly used in LFE when passing messages / data between functions. Our initial, generated project code is already doing this, and given the game state data we will be working with, this feels like a good fit what we need to implement.

Normally records are used for application data, but since we just care about the value of two integers (the number selected for the answer and the number guessed by the player), we'll keep things simple in this game:

```lisp
(set answer 42)
```

Let's create a function with a guard:

```lisp
lfe> (defun check
lfe>   ((guess) (when (< guess answer))
lfe>    (io:format "Guess is too low~n")))
```

The extra parenthesis around the function's arguments is due to the use of the pattern-matching form of function definition we're using here. We need this form, since we're going to use a guard. The `when` after the function args is called a "guard" in LFE. As you might imagine, we could use any number of these.

```lisp
lfe> (check 10)
;; Guess is too low
;; ok
```

Let's add some more guards for the other checks we want to perform:

```lisp
lfe> (defun check
lfe>   ((guess) (when (< guess answer))
lfe>    (io:format "Guess is too low~n"))
lfe>   ((guess) (when (> guess answer))
lfe>    (io:format "Guess is too high~n"))
lfe>   ((guess) (when (== guess answer))
lfe>    (io:format "Correct!~n")))
```

```lisp
lfe> (check 10)
;; Guess is too low
;; ok
```
```lisp
lfe> (check 100)
;; Guess is too high
;; ok
```
```lisp
lfe> (check 42)
;; Correct!
;; ok
```

This should give a very general sense of what is possible.
