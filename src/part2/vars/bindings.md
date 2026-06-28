# Bindings

## In the REPL

To set a variable in the LFE REPL, use the `set` macro:

``` lisp
lfe> (set answer 42)
42
```

In the language itself, LFE doesn't support global variables -- a valuable
feature inherited from Erlang. However, in order for a REPL experience to be
useful, an environment must be maintained in which the user may write
expressions and then refer to them later. This environment is essentially a
mechanism for global state in the context of a single user running a single
Erlang VM. If we set a variable called `answer`, that variable will be available
to us as long as the REPL process continues or until we reset the environment.

Setting another value with the same variable name is allowed: it merely replaces
the assignment in the current REPL environment:

``` lisp
lfe> (set answer "forty-two")
"forty-two"
```

With a variable assigned with `set` it may be used at any time in the REPL
environment where it was defined:

``` lisp
lfe> (++ "The answer is " answer)
"The answer is forty-two"
```

Attempting to use a variable that has not been defined results in an error:

``` lisp
lfe> (++ "The question was " question)
** exception error: symbol question is unbound
  in lfe_eval:eval_error/1 (src/lfe_eval.erl, line 1292)
  in lists:map/2 (lists.erl, line 1243)
  in lists:map/2 (lists.erl, line 1243)
```

If you don't have any need to update the environment with data that you only
need for a specific calculation, you may use the `let` form:

``` lisp
lfe> (let ((short-lived-value (* 2 (+ 1 2 3 4 5 6))))
lfe>   (io:format "The answer is ~p~n" `(,short-lived-value)))
The answer is 42
ok
```

Let's make sure that variable wasn't saved to our environment:

``` lisp
lfe> short-lived-value
** exception error: symbol short-lived-value is unbound
```

The lexical scope for the `short-lived-value` is within the `let` only and is
not available outside of that scope.

## In Functions and Macros

Within functions, variables are lexically scoped and bound with `let` and
`let*`. One may also define lexically scoped functions _inside_ other functions,
and this is done with `flet` and `fletrec` (the latter required for defining
recursive functions inside another function). These will be covered in detail
[later in the book](../../part3/funs/).

We've seen `let` used above in the REPL; the same applies inside functions:

``` lisp
(defun display-answer ()
  (let ((answer (* 2 (+ 1 2 3 4 5 6))))
    (io:format "The answer is ~p~n" `(,answer))))
```

This is a straight-forward case of assignment; but what if we needed to assign
a varaible that depended upon _another_ variable. Using `let`, you'd have to do
this:

``` lisp
(defun display-answer ()
  (let ((data '(1 2 3 4 5 6)))
    (let ((answer (* 2 (lists:sum data))))
      (io:format "The answer is ~p~n" `(,answer)))))
```

However, as with other Lisps, LFE provides a convenience macro for this: `let*`.
Here's how it's used:

``` lisp
(defun display-answer ()
  (let* ((data '(1 2 3 4 5 6))
         (answer (* 2 (lists:sum data))))
    (io:format "The answer is ~p~n" `(,answer))))
```

Lexical scoping helps one isolate unrelated data or calculations, even in the
same function: multiple `let` or `let*` blocks may be declared in a function
and none of the bound variables in one block will be available to another block.
Attempting to do so will result in an error.

## In Modules

In LFE, one cannot bind variables at the module-level, only functions and
macros. This is part of the "no global variables" philosophy (and practice!) of
Erlang and LFE. Module-level bindings are done with `defun` for functions and
`defmacro` for macros. The creation of [modules](../../part3/modules),
[functions](../../part3/funs), and [macros](../../part4/macros) will all be
covered in detail later in the book.

## Shadowing

One shadows a variable in one scope when, at a higher scope, that variable was
also defined. Here's an annotated example:

``` lisp
(defun shadow-demo ()
  (let ((a 5))
    (io:format "~p~n" `(,a))    ; prints 5
    (let ((a 'foo))             ; 'a' here shadows 'a' in the previous scope
      (io:format "~p~n" `(,a))) ; prints foo
    (io:format "~p~n" `(,a)))   ; prints 5; the shadow binding is out of scope
  (let ((a 42))
    (io:format "~p~n" `(,a))))  ; prints 42 - new scope, no shadowing
```

Shadowing also may occur at the module-level with the definition of functions,
and the shadowing could be of functions at one of several levels. Here's a
run-down on function shadowing in modules, from the highest or "outermost" to
the lowest or "innermost":

* Predefined Erlang built-in functions (BIFs) may be shadowed by any of the
  following
* Predefined LFE BIFs may be shadowed by any of the following
* Module imports may shadow any of the above via aliasing
* Functions defined in a module may shadow any of the above
* Functions defined inside a function (e.g., via `flet` or `fletrec`) may shadow
  any of the above

Note that to shadow functions in LFE, functions must match both in name as well
as arity (number of arguments).

The `hd` Erlang BIF returns the "head" of a list (the first item). Here's an
example of shadowing it in the REPL. Here's the BIF at work:

``` lisp
lfe> (hd '(a b c d e))
a
```

Next, paste this into the REPL:

``` lisp
(defun hd (_)
  ;; part of the pun here is that the same function in Lisp is called 'car'
  "My other car is The Heart of Gold.")
```

The `hd` function takes one argument (a list), so our function also needs to
take one. However, since we don't do anything with that, we use the "don't care"
variable `_`.

Now let's call `hd` again:

``` lisp
lfe> (hd '(a b c d e))
"My other car is The Heart of Gold."
```

Shadowed!

Note that, like many other Lisps, LFE has the `car` function, but since this is
a core form, it can't be shadowed (see the next section).

## The Unshadowable

<div class="alert alert-info">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    Information
  </h4>
  <p class="mb-0">
    Core LFE forms can never be shadowed.
  </p>
</div>

Shadowing does not apply to the supported LFE
[core forms](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_guide.7.md#core-forms).
It may appear that
your code is shadowing those forms, but the compiler will _always_ use
the core meaning and never an alternative. It does this silently, without
warning -- so take care and do not be surprised!
