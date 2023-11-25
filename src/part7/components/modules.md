# Modules

When defining modules in LFE, put exported functions and their arities on separate lines. Optionally, functions of the same name with different arity may be put on the same line. Functions within a single `export` call should be sorted alphabetically.

Do not use `(export all)`; explicitly exporting functions constitutes the conventional contract for clients utilising the module.

Very  bad:

```lisp
(defmodule maths
  (export all))
```

Bad:

```lisp
(defmodule maths
  (export (factorial 2)
          (large-prime-number? 1)
          (small-prime-number? 1)
          (ackermann 2)
          (factorial 1)))
```

Better:

```lisp
(defmodule maths
  (export
   (ackermann 2)
   (factorial 1) (factorial 2)
   (large-prime-number? 1)
   (small-prime-number? 1)))
```

If you have a public API with groups of related functions in a module, you may indicate their relatedness with separate `export`s:

```lisp
(defmodule maths
  (export
   (util-func 1)
   (other-util 2))
  (export
   (ackermann 2)
   (factorial 1) (factorial 2)
   (large-prime-number? 1)
   (small-prime-number? 1)))
```

## With Pseudo-Packages

If you are using the LFE `rebar3` plugin, then you also have the flexibility of organising your project's source code into sub-directories under your source directory (see the [projects](./projects.html) section for more information).

In that case, you would define your module like so:

```lisp
(defmodule project.subdir.maths
  (export
   (ackermann 2)
   (factorial 1) (factorial 2)
   (small-prime-number? 1)
   (large-prime-number? 1)))
```

Since there is no support in Erlang and LFE for actual packages, the dotted name is actually a module. As such, when referencing this module elsewhere, use import aliases to improve readability):

```lisp
(defmodule client
  (export
   (some-func 0))
  (import
   (from project.subdir.maths
         (small-prime-number? 1))))

(defun some-func ()
  (small-prime-number? 100))
```

Or, if you need to avoid a name collision between the imported function and one in the client module:

```lisp
(defmodule client
  (export
   (some-func 0))
  (import
   (rename project.subdir.maths
           ((small-prime-number? 1) small-prime?))))

(defun some-func ()
  (small-prime? 100))
```


## When to Create

If some portion of your code is reusable enough to be a module then the maintenance gains are really worth the overhead of splitting it out with separate tests and docs.<a href="#footnote-1"><sup>1</sup></a>

Gains for separating code into separate modules include, but are not limited to:

* Easier reuse in other parts of the software being developed.
* Increased ability to reason about problems due to increased simplicity and separation of concerns.
* Great clarity and understanding of the system as a whole.

A good general workflow around module creation:

1. Start small and remain focused on the problem at hand.
1. Write just the functions you need.
1. Keep the functions small and limit them to one specific chunk of functionality (do one thing and do it well).
1. Make incremental changes as needed.

For new code:
1. Experiment with in the LFE REPL by defining your function and then calling with different values (expected and otherwise).
1. When it works in the REPL, create a test module in `./test` and paste the function calls in a test case.
1. Create a new module in `./src` and paste the final form of your function from the REPL.
4. Ensure the tests pass successfully for the new module.

[Build your libraries](libraries.html) using this approach

----

<ol>
<li><a name="footnote-1">
Parts of this page were adapted from a <a href="https://gist.github.com/substack/5075355">Github Gist</a> by <a href="https://github.com/substack">James Halliday</a>.
</li>
</ol>
