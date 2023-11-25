# Docstrings

First and foremost, document everything.

You should use document strings (a.k.a. "docstrings") on all visible functions to explain how to use your code.

Unless some bit of code is painfully self-explanatory, document it with a documentation string.

Documentation strings are destined to be read by the programmers who use your code. They can be extracted from functions, types, classes, variables and macros, and displayed by programming tools, such as IDEs, or by REPL queries; web-based documentation or other reference works can be created based on them. Documentation strings are thus the perfect locus to document your API. They should describe how to use the code (including what pitfalls to avoid), as opposed to how the code works (and where more work is needed), which is what you'll put in comments.

Supply a documentation string when defining top-level functions, records, classes, variables and macros. Generally, add a documentation string wherever the language allows.

For functions, the docstring should describe the function's contract: what the function does, what the arguments mean, what values are returned, what conditions the function can signal. It should be expressed at the appropriate level of abstraction, explaining the intended meaning rather than, say, just the syntax.

Some LFE forms do not accept docstrings, in which case a preceding code comment should be used instead.

```lisp
(defun small-prime-number? (n)
  "Return true if N, an integer, is a prime number. Otherwise, return false."
  ((n) (when (< n 4))
   (>= n 2))
  ((n) (when (== 0 (rem n 2)))
   'false)
  ((n)
   (lists:all #'not/1
              (lists:map (lambda (x) (== 0 (rem n x)))
                         (lists:seq 3 (trunc (math:sqrt n)))))))
```

```lisp
(defmacro is (bool-expression)
  "Assert bool-expression evaluates to 'true."
  `(assert ,bool-expression))
  ```

```lisp
;;; This record tracks test results and is ulimately used when reporting the
;;; status of completed tests.
(defrecord state
  (status (orddict:new))
  test-type
  (ok 0)
  (fail 0)
  (err 0)
  (skip 0)
  (cancel 0)
  (time 0))
```

A long docstring may usefully begin with a short, single-sentence summary, followed by the larger body of the docstring.

Text in successive lines of docstrings are indented two spaces, aligned with the open quote in the first line of documentation, not with the first character of the text.
