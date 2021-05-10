# Code Comments

Comments are explanations to the future maintainers of the code. Even if you're the only person who will ever see and touch the code, even if you're either immortal and never going to quit, or unconcerned with what happens after you leave (and have your code self-destruct in such an eventuality), you may find it useful to comment your code. Indeed, by the time you revisit your code, weeks, months or years later, you will find yourself a different person from the one who wrote it, and you will be grateful to the previous you for making the code readable.

Comment anything complicated so that the next developer can understand what's going on.

Also use comments as a way to guide those who read the code, so they know what to find where.

Code comments in LFE, as in most Lisp dialects, begin with a semi-colon, with their number having conventional semantic value:

* **Four Semi-colons**: These are used for file headers and important comments that apply to large sections of code in a source file.
* **Three Semi-colons**: These are used to begin comments that apply to just one top-level form or small group of top-level forms.
* **Two Semi-colons**: These are used inside a top-level form, for comments appearing between lines. For code that uses unobvious forms to accomplish a task, you must include a comment stating the purpose of the form and the task it accomplishes.
* **One Semi-colon**: This is used for parenthetical remark and _only_ occurs at the end of a line. You should use spaces to separate the comment from the code it refers to so the comment stands out. You should try to vertically align consecutive related end-of-line comments.

For all comments, there should be a space between the semicolon and the text of the comment.

```lisp
;;;; File-level comments or comments for large sections of code.
(defmodule math-n-things
  (export
   (utility-function 0)
   ...
   (small-prime-number? 1)
   (large-prime-number? 1)
   ...))

;;; The functions in this section are utility in nature, supporting others in
;;; the module. More details on their intended use cases are available here:
;;; * https://some.url/

(defun utility-function ()
  ...)

;;; Prime numbers section

(defun small-prime-number? (n)
  "Return true if N, an integer, is a prime number. Otherwise, return false."
  ((n) (when (< n 4))  ; parenthetical remark here
   (>= n 2))           ; continuation of the remark
  ((n) (when (== 0 (rem n 2)))
   'false)             ; different remark here
  ((n)
   ;; Comment that applies to a section of code.
   (lists:all #'not/1
              (lists:map (lambda (x) (== 0 (rem n x)))
                         (lists:seq 3 (trunc (math:sqrt n)))))))

(defun large-prime-number? (n)
  ...)
```
