# Indentation

In general, use your text editor's indentation capabilities. If you are contributing to a particular library, be sure to ask the maintainers what standard they use, and follow those same guidelines, thus saving everyone from the drudgery of whitespace fixes later.

In particular, you'll want to do everything you can to follow the conventions laid out in the Emacs LFE mode supplied in the [LFE source](https://github.com/rvirding/lfe/tree/develop/emacs). Instructions for use are given in the [LFE Github wiki](https://github.com/rvirding/lfe/wiki/Emacs-setup), but we'll repeat it here. Simply edit your `~/.emacs` file to inlcude the following:

```lisp
;; Prevent tabs being added:
(setq-default indent-tabs-mode nil)

;; LFE mode.
;; Set lfe-dir to point to where the lfe emacs files are.
(defvar lfe-dir (concat (getenv "HOME") "/git/lfe/emacs"))
(setq load-path (cons lfe-dir load-path))
(require 'lfe-start)
```

In general though, indentation is two lines per form, for instance:

```lisp
(defun f ()
  (let ((x 1)
        (y 2))
    (lfe_io:format "X=~p, Y=~p~n"  (list x y))))
```

Note that LFE has many exceptions to this rule, given the complex forms it defines for features inheritied from Erlang (e.g., pattern-matching in function heads). A few examples for the number exceptions to the two-space indentation rule above:

```lisp
(cond ((lists:member x '(1 2 3)) "First three")
      ((=:= x 4) "Is four")
      ((>= x 5) "More than four")
      ('true "You chose poorly"))
```

```lisp
(defun ackermann
  ((0 n)
   (+ n 1))
  ((m 0)
   (ackermann (- m 1) 1))
  ((m n)
   (ackermann (- m 1) (ackermann m (- n 1)))))
```

The last function would actually be better written as follows, but the form above demonstrates the indentation point:

```lisp
(defun ackermann
  ((0 n) (+ n 1))
  ((m 0) (ackermann (- m 1) 1))
  ((m n) (ackermann (- m 1) (ackermann m (- n 1)))))
```


Maintain a consistent indentation style throughout a project.

Indent carefully to make the code easier to understand.

 Use indentation to make complex function applications easier to read. When an application does not fit on one line or the function takes many arguments, consider inserting newlines between the arguments so that each one is on a separate line. However, do not insert newlines in a way that makes it hard to tell how many arguments the function takes or where an argument form starts and ends.

Bad:
```text
(do-something first-argument second-argument (lambda (x)
    (frob x)) fourth-argument last-argument)
```

Better:
```lisp
(do-something first-argument
              second-argument
              (lambda (x) (frob x))
              fourth-argument
              last-argument)
```
