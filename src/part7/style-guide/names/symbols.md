# Symbols

Use lower case for all symbols (Erlang "atoms"). Consistently using lower case makes searching for symbol names easier and is more readable.

Place hyphens between all the words in a symbol. If you can't easily say an identifier out loud, it is probably badly named.

Always prefer `-` over `/` or `.` unless you have a well-documented overarching reason to, and approval from other hackers who review your proposal.

Bad:

```text
(defun *default-username* ()"Ann")
(defun *max-widget-cnt* () 200)
```

Better:

```lisp
(defun *default-user-name* () "Ann")
(defun *maximum-widget-count* () 200)
```

Unless the scope of a variable is very small, do not use overly short names like `i` and `zq`. 
