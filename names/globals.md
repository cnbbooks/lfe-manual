# Global Variables and Constants

Erlang, and thus LFE, does not support global variables or mutable data. However, many projects define constants in modules. Traditionally, Lisp projects have used symbols enclosed in `+` for global constants and symbols enclosed in `*` (a.k.a. "earmuffs") for global variables.

Adapted for LFE, one could use these conventions for module constants and default values, respectively.

```lisp
(defun +my-pi+ () 3.14)
(defun *default-host* () "127.0.0.1")
```
