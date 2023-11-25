# Names in Modules

 When naming a symbol in a module, you should not include the module name as part of the name of the symbol. Naming a symbol this way makes it awkward to use from a client module accessing the symbol by qualifying it with a module prefix, where the module name then appears twice (once as part of the module name, another time as part of the symbol name itself).

 Bad:

 ```text
(defmodule varint
  (export
    (varint-length64 0))

(defun varint-length64 () ... )

(defmodule client-code)

(defun +padding+ ()
  (varint:varint-length64))
```

Better:

```lisp
(defmodule varint
  (export
    (length64 0))

(defun length64 () ... )

(defmodule client-code)

(defun +padding+ ()
  (varint:length64))
```
