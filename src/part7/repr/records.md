# Records

## Use records as the principle data structure

Use records as the principle data structure in messages. A record is a tagged tuple and was introduced in Erlang version 4.3 and thereafter.

If the record is to be used in several modules, its definition should be placed in a header file (with suffix `.lfe`) that is included from the modules. If the record is only used from within one module, the definition of the record should be in the beginning of the file the module is defined in.

The record features of LFE can be used to ensure cross module consistency of data structures and should therefore be used by interface functions when passing data structures between modules.

## Use selectors and constructors

Use the record macros provided by LFE for managing instances of records. Don't use matching that explicitly assumes that the record is a tuple.

Bad:

```lisp
(defun demo ()
  (let* ((joe (make-person name "Joe" age 29))
         (`#(person ,name ,age) joe))
    ...
    ))
```

Good:

```lisp
(defun demo ()
  (let* ((joe (make-person name "Joe" age 29))
         (name-2 (person-name joe)))
    ...
    ))
```
