# Inclusion

LFE supports including header files and library files, which is essential for larger projects and when working with Erlang/OTP libraries.

## `include-lib`

The `include-lib` directive allows you to include files from installed OTP applications or other libraries:

```lisp
(include-lib "kernel/include/file.hrl")
(include-lib "stdlib/include/qlc.hrl")
```

This searches for the include file in the standard library locations and makes the definitions available in your code.

## `include-file`

The `include-file` directive includes files using relative or absolute paths:

```lisp
(include-file "local-definitions.lfe")
(include-file "../shared/common.lfe")
(include-file "/absolute/path/to/file.lfe")
```

## Include File Content

Include files typically contain:

* Record definitions
* Macro definitions
* Constant definitions
* Type specifications

Example include file (`records.lfe`):

```lisp
(defrecord person
  name
  age
  email)

(defmacro debug (msg)
  `(io:format "DEBUG: ~p~n" (list ,msg)))
```

After including this file, you can use the record and macro definitions in your code:

```lisp
lfe> (include-file "records.lfe")
debug

;; In LFE, when including a file in the REPL, the last
;; function defined in the file is printed to stdout.

lfe> (make-person name "Robert" age 54 email "robert@lfe.io")
#(person "Robert" 54 "robert@lfe.io")
lfe>
```

```lisp
lfe> (debug "oops")
DEBUG: "oops"
ok
```
