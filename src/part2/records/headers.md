# Header Files: Sharing Records Between Modules

When multiple modules need the same record definition, you use header files (with a `.lfe` extension by convention):

```lfe
;;; person.lfe - record definitions
(defrecord person
  name
  (age 0)
  (phone ""))
```

Then include it in your modules:

```lfe
(defmodule contacts
  (export all))

(include-file "person.lfe")

(defun make-contact (n a p)
  (make-person name n age a phone p))
```

**Important Philosophical Point**: While sharing record definitions via header files is common in the wild, it's often better to keep record definitions private to a single module and provide accessor functions for other modules to use. This prevents tight coupling, makes refactoring easier, and generally keeps your code from turning into the kind of tangled mess that causes programmers to take up gardening instead.
