# File Headers

 Every source file should begin with a brief description of the contents of that file.

After that description, every file should start the code itself with a `(defmodule ...)` form.

```lisp
;;;; Variable length encoding for integers and floating point numbers.

(defmodule num-encode
  ...)
```

It is not necessary to include copyright info in every file as long as the project has a `LICENSE` file in its top-level directory. Files which differ in license from that file should get have a copyright notice in their header section.

If you are contributing to a project that has established a convention of adding copyright headers to all files, simply follow that convention.
