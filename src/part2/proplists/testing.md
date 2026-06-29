# The Proplist Testing Pattern

When writing tests for functions that take options, the proplist pattern enables easy test case generation:

```lfe
(defun test-connect-variants ()
  (let ((base-opts '((host "localhost") (port 8080))))
    (lists:foreach
      (lambda (extra-opts)
        (let ((opts (++ extra-opts base-opts)))
          (test-connect opts)))
      '(((timeout 1000))
        ((timeout 5000) (retry 3))
        ((ssl true) (certfile "/path/to/cert"))
        (verbose)))))
```

This allows testing multiple option combinations by composing base options with variants—difficult to do cleanly with records, natural with proplists.
