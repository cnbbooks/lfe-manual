# Debugging Binary Code: When Patterns Don't Match

Binary patterns can fail silently (well, with exceptions) when they don't match. Helpful debugging:

```lfe
(defun debug-parse (binary)
  "Wrapper that shows why parsing failed."
  (try
    (parse-packet binary)
    (catch
      ((tuple 'error 'badmatch value 'stack stacktrace)
       (lfe_io:format "Binary match failed!~n")
       (lfe_io:format "Binary size: ~p bytes~n" (list (byte_size binary)))
       (lfe_io:format "First 20 bytes: ~p~n"
                     (list (binary:part binary 0 (min 20 (byte_size binary)))))
       (tuple 'error 'parse-failed)))))
```

Use `io:format("~p~n", [Binary])` liberally during development. The shell shows binaries in a readable format that helps spot patterns.
