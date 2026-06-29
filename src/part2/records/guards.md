# Guards and Records: Testing for Type

The BIF `is_record/2` lets you check if something is a record of a particular type:

```lfe
(defun process-person (p)
  (if (is_record p 'person)
    (do-person-things p)
    (io:format "That's not a person!~n")))
```

Or in a guard:

```lfe
(defun process-person (p) (when (is_record p 'person))
  (do-person-things p))
```

This checks three things:
1. Is `p` a tuple?
2. Is the first element the atom `person`?
3. Is the tuple the right size?

If all three conditions hold, congratulations: you probably have a person record. Or an extremely coincidental tuple. The universe tends not to make such distinctions.
