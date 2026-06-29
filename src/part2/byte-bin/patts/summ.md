# Summary

The beauty of binary pattern matching lies in its symmetry with construction. If you can write:

```lfe
(binary ((type (size 8)) (len (size 16) big) (data (size len) binary)))
```

to *construct* a binary, you can use the exact same pattern to *deconstruct* it. This makes code that handles binary protocols remarkably readable—the pattern itself documents the structure of the data.

This is protocol parsing as it was meant to be: declarative, obvious, and less prone to off-by-one errors than manual bit-shifting in C. It's one of those rare cases where the elegant solution is also the practical one, rather like discovering that your towel is, improbably, exactly the right size for everything you need it to do.
