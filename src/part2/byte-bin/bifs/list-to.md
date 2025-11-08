# list-to-binary: The Great Flattener

We've already met `list-to-binary`, but it deserves a more formal introduction:

```lfe
(list-to-binary list-of-bytes-and-binaries)
```

This function takes what Erlang calls an "iolist"—a deeply recursive definition that essentially means "a list containing integers (0-255), binaries, or other iolists." The function then flattens this entire structure into a single contiguous binary, removing all the list overhead and leaving you with just the raw bytes.

```lfe
lfe> (list-to-binary '(1 2 3))
#B(1 2 3)
lfe> (list-to-binary (list #B(1 2) 3 (list 4 5) #B(6)))
#B(1 2 3 4 5 6)
```

Note how nesting is handled transparently. The function doesn't care how your data is structured—it cares only about the bytes, and those it will find no matter how deeply you've hidden them in nested lists. It's like having a particularly determined detective who will find what they're looking for even if you've buried it under seventeen layers of bureaucratic paper trails and misdirection.
