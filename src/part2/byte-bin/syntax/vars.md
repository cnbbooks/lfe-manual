# Variables in Binary Construction

You can use variables in binary construction, as we just demonstrated:

```lfe
lfe> (let ((x 42))
    #B(x))
#B(42)
```

The variable is evaluated and its value is inserted into the binary. This works as long as the value is something sensible—an integer in the correct range, another binary, or a string. Attempting to insert tuples, lists, or atoms directly will result in the system politely declining your request, typically by throwing a bad argument exception.

If you want to insert complex terms into binaries, you'll need to serialize them first, which we'll discuss in a later chapter. For now, just remember: binaries like numbers and other binaries. They're not particularly sociable with the more structured data types, preferring to maintain their raw, untyped aesthetic.
