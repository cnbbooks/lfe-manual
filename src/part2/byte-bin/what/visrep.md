# Visual Representation

When the REPL prints a binary, it uses the notation `#B(...)` (or sometimes `<<...>>` if you're looking at Erlang documentation). The contents are displayed as integers, each representing a byte:

```lfe
lfe> #B(1 2 3)
#B(1 2 3)
```

If the binary happens to contain bytes that form printable ASCII characters, the shell might helpfully display it as a string:

```lfe
lfe> #B(72 101 108 108 111)
#"Hello"
```

Note the `#"..."` notation for binary strings. This is the shell being clever, recognizing that the bytes `72 101 108 108 111` correspond to the ASCII codes for "Hello" and presenting them in a more readable format. The bytes haven't changed—only their cosmetic presentation has been upgraded for human consumption.

This is similar to how the number `42` and the string `"42"` are different beasts, even though they might occasionally be mistaken for each other at parties. One is a numeric value; the other is a sequence of character codes that, when interpreted by a human visual system, evoke the *concept* of forty-two. The distinction matters more than you might think.
