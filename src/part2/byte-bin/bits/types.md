# Type Annotations

You can explicitly specify the type of a segment:

```lfe
(value type-specifier)
```

The available types include:

- `integer` (the default)
- `float`
- `binary`
- `bitstring`
- `bytes` (synonym for binary)
- `bits` (synonym for bitstring)
- `utf8`, `utf16`, `utf32` (for Unicode text)

```lfe
lfe> (binary (1.23 float))
#B(63 243 174 20 122 225 71 174)
lfe> (binary (42 integer))
#B(42)
lfe> (binary (#"hello" binary))
#"hello"
```

When you specify `binary` as a type, you're saying "this segment is itself a binary, and should be included in its entirety." This is how you concatenate binaries within the bit syntax:

```lfe
lfe> (let ((b1 #B(1 2))
        (b2 #B(3 4)))
    (binary (b1 binary) (b2 binary)))
#B(1 2 3 4)
```

The `binary` type annotation is required here because otherwise the system wouldn't know whether you meant "include the binary" or "include the integer that represents some aspect of the binary" or possibly "make me a sandwich." Explicit is better than implicit, as some Python programmer probably said once, though that principle is more universally applicable than its origin might suggest.
