# binary-to-list: The Decomposer

The inverse operation is equally straightforward:

```lfe
(binary-to-list binary)
```

This extracts each byte from the binary and produces a list of integers:

```lfe
lfe> (binary-to-list #B(1 2 3))
(1 2 3)
lfe> (binary-to-list #"Hello")
(72 101 108 108 111)
```

Why would you want to convert a space-efficient binary into a less-efficient list? Perhaps you need to manipulate individual elements and the list structure makes that more convenient. Perhaps you're interfacing with code that expects lists. Perhaps you're being deliberately perverse for artistic purposes. The function doesn't judge; it merely transforms.

There's also a variant that converts a slice of the binary:

```lfe
(binary-to-list binary start end)
```

Where `start` and `end` are byte positions (1-indexed, because someone, somewhere, decided that counting should start at 1 despite computers preferring 0, and we're all just living with the consequences). This allows you to extract a portion of the binary without converting the entire thing to a list first.
