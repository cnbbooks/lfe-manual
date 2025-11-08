# The List-to-Binary Bridge

Speaking of `list-to-binary`, this BIF (Built-In Function) is your primary tool for converting list-based data into binary form:

```lfe
lfe> (list-to-binary '(72 101 108 108 111))
#"Hello"
```

It accepts what Erlang calls an "iolist"—a structure that can be a flat list of integers (0-255) and binaries, or a nested structure of the same. The nesting can be arbitrarily deep, because sometimes data structures grow organically and you don't want to flatten everything just to convert it to a binary:

```lfe
lfe> (list-to-binary (list #B(1 2) (list 3 4) #B(5 6)))
#B(1 2 3 4 5 6)
```

Note how the nested list `(list 3 4)` was automatically flattened. The system understands that you want all the bytes, regardless of how they were structured in your list. It's remarkably forgiving, which is pleasant when you're dealing with data that was accumulated through various means and hasn't been tidied up into a perfectly flat structure.
