# Mixing Integers and Strings

The `#B(...)` syntax allows you to mix integers and strings freely:

```lfe
lfe> #B(1 2 "Hello" 3 4)
#B(1 2 72 101 108 108 111 3 4)
```

Note how the string "Hello" was expanded into its constituent byte values. The binary doesn't remember that those bytes once belonged to a string—it just stores the raw values. The distinction between "a byte containing 72" and "the letter H" exists only in our meat-based interpretation systems, not in the silicon.

This mixing capability is occasionally useful when you need to construct a binary containing both protocol-specific numeric values and actual text data. Which is to say, it's useful frequently, because protocols are designed by people who can never quite decide whether they want to use magic numbers or human-readable identifiers.
