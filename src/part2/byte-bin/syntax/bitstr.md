# Binary Strings

A particularly convenient special case involves text. Since strings in many contexts are just sequences of character codes, you can create a binary directly from string literals:

```lfe
lfe> #"Hello"
#"Hello"
```

This is exactly equivalent to:

```lfe
lfe> #B(72 101 108 108 111)
#"Hello"
```

The ASCII codes for H, e, l, l, and o are 72, 101, 108, 108, and 111, respectively. You can verify this with a character code lookup, or by asking any passing computer scientist, though the latter approach may result in a lengthy discussion about character encodings and why Unicode was necessary and why UTF-8 is simultaneously brilliant and a source of ongoing existential dread.

Binary strings are particularly efficient for storing text that you don't plan to manipulate character-by-character. If you're reading a file and want to keep its contents in memory without converting everything to a list (which, remember, has pointer overhead for every element), binary strings are your friend. A friend who doesn't talk much and just does their job efficiently, like a particularly competent butler.

The syntax `#"..."` is exactly like the list string syntax `"..."`, except the result is a binary instead of a list. Everything about escape sequences works identically:

```lfe
lfe> #"Line 1\nLine 2"
#"Line 1\nLine 2"
lfe> #"Tab\there"
#"Tab\there"
```

You can even use the hexadecimal escape sequences:

```lfe
lfe> #"\x48\x65\x6c\x6c\x6f"
#"Hello"
```

Though one might reasonably ask why you'd want to write "Hello" in hexadecimal, unless you're being deliberately obscure for job security purposes. Security through obscurity might not work for cryptography, but it works reasonably well for ensuring you're the only person who can maintain your code.
