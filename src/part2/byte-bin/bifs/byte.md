# byte-size: The Measurer

To find out how many bytes are in a binary:

```lfe
(byte-size binary)
```

Simple enough:

```lfe
lfe> (byte-size #B(1 2 3 4 5))
5
```

For bitstrings that aren't byte-aligned, this returns the number of bytes required to contain all the bits, rounded up:

```lfe
lfe> (let ((bs (binary (1 (size 9)))))  ; 9 bits
    (byte-size bs))
2
```

Nine bits require two bytes to store, because bytes, being stubbornly eight bits each, don't come in fractions despite what one might wish when dealing with data that refuses to align itself properly.
