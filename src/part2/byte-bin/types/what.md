# The Type Itself

The fundamental type specifiers tell the system what kind of value the bits represent:

## integer

The default type. Bits are interpreted as a signed or unsigned integer (depending on the sign specifier, which we'll discuss in a moment). If no size is specified, 8 bits is assumed, because bytes remain the atomic unit of traditional computing despite our best efforts to think in arbitrary bit quantities.

```lfe
lfe> (binary (42 integer))
#B(42)
lfe> (binary (42 (size 16) integer))
#B(0 42)
```

Integers are straightforward, which is pleasant. They're numbers. They fit in a specified number of bits. There's no floating-point imprecision to worry about, no encoding ambiguities, just bits in a row meaning what bits in a row have meant since boolean algebra was invented: numbers, but in base 2, because base 10 is for humans and we're talking to silicon now.

## float

Floating-point numbers, represented in IEEE 754 format. These require either 32 bits (single precision) or 64 bits (double precision). If you specify any other size, the system will object strenuously, because IEEE 754 doesn't define formats for arbitrary bit widths and inventing new ones would be the kind of creative interpretation that ends with incompatible data and sad engineers.

```lfe
lfe> (binary (3.14159 (size 32) float))
#B(64 73 15 209)
lfe> (binary (3.14159 (size 64) float))
#B(64 9 33 245 249 84 68 45)
```

The 32-bit version has less precision, which you'd notice if you were doing extensive calculations with it, or if you were a physicist worried about whether your simulation of quantum mechanics was accurate to within acceptable rounding error. For drawing circles on computer screens, 32 bits is typically fine. For landing spacecraft on distant planets, you might want to reconsider.

The default size for floats is 64 bits, because at some point someone decided that double precision should be the default and everyone else went along with it, probably because arguing about default floating-point precision is the kind of thing that makes standardization committees long for retirement.

## binary

This specifies that the segment is itself a complete binary that should be included in its entirety:

```lfe
lfe> (let ((inner #B(1 2 3)))
    (binary (inner binary)))
#B(1 2 3)
```

The size specifier, if provided, must be divisible by 8, because binaries are byte-aligned by definition. If you try `(binary (#B(1) (size 7) binary))`, the system will inform you of your error, typically at compile time, which is better than runtime, which is better than never.

When pattern matching, the `binary` type consumes the rest of the binary unless you specify a size:

```lfe
lfe> (let (((binary (head (size 8)) (tail binary)) #B(1 2 3 4 5)))
    (list head tail))
(1 #B(2 3 4 5))
```

This is extraordinarily useful for protocol parsing where you need to extract a header and then process the rest of the data separately. It's also useful for implementing streaming processing, where you take a bit, do something with it, then recursively process the remainder, which is the functional programming way of saying "loop" without admitting that you're essentially looping.

## bitstring

Like `binary`, but doesn't require byte alignment. This lets you work with data at arbitrary bit boundaries:

```lfe
lfe> (let (((binary (bits1 (size 3) bitstring) (bits2 (size 5) bitstring))
         #B(156)))  ; 10011100 in binary
    (list (bit-size bits1) (bit-size bits2)))
(3 5)
```

Here we've split a byte into a 3-bit chunk and a 5-bit chunk. The result isn't two integers—it's two bitstrings, which are like binaries but more flexible about their alignment, like yoga practitioners of the data structure world.

## bytes and bits

Synonyms for `binary` and `bitstring`, respectively, provided for those who prefer the plural form or who learned the syntax from documentation that used these terms. Language designers, in an unusual fit of generosity, sometimes provide multiple ways to say the same thing, presumably to reduce the learning curve for people coming from different backgrounds, or possibly just to give everyone something to argue about in code reviews.

## utf8, utf16, utf32

These specify Unicode encodings. They're used for handling text in various Unicode Transformation Formats:

```lfe
lfe> (binary (#\α utf8))
#B(206 177)
lfe> (binary (#\α utf16))
#B(3 177)
lfe> (binary (#\α utf32))
#B(0 0 3 177)
```

The Greek letter alpha (α, Unicode code point U+03B1) is encoded differently in each format. UTF-8 uses 2 bytes, UTF-16 uses 2 bytes, and UTF-32 uses 4 bytes. This is because the UTF formats have different properties: UTF-8 is space-efficient for ASCII and variable-length, UTF-16 is space-efficient for most modern scripts but still variable-length, and UTF-32 is fixed-length but wastes space for anything that isn't using the full Unicode range.

The choice of encoding is a complex topic involving trade-offs between space efficiency, parsing complexity, and whether your data is mostly ASCII or includes substantial non-Latin scripts. Entire flame wars have been fought over this topic, with participants taking positions ranging from "UTF-8 for everything" to "UTF-16 for everything" to "why not just use ASCII and pretend the rest of the world doesn't exist?" (The last position is generally held by people who don't realize they're advocating for essentially 1960s-era computing.)

For these types, the size is determined by the encoding and cannot be explicitly specified, because suggesting that UTF-8 should use a different number of bytes than UTF-8 specifies would be creating a new encoding, and we have enough encodings already, thank you.
