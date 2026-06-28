# Constructing Simple Binaries

To create a binary containing actual data, you simply list the values between the parentheses:

```lfe
> #B(1 2 3)
#B(1 2 3)
```

Each integer you provide becomes a byte in the resulting binary. The values must be in the range 0 to 255, because that's what fits in 8 bits, and attempting to stuff larger numbers into bytes is a bit like trying to fit an elephant into a phone booth—technically possible if you're willing to ignore conventional constraints like physics, but generally frowned upon by those who have to clean up afterward.

```lfe
> #B(42)
#B(42)
> #B(255)
#B(255)
> #B(256)
; This will result in an error, because 256 requires 9 bits
```

The system will politely inform you when you've exceeded the byte boundary, typically by throwing an exception. It's being helpful, really. Better to know at compile time than to discover later that your carefully constructed binary has been truncated in mysterious and exciting ways.
