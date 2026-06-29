# Default Sizes

LFE, being a sensible language designed by beings who've thought about these things, provides reasonable defaults:

- **Integers**: 8 bits (1 byte), because bytes are the lingua franca of data
- **Floats**: 64 bits (8 bytes), because IEEE 754 double-precision is the standard, and standards exist for a reason
- **Binaries**: Whatever size they actually are, which is refreshingly honest

Let's observe these defaults in their natural habitat:

```lfe
lfe> (set default-int (binary 42))
#B(42)
lfe> (byte_size default-int)
1

lfe> (set default-float (binary (3.14159 float)))
#B(64 64 9 33 251 84 68 45)
lfe> (byte_size default-float)
8
```
