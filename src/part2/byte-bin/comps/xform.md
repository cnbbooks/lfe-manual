# Transforming Values

Binary comprehensions can transform values during generation:

```lfe
lfe> (binary-comp ((<<n>> (list-gen (<- '(1 2 3 4 5))))) (* n 2))
#B(2 4 6 8 10)
```

Each number is doubled before being packed into the binary. This is useful for scaling pixel values, applying gain to audio samples, or any number of byte-level transformations that would otherwise require explicit loops.
