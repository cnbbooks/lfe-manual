# The General Form

The bit syntax uses the `binary` form (or `#B(...)` for construction) with segments that can be annotated:

```lfe
(binary segment1 segment2 ... segmentN)
```

Each segment can be:

- A simple value: `42`
- A value with size: `(42 (size 16))`
- A value with type: `(42 integer)`
- A value with size and type: `(42 (size 16) integer)`
- A value with various other qualifiers we'll explore momentarily

The pattern is consistent: value first, then optional annotations. The annotations provide additional information about how to interpret or pack the bits for that segment.
