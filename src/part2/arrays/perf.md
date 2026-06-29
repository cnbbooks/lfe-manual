# Performance Characteristics

Arrays in Erlang are implemented using a tree structure that provides the following complexity characteristics:

- **Access by index:** O(log n) — better than lists, worse than actual arrays in languages with mutable memory
- **Setting a value:** O(log n) — creates a new array with structural sharing
- **Size queries:** O(1) — constant time
- **Conversion to/from lists:** O(n) — must visit every element

This makes arrays most suitable for scenarios where:

1. You need better-than-linear access to arbitrary indices
2. You're not doing constant-time updates (this is functional programming, after all)
3. The numeric indices are meaningful in your problem domain
4. You're not obsessing about the zero-based indexing
