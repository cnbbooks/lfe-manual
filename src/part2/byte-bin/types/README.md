# Type Specifiers

## On the Nature of Types in Binary Segments

If the bit syntax is a language for describing binary structures, then type specifiers are its vocabulary—or possibly its accent marks and articles, depending on how metaphorically inclined you're feeling today. They modify and qualify segments, telling the system not just "there are bits here" but rather "these bits mean something specific, please interpret them accordingly."

The full specification of a segment can be positively baroque:

```lfe
(value (size N) type-spec1 type-spec2 ... type-specN)
```

Where each `type-spec` can modify how the value is interpreted or stored. The order doesn't matter (except to pedants and to the parser, which cares deeply about syntax but not about your feelings), and you can omit any specifiers you don't need, in which case sensible defaults are applied. The defaults are "sensible" in the way that most defaults are sensible: reasonable for the common case, potentially surprising in edge cases, and the source of occasional debugging sessions where you discover that what you thought was happening wasn't actually happening because of an implicit assumption you didn't realize you'd made.

Welcome to software engineering. We have coffee, because we need it.
