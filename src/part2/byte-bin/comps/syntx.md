# The Basic Syntax

A binary comprehension has two primary forms, distinguished by what they produce:

## Form 1: Producing Lists from Binaries

```lfe
(list-comp (<qual> ...) (<binary-generator> ...) [<guard> ...])
```

This generates a list by iterating over binary data. The syntax `(<= binary-pattern source-binary)` creates a binary generator.

## Form 2: Producing Binaries

```lfe
(binary-comp (<binary-qual> ...) (<binary-generator> ...) [<guard> ...])
```

This generates a binary by iterating over binary (or list) data. The binary qualifier must be wrapped in `<< >>` (conceptually speaking—LFE uses the `binary` form).
