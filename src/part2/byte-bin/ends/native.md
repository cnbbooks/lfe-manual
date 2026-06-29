# The Native Option: Runtime Flexibility

The `native` specifier tells LFE to use whatever byte order your CPU natively prefers. This is determined at runtime, making your code portable while still being efficient:

```lfe
lfe> (binary ((12345 (size 32) native)))
#B(0 0 48 57)  ; Result depends on your CPU architecture
```

On an Intel x86-64 system (little-endian):

```lfe
#B(57 48 0 0)
```

On a SPARC or PowerPC system (big-endian):

```lfe
#B(0 0 48 57)
```

## When to Use Native

Use `native` when:

- You're storing binary data that will only be read on the same system
- Performance is critical (native operations avoid byte-swapping overhead)
- You're working with platform-specific file formats

**Don't** use `native` when:

- Exchanging data over networks
- Writing portable file formats
- Any scenario where the data might cross architectural boundaries
