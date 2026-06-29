# Security Considerations: Trust No Binary

An important warning: `binary-to-term` will happily reconstruct any valid term, including atoms. If you deserialize untrusted binaries, you could exhaust your atom table (atoms aren't garbage collected). For untrusted input, use:

```lfe
lfe> (binary-to-term untrusted-binary (list 'safe))
```

The `'safe` option prevents creation of new atoms, resources, and functions. It's the equivalent of wearing a towel—not foolproof protection, but sensible nonetheless.
