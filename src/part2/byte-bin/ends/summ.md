# Summary

```
Is your data multi-byte? (size > 8)
├─ No → Endianness doesn't matter
└─ Yes →
    ├─ Network/portable format? → Use `big`
    ├─ Local storage only? → Use `native` (for performance)
    └─ Specific platform requirement? → Use whatever that platform uses
```

Remember: The default is `big`, which is almost always what you want for interoperability. When in doubt, use big-endian and spare yourself the debugging sessions that come from accidentally mixing byte orders.
