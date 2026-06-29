# Summary

Binary manipulation in LFE/Erlang is one of those rare features that's both powerful and elegant. The bit syntax lets you write code that:

- Directly expresses protocol specifications
- Performs bit-level operations without manual shifting/masking
- Handles non-byte-aligned data naturally
- Compiles to efficient machine code

These examples demonstrate real-world patterns you'll encounter:

- Protocol parsing (IPv4)
- Custom protocol design (Towel Service)
- Binary file formats (logging)

The techniques here scale from tiny embedded devices to massive distributed systems. They've been battle-tested in telecommunications, embedded systems, network infrastructure, and more.

The key insight is that when you need to manipulate binary data, fighting against byte boundaries and bit positions is unnecessary pain. LFE gives you the tools to express what you mean directly, and the compiler optimizes it into fast machine code. This is how programming should work: express your intent clearly, let the computer handle the tedious details.

Now go forth and manipulate binaries with confidence. Just remember to bring your towel.
