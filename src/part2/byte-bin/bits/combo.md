# Combining Size and Type

You can specify both size and type:

```lfe
lfe> (binary (1.23 (size 32) float))
#B(63 157 112 164)
lfe> (binary (1.23 (size 64) float))
#B(63 243 174 20 122 225 71 174)
```

The 32-bit float is less precise than the 64-bit version, which is why they're different. This is the fundamental trade-off in floating-point representation: more bits give you more precision, at the cost of more bits. When you have millions of floating-point numbers and limited bandwidth or storage, those extra 32 bits per number can add up to "can we afford the hardware to store all this?" versus "it barely fits." Compression algorithms have entire doctoral theses dedicated to this topic.

For integers, specifying both size and type looks like:

```lfe
lfe> (binary (42 (size 16) integer))
#B(0 42)
```

Though since `integer` is the default type, the type annotation is typically omitted unless you're being exceptionally explicit for documentation purposes, or your team has a style guide written by someone who believes that verbosity correlates with correctness (a belief that is often tested in code review sessions where "can we make this simpler?" meets "but the specification says...").
