# Endianness

When a value spans multiple bytes, the byte order matters:

## big (big-endian)

The default. Most significant byte first:

```lfe
lfe> (binary (16#1234 (size 16) big))
#B(18 52)  ; 0x12 0x34
```

Called "big-endian" because the big end (most significant byte) comes first. Also called "network byte order" because network protocols standardized on this representation, probably because the alternative was every machine interpreting every other machine's data incorrectly and no one being able to communicate, which would rather defeat the purpose of networking.

## little (little-endian)

Least significant byte first:

```lfe
lfe> (binary (16#1234 (size 16) little))
#B(52 18)  ; 0x34 0x12
```

Called "little-endian" because the little end comes first. Also called "the Intel way" by those who associate it with x86 processors, which is technically correct but glosses over the fact that many processor architectures have used little-endian representation and Intel didn't invent it, they just popularized it through market dominance.

## native

Determined at runtime based on the machine's native byte order:

```lfe
lfe> (binary (16#1234 (size 16) native))
; Result depends on your CPU architecture
```

On Intel processors, this is little-endian. On most network equipment and older RISC processors, it's big-endian. On some exotic architectures that almost nobody uses, it might be something else entirely, like middle-endian, which exists primarily to make computer architecture textbooks more interesting and to give programming language designers nightmares about portability.

Using `native` is appropriate when:

1. You're reading/writing data on the same machine and don't care about portability
2. You want maximum performance and are willing to deal with potential portability issues
3. You're implementing something that explicitly needs to respect the machine's native format

It's inappropriate when you need to exchange data between machines that might have different architectures, which is approximately 90% of binary data interchange scenarios in modern computing.

Endianness only affects values that span multiple bytes. Single-byte values are immune to byte-order issues, because there's only one byte and thus only one possible ordering for it. This is one of the arguments for using byte-aligned protocols when possible: fewer opportunities for endianness bugs.
