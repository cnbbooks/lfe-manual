# The Parable of Big-Endian and Little-Endian

The terms "big-endian" and "little-endian" come from Jonathan Swift's *Gulliver's Travels*, where two fictional kingdoms went to war over which end of a boiled egg should be opened first. Computer engineers, finding this satirical commentary on human pettiness to be mirrored in the constant byte-bickering of the 70s and 80s, decided to apply the same principle.

## The Two Schools of Thought

**Big-Endian** (also called "network byte order"):

- Most significant byte comes first
- Reads naturally from left to right (for humans accustomed to Western number systems)
- Used by network protocols, Java Virtual Machine, and certain RISC processors

**Little-Endian**:

- Least significant byte comes first
- Reads backwards (from a human perspective)
- Used by Intel x86/x64, most ARM processors, and assorted rebels

## A Concrete Example

Consider the 32-bit hexadecimal number `0x12345678`. How would different systems store this in memory?

```lfe
lfe> (set num #x12345678)
305419896

; Big-endian representation (most significant byte first):
lfe> (binary ((num (size 32) big)))
#B(18 52 86 120)  ; 0x12, 0x34, 0x56, 0x78

; Little-endian representation (least significant byte first):
lfe> (binary ((num (size 32) little)))
#B(120 86 52 18)  ; 0x78, 0x56, 0x34, 0x12
```

Notice how the bytes are in completely opposite order. This is why cross-platform binary protocols require careful specification of byte order—what looks like `0x12345678` on one system might be interpreted as `0x78563412` on another.
