# Bit Syntax Fundamentals

## In Which We Descend Into Particular Detail

The bit syntax is where LFE's approach to binary data transforms from "convenient" into "why doesn't every language do this?" It's a notation for describing binary data at arbitrary bit-level granularity, and it does something that would be remarkable if it weren't so obviously sensible: it makes code look like the *specification* of the binary format you're working with.

This is worth pausing to appreciate. In most languages, if someone hands you a protocol specification that says "the packet consists of a 3-bit flag field, followed by a 13-bit length field, followed by an 8-bit type identifier," your next step involves a calculator, several bitwise operators, a growing sense of dread, and possibly a therapeutic discussion with a rubber duck about whether this career path was really the right choice.

In LFE, you write something very close to: `(binary (flag (size 3)) (length (size 13)) (type (size 8)))`. The code *reads* like the spec. This is not an accident. This is deliberate design by people who had to implement telecommunications protocols and decided that life was too short for bit-masking gymnastics.
