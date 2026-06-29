# Special Cases and Edge Conditions

Some combinations are invalid and will generate errors:

- Signed/unsigned with floats (floats have their own sign representation)
- Endianness with single-byte values (there's no byte order when there's only one byte)
- Non-multiple-of-8 sizes for binary type
- UTF types with explicit sizes (the encoding determines the size)
- Unit specifications with UTF types

The system will generally catch these at compile time if the values are known, or at runtime if they're determined by variables. Either way, you'll be informed of your mistake, which is better than silently producing incorrect results, which is the approach taken by languages that believe in giving programmers enough rope to hang themselves and then selling them more rope.
