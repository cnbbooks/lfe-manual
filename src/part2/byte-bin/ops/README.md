# Bitwise Operators

> "In the beginning the Universe was created. This has made a lot of people very angry and been widely regarded as a bad move." — Douglas Adams

In a similar vein, someone decided that Boolean algebra should be applied directly to the individual bits of integers, creating bitwise operators. This has made a lot of programmers very confused and been widely regarded as powerful, if occasionally bewildering.

LFE inherits Erlang's bitwise operators, which operate on integers at the bit level. Unlike the binary syntax (which constructs and deconstructs binary data structures), bitwise operators perform logical and shift operations on integer values interpreted as sequences of bits.

The available operators are:

| Operator | Operation | Example |
|----------|-----------|---------|
| `(band A B)` | Bitwise AND | `(band 12 10)` → `8` |
| `(bor A B)` | Bitwise OR | `(bor 12 10)` → `14` |
| `(bxor A B)` | Bitwise XOR | `(bxor 12 10)` → `6` |
| `(bnot A)` | Bitwise NOT | `(bnot 12)` → `-13` |
| `(bsl A N)` | Bit Shift Left | `(bsl 5 2)` → `20` |
| `(bsr A N)` | Bit Shift Right | `(bsr 20 2)` → `5` |

These operators work on integers of arbitrary size, limited only by available memory—though in practice, very large integers become unwieldy in much the same way that very large towels become difficult to fold.
