# lfe_io_format.erl - Formatted Output

**Purpose**: Printf-style formatted output for LFE with support for LFE-specific format directives.

**Location**: `src/lfe_io_format.erl`
**Size**: 494 LOC, 17KB

**Module Classification**: I/O support, formatted output

#### Public API

```erlang
fwrite1(Format, Args) -> [char()]
```

Format string with arguments. Located at `lfe_io_format.erl:52-56`.

#### Format Directives

**From Erlang's `io:format`**:

```
~w  - Write term
~p  - Pretty print term
~s  - String
~c  - Character
~d  - Decimal integer
~f  - Float
~e  - Exponential float
~g  - General float
~b  - Integer in arbitrary base
~x  - Hexadecimal
~n  - Newline
~~  - Literal tilde
```

**LFE Extensions**:

```
~a  - Write atom (without quotes)
~l  - Write list (without parens)
```

**Modifiers**:

```
~Nw    - Width N
~N.Mf  - Width N, precision M
~-Nw   - Left-align in width N
~0Nd   - Pad with zeros
```

#### Examples

```lisp
(lfe_io:format "Hello ~s!~n" '("world"))
→ "Hello world!\n"

(lfe_io:format "Number: ~d, Float: ~.2f~n" '(42 3.14159))
→ "Number: 42, Float: 3.14\n"

(lfe_io:format "~w ~p~n" '((a b c) (x y z)))
→ "(a b c) (x y z)\n"

(lfe_io:format "Atom: ~a, List: ~l~n" '(foo (1 2 3)))
→ "Atom: foo, List: 1 2 3\n"
```

#### Implementation

**Parser** (`parse_format/1` at lines 89-156):

Parses format string into directive list:

```erlang
parse_format("~d ~s") →
    [{int, [], []}, {string, " "}, {string, [], []}]
```

**Formatter** (`format_directive/2` at lines 178-312):

Applies each directive:

```erlang
format_directive({int, Width, Prec}, [Arg|Args]) ->
    S = integer_to_list(Arg),
    Padded = pad_string(S, Width, Prec),
    {Padded, Args}.
```

**LFE-Specific** (`write_atom/1` at lines 387-401, `write_list/1` at lines 403-419):

Special handling for LFE atoms and lists.

#### Dependencies

- `lfe_io_write` - Term writing
- `lists`, `io_lib`, `string` (Erlang stdlib)

#### Used By

- `lfe_io` - Via `format1/2`
- LFE user code - Formatted output

#### Special Considerations

**Error Handling**: Throws `badarg` if format/args mismatch.

**Performance**: Parsing format string has overhead; cache results for repeated formats.

**Compatibility**: Most Erlang `~` directives work identically.
