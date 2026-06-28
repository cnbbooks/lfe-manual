# lfe_bits.erl - Bitstring Specification

**Purpose**: Parse and validate bitstring segment specifications.

**Location**: `src/lfe_bits.erl`
**Size**: 137 LOC, 5.0KB

**Module Classification**: Library support, binary handling

#### Public API

```erlang
get_bitspecs(Specs) -> {Type, Size, Unit, Sign, Endian}
```

Parse bitstring specifications. Located at `lfe_bits.erl:37-41`.

#### Specification Format

**Segment Specs** (from binary syntax):

```lisp
#B(value)                    ; Default: 8-bit unsigned int
#B((value integer))          ; Explicit type
#B((value (size 16)))        ; 16-bit
#B((value integer (size 16) big unsigned))  ; Full spec
```

**Types**:

- `integer` - Integer value
- `float` - Floating-point
- `binary` - Binary/bitstring
- `bytes` - Alias for binary
- `bitstring` - Bitstring
- `utf8`, `utf16`, `utf32` - UTF encoding

**Modifiers**:

- `(size N)` - Bit size
- `(unit N)` - Unit size (for binary/bitstring)
- `signed`, `unsigned` - Sign
- `big`, `little`, `native` - Endianness

#### Parsing

**Spec Parser** (`parse_spec/1` at lines 78-134):

```erlang
parse_spec([integer|Rest]) ->
    parse_spec(Rest, {integer, default, 1, unsigned, big});
parse_spec([float|Rest]) ->
    parse_spec(Rest, {float, 64, 1, unsigned, big});
parse_spec([(size, N)|Rest], {T, _, U, S, E}) ->
    parse_spec(Rest, {T, N, U, S, E});
parse_spec([signed|Rest], {T, Sz, U, _, E}) ->
    parse_spec(Rest, {T, Sz, U, signed, E});
...
```

**Defaults**:

| Type | Size | Unit | Sign | Endian |
|------|------|------|------|--------|
| integer | 8 | 1 | unsigned | big |
| float | 64 | 1 | unsigned | big |
| binary | all | 8 | - | - |
| bitstring | all | 1 | - | - |
| utf8/16/32 | - | - | - | - |

#### Dependencies

- `lists` (Erlang stdlib)

#### Used By

- `lfe_eval_bits` - Runtime binary operations
- `lfe_translate` - Compile-time binary translation

#### Special Considerations

**Validation**: Ensures specs are valid (e.g., float must be 32 or 64 bits).

**Error Reporting**: Returns descriptive errors for invalid specs.
