# lfe_eval_bits.erl - Binary Evaluation

**Purpose**: Evaluate binary construction and pattern matching at runtime.

**Location**: `src/lfe_eval_bits.erl`
**Size**: 318 LOC, 11KB

**Module Classification**: Runtime support, specialized evaluator

#### Public API

```erlang
eval_binary(Segments, Env) -> Binary
```

Evaluate binary construction. Located at `lfe_eval_bits.erl:35-38`.

```erlang
match_binary(Segments, Binary, Env) -> {yes, Bindings} | no
```

Match binary pattern. Located at `lfe_eval_bits.erl:42-45`.

#### Binary Segment Types

**Segment Syntax**:

```lisp
[binary [Value Spec1 Spec2 ...]]

Specs:
  size N           ; Bit size
  unit N           ; Unit size (default 1)
  integer          ; Integer value
  float            ; Float value
  binary           ; Binary value
  bytes            ; Alias for binary
  bitstring        ; Bitstring value
  utf8, utf16, utf32  ; UTF encoding
  signed, unsigned ; Sign (default unsigned)
  big, little, native  ; Endianness (default big)
```

**Example**:

```lisp
#B(42)                    ; Single byte
#B(42 (f 32))             ; 32-bit float
#B((s "hello"))           ; Binary string
#B((u16 65))              ; UTF-16 character
#B((x integer (size 16))) ; 16-bit integer
```

#### Binary Construction

**Process** (`eval_binary/2` at lines 52-121):

1. Evaluate each segment value
2. Apply segment type/size specifications
3. Construct binary from segments

```erlang
eval_segment([Value|Specs], Env) ->
    Val = lfe_eval:expr(Value, Env),
    Type = get_spec_type(Specs),
    Size = get_spec_size(Specs, Env),
    Unit = get_spec_unit(Specs),
    Sign = get_spec_sign(Specs),
    Endian = get_spec_endian(Specs),
    % Build binary segment
    <<(convert_value(Val, Type)):Size/unit:Unit-Sign-Endian>>.
```

#### Binary Matching

**Process** (`match_binary/3` at lines 129-248):

1. Iterate through segments
2. Match each segment against binary
3. Accumulate bindings

```erlang
match_segment([Pat|Specs], Binary, Offset, Env) ->
    Type = get_spec_type(Specs),
    Size = get_spec_size(Specs, Env),
    Unit = get_spec_unit(Specs),
    Sign = get_spec_sign(Specs),
    Endian = get_spec_endian(Specs),
    % Extract value from binary
    <<_:Offset, Value:Size/unit:Unit-Sign-Endian, _/binary>> = Binary,
    % Match pattern against value
    case lfe_eval:match(Pat, Value, Env) of
        {yes, Bindings} -> {yes, Bindings, Offset+Size*Unit};
        no -> no
    end.
```

#### UTF Encoding

**UTF-8/16/32 Support** (`eval_utf_segment/2` at lines 156-189):

```erlang
eval_utf8(Char) ->
    unicode:characters_to_binary([Char], unicode, utf8).

eval_utf16(Char, Endian) ->
    unicode:characters_to_binary([Char], unicode, {utf16, Endian}).

eval_utf32(Char, Endian) ->
    unicode:characters_to_binary([Char], unicode, {utf32, Endian}).
```

**Example**:

```lisp
#B((u8 65))        ; UTF-8 'A'
#B((u16 65 big))   ; UTF-16 big-endian 'A'
#B((u32 65 little)); UTF-32 little-endian 'A'
```

#### Spec Defaults

**Default Specifications** (`lfe_eval_bits.erl:267-301`):

- **Type**: `integer`
- **Size**: 8 bits (for integer), full value (for binary/bitstring)
- **Unit**: 1
- **Sign**: `unsigned`
- **Endian**: `big`

#### Dependencies

**LFE modules**:

- `lfe_eval` - Expression evaluation, pattern matching
- `lfe_bits` - Bitstring specification parsing

**Erlang stdlib**:

- `unicode` - UTF encoding/decoding

#### Used By

- `lfe_eval` - Delegated binary operations

#### Special Considerations

**Performance**:

Binary operations are relatively fast even in interpreted mode (use Erlang's native binary operations).

**Size Expressions**:

Sizes can be expressions:

```lisp
(let ([n 16])
  #B((x integer (size n))))  ; Size evaluated at runtime
```

**Alignment**:

Bitstrings need not be byte-aligned:

```lisp
#B((x integer (size 3))      ; 3 bits
   (y integer (size 5)))     ; 5 bits
; Total: 8 bits (1 byte)
```

**String Shorthand**:

```lisp
#"hello"  ; Shorthand for: #B((s "hello"))
```
