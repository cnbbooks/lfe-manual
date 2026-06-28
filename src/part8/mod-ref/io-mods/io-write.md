# lfe_io_write.erl - Compact Writer

**Purpose**: Write LFE terms in compact (non-pretty) format with depth limiting.

**Location**: `src/lfe_io_write.erl`
**Size**: 156 LOC, 5.3KB

**Module Classification**: I/O support, compact formatting

#### Public API

```erlang
term(Term) -> [char()]
term(Term, Depth) -> [char()]
```

Convert term to compact string representation. Located at `lfe_io_write.erl:36-38`.

#### Output Format

**Data Types**:

```lisp
; Atoms
foo          → "foo"
'foo-bar'    → "foo-bar"
'|complex|'  → "|complex|"

; Numbers
42           → "42"
3.14         → "3.14"

; Strings
"hello"      → "\"hello\""

; Lists
(1 2 3)      → "(1 2 3)"
'()          → "()"

; Improper lists
(a . b)      → "(a . b)"

; Tuples
#(a b c)     → "#(a b c)"

; Binaries
#"bytes"     → "#\"bytes\""

; Maps
#M(a 1 b 2)  → "#M(a 1 b 2)"
```

**Depth Limiting**:

```erlang
term([a, [b, [c, [d, [e]]]]], 3)
→ "(a (b (c ...)))"
```

The `...` indicates truncation at depth limit.

#### Implementation

**Main Dispatch** (`term/2` at lines 45-92):

```erlang
term(Atom, _D) when is_atom(Atom) ->
    write_atom(Atom);
term(Number, _D) when is_number(Number) ->
    write_number(Number);
term(List, D) when is_list(List) ->
    write_list(List, D);
term(Tuple, D) when is_tuple(Tuple) ->
    write_tuple(Tuple, D);
term(Binary, D) when is_binary(Binary) ->
    write_binary(Binary, D);
term(Map, D) when is_map(Map) ->
    write_map(Map, D).
```

**List Writing** (`write_list/2` at lines 95-123):

Handles:

- Empty lists: `()`
- Proper lists: `(a b c)`
- Improper lists: `(a . b)`
- Depth limiting

**Atom Quoting** (`write_atom/1` at lines 126-141):

Quotes atoms when necessary:

- Contains special characters
- Starts with uppercase
- Is a reserved word

#### Dependencies

- `lists`, `io_lib` (Erlang stdlib)

#### Used By

- `lfe_io` - Via `print1/1,2`
- `lfe_error` - Error formatting
- Internal modules - Debugging output

#### Special Considerations

**No Line Breaks**: Output is single-line (compact).

**Performance**: Optimized for speed over readability.

**Erlang Compatibility**: Output can be read by Erlang's `erl_scan` (mostly).
