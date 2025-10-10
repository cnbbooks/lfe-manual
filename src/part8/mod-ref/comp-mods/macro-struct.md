# lfe_macro_struct.erl - Struct Macro Generator

**Purpose**: Generate accessor macros for struct (map-based) definitions.

**Location**: `src/lfe_macro_struct.erl`
**Size**: 46 LOC, 1.5KB

**Module Classification**: Compiler support, code generation

#### Public API

```erlang
define(Name, FieldDefs, Env, Line) ->
    {[{Name, Macros}], Env}
```

Generate struct macros. Located at `lfe_macro_struct.erl:42`.

#### Struct vs Record

**Structs use maps** instead of tuples:

```lisp
(define-struct [name age city])

(struct person name "Alice" age 30)
→ #{__struct__ => person, name => "Alice", age => 30, city => undefined}
```

**Key Difference**: Structs include a `__struct__` key identifying the struct type (Elixir-inspired).

#### Generated Macros

**Fewer macros than records** (structs use runtime functions):

1. **Module functions** (not macros):
   - `__struct__/0` - Return default struct
   - `__struct__/1` - Create struct from proplist

2. **Accessor macros**:
   - `struct` - Constructor macro
   - `is-struct` - Type test macro
   - `struct-field` - Field access macro

#### Struct Runtime

Struct operations are defined in `lfe_struct.erl` (33 LOC):

```erlang
new(Mod) -> Mod:'__struct__'().
new(Mod, Fields) -> Mod:'__struct__'(Fields).
is(Struct, Mod) -> maps:get('__struct__', Struct) =:= Mod.
fetch(Struct, Mod, Field) -> maps:get(Field, Struct).
...
```

#### Dependencies

- `lfe_struct` (runtime)
- `maps` (Erlang)

#### Used By

- `lfe_macro` - Expands `defstruct` forms

#### Special Considerations

**Flexibility**: Maps are more flexible than tuples:

- Can add fields dynamically
- Pattern matching on maps
- Better introspection

**Performance**: Map access is slower than tuple element access

**OTP Compatibility**: Less compatible with OTP than records (some behaviors expect records)
