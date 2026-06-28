# lfe_struct.erl - Struct Runtime Support

**Purpose**: Runtime support functions for struct operations (map-based records).

**Location**: `src/lfe_struct.erl`
**Size**: 33 LOC, 988B

**Module Classification**: Library, runtime support

This is the **smallest module** in the codebase.

#### Public API

```erlang
new(Module) -> Struct
new(Module, Fields) -> Struct
is(Struct, Module) -> boolean()
fetch(Struct, Module, Field) -> Value
```

Struct operations. Located at `lfe_struct.erl:25-33`.

#### Struct Format

**Structs are maps** with `__struct__` key:

```erlang
#{__struct__ => ModuleName,
  field1 => Value1,
  field2 => Value2}
```

#### Operations

**Create**:

```erlang
Struct = lfe_struct:new(person, [{name, "Alice"}, {age, 30}])
% → #{__struct__ => person, name => "Alice", age => 30}
```

**Type Check**:

```erlang
lfe_struct:is(Struct, person)  % → true
lfe_struct:is(Struct, other)   % → false
```

**Field Access**:

```erlang
lfe_struct:fetch(Struct, person, name)  % → "Alice"
```

#### Dependencies

- `maps` (Erlang stdlib)

#### Used By

- `lfe_macro_struct` - Macro expansion
- `lfe_eval` - Struct evaluation
- User code - Struct operations

#### Special Considerations

**Elixir-Inspired**: Borrows from Elixir's struct concept.

**Map-Based**: More flexible than records (tuples).

**Type Tagged**: `__struct__` key identifies struct type.
