# lfe_lib.erl - Utility Functions

**Purpose**: General-purpose utility functions used across multiple modules.

**Location**: `src/lfe_lib.erl`
**Size**: 124 LOC, 4.5KB

**Module Classification**: Utilities, shared functions

#### Public API

**Type Predicates**:

```erlang
is_symb(Value) -> boolean()
is_symb_list(Value) -> boolean()
is_posint_list(Value) -> boolean()
is_proper_list(Value) -> boolean()
is_doc_string(Value) -> boolean()
```

Type checking predicates. Lines 32-54.

**Form Processing**:

```erlang
proc_forms(Fun, Forms, State) -> {Forms, State}
proc_forms(Fun, Forms, Line, State) -> {Forms, State}
```

Process nested `progn` forms. Lines 56-84.

**Name Parsing**:

```erlang
split_name(Name) -> [Module] | [Module, Function] | [Module, Function, Arity]
```

Parse qualified names like `'lists:map/2'`. Lines 105-123.

#### Form Processing

**Purpose**: Flatten nested `(progn ...)` forms while applying a function to each form.

**Example**:

```erlang
Forms = [{[progn,
           [progn, Form1, Form2],
           Form3], 1}],

Fun = fun(F, Line, State) ->
          {[process(F)], State}
      end,

{ProcessedForms, FinalState} = lfe_lib:proc_forms(Fun, Forms, State0)
```

**Use Case**: Compiler passes need to process all forms at any nesting level.

#### Dependencies

- `lists`, `string` (Erlang stdlib)

#### Used By

- `lfe_comp` - Form processing
- `lfe_macro` - Form processing
- `lfe_lint` - Form processing
- Other compiler modules

#### Special Considerations

**Shared Utility**: One of the most widely used utility modules.

**Simple Functions**: Small, focused, well-tested functions.
