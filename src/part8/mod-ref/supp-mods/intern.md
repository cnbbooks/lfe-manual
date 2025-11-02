# lfe_internal.erl - Type and Form Validation

**Purpose**: Centralized knowledge of core forms, BIFs, guards, and types.

**Location**: `src/lfe_internal.erl`
**Size**: 360 LOC, 14KB

**Module Classification**: Utilities, validation

#### Public API

**Form Classification**:

```erlang
is_core_form(Name) -> boolean()
is_core_func(Name, Arity) -> boolean()
is_guard_func(Name, Arity) -> boolean()
is_bif(Name, Arity) -> boolean()
is_erl_bif(Name, Arity) -> boolean()
is_lfe_bif(Name, Arity) -> boolean()
is_type(Name, Arity) -> boolean()
```

Check if name/arity is a known form/function/type. Lines 32-89.

#### Core Forms

**Special Forms** (`is_core_form/1` at lines 123-178):

```erlang
is_core_form(quote) -> true;
is_core_form(cons) -> true;
is_core_form(car) -> true;
is_core_form(cdr) -> true;
is_core_form(list) -> true;
is_core_form(tuple) -> true;
is_core_form(binary) -> true;
is_core_form(map) -> true;
is_core_form(lambda) -> true;
is_core_form('match-lambda') -> true;
is_core_form(let) -> true;
is_core_form('let-function') -> true;
is_core_form('letrec-function') -> true;
is_core_form('let-macro') -> true;
is_core_form(progn) -> true;
is_core_form(if) -> true;
is_core_form('case') -> true;
is_core_form(receive) -> true;
is_core_form(after) -> true;
is_core_form(call) -> true;
is_core_form(try) -> true;
is_core_form('catch') -> true;
is_core_form(funcall) -> true;
is_core_form(_) -> false.
```

#### Guard Functions

**Allowed in Guards** (`is_guard_func/2` at lines 201-267):

Type tests, comparisons, arithmetic, boolean operators, bitwise operators, etc.

```erlang
is_guard_func(is_atom, 1) -> true;
is_guard_func(is_list, 1) -> true;
is_guard_func('==', 2) -> true;
is_guard_func('=:=', 2) -> true;
is_guard_func('+', 2) -> true;
is_guard_func('and', 2) -> true;
is_guard_func('or', 2) -> true;
...
```

**Not Allowed**: User-defined functions, most BIFs.

#### BIF Lists

**Erlang BIFs** (`is_erl_bif/2` at lines 289-367):

Comprehensive list of Erlang's built-in functions.

**LFE BIFs** (`is_lfe_bif/2` at lines 369-399):

LFE-specific functions in `lfe` module.

#### Type Names

**Built-in Types** (`is_type/2` at lines 421-467):

```erlang
is_type(any, 0) -> true;
is_type(none, 0) -> true;
is_type(atom, 0) -> true;
is_type(integer, 0) -> true;
is_type(float, 0) -> true;
is_type(list, 0) -> true;
is_type(list, 1) -> true;  % list(T)
is_type(tuple, 0) -> true;
is_type(map, 0) -> true;
...
```

#### Dependencies

- None (self-contained)

#### Used By

- `lfe_eval` - Validation
- `lfe_translate` - Validation
- `lfe_lint` - Validation
- `lfe_macro` - Form checking

#### Special Considerations

**Central Authority**: Single source of truth for "what's valid in LFE".

**Frequently Updated**: When adding language features, this module must be updated.

**Performance**: Functions are called frequently; must be fast (usually constant-time lookups).
