# lfe_codegen.erl - Code Generator

**Purpose**: Generate Erlang Abstract Format from validated LFE forms. Coordinates lambda lifting and translation to Core Erlang.

**Location**: `src/lfe_codegen.erl`
**Size**: 499 LOC, 18KB

**Module Classification**: Compiler backend, orchestration

#### Public API

```erlang
forms(Forms, CompilerInfo) ->
    {ok, ErlangAST, State}
  | {error, Errors, Warnings, State}
```

Generate Erlang AST from LFE forms. Located at `lfe_codegen.erl:82-89`.

#### Code Generation Pipeline

**Process** (`comp_forms/2` at lines 155-235):

1. **Collect Module Definitions** (`collect_mod_defs/2`):
   - Extract module name, exports, imports
   - Collect attributes, records, types, specs
   - Store function definitions

2. **Build Struct Definition** (`build_struct_def/1`):
   - If struct defined, generate `__struct__/0` and `__struct__/1`
   - Auto-export struct functions

3. **Build Info Function** (`build_info_func/1`):
   - Generate `__info__/1` function
   - Returns: module, functions, macros, attributes, etc.
   - Auto-export `__info__/1`

4. **Compile Attributes** (`compile_attributes/1`):
   - Module name, exports, imports
   - Custom attributes
   - Type definitions, specs
   - Records

5. **Compile Functions** (`compile_functions/1`):
   - Lambda lift each function (`lfe_codelift`)
   - Translate to Erlang AST (`lfe_translate`)
   - Build function clauses

#### State Record

```erlang
-record(lfe_cg, {
    module=[],           % Module name
    mline=0,            % Module line
    exports=ordsets,     % Exported functions
    imports=orddict,     % Imported functions
    aliases=orddict,     % Module aliases
    onload=[],          % On-load function
    records=[],         % Records
    struct=undefined,   % Struct definition
    attrs=[],           % Attributes
    metas=[],           % Type/spec metadata
    funcs=orddict,      % Function definitions
    opts=[],            % Compiler options
    file=[],            % Source file
    func=[],            % Current function
    errors=[],
    warnings=[]
}).
```

#### Generated Functions

**Struct Functions** (`build_struct_def/1` at lines 394-434):

If module defines a struct, two functions are generated:

```erlang
__struct__() ->
    #{__struct__ => ModuleName, field1 => undefined, ...}.

__struct__(Fields) ->
    maps:merge(__struct__(), maps:from_list(Fields)).
```

**Info Function** (`build_info_func/1` at lines 436-492):

```erlang
__info__(module) -> ModuleName;
__info__(functions) -> [{func,arity}, ...];
__info__(macros) -> [];  % Always empty (macros are compile-time)
__info__(attributes) -> [{attr, value}, ...];
...
```

#### Erlang AST Format

The generated AST uses Erlang's abstract format:

**Module Attribute**:

```erlang
{attribute, Line, module, ModuleName}
```

**Export Attribute**:

```erlang
{attribute, Line, export, [{FuncName, Arity}, ...]}
```

**Function Definition**:

```erlang
{function, Line, Name, Arity, Clauses}
```

**Clause**:

```erlang
{clause, Line, Patterns, Guards, Body}
```

#### Dependencies

**LFE modules**:

- `lfe_codelift` - Lambda lifting
- `lfe_translate` - Core Erlang translation
- `lfe_internal` - Validation
- `lfe_lib` - Utilities

**Erlang compiler**:

- `compile:forms/2` - Final compilation

#### Used By

- `lfe_comp` - Compilation pipeline

#### Key Algorithms

**Lambda Lifting** (via `lfe_codelift`):

Nested lambdas are hoisted to module level:

```lisp
(defun outer (x)
  (lambda (y) (+ x y)))

; After lifting:
; outer/1: calls lifted lambda
; -lfe-outer-lambda-1/2: the lifted function taking x and y
```

**Import Translation** (`build_imports/1` at lines 321-356):

LFE import forms are transformed to Erlang imports:

```lisp
(import (from lists map filter))
→ -import(lists, [map/2, filter/2]).

(import (rename lists (reverse rev)))
→ Stored in context, calls to rev/1 translate to lists:reverse/1
```

**Record Translation** (`compile_records/1` at lines 289-319):

```lisp
(define-record person name age)
→ -record(person, {name, age}).
```

#### Special Considerations

**Auto-Exports**:

These functions are automatically exported:

- `__info__/1` (always)
- `__struct__/0`, `__struct__/1` (if struct defined)

**Compile-Time vs Runtime**:

- Macros are **not** included in `__info__/1` (they're compile-time only)
- Functions include metadata from specs

**OTP Compatibility**:

Generated code is standard Erlang AST, fully compatible with OTP tools.
