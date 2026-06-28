# Stage 6: Code Generation (lfe_codegen.erl)

**Purpose**: Convert validated LFE forms to Erlang Abstract Format structure.

**Module**: `lfe_codegen.erl` (499 LOC) - Location: `src/lfe_codegen.erl`

**Code generation state**:

```erlang
-record(lfe_cg, {
    module = [],           % Module name
    mline = 0,            % Module definition line
    exports = ordsets:new(), % Exports
    imports = orddict:new(), % Imports
    aliases = orddict:new(), % Aliases
    onload = [],          % Onload function
    records = [],         % Records
    struct = undefined,   % Struct definition
    attrs = [],           % Attributes
    metas = [],           % Meta data (types, specs)
    funcs = orddict:new(), % Defined functions
    opts = [],            % Compiler options
    file = [],            % File name
    func = [],            % Current function
    errors = [],
    warnings = []
}).
```

**Processing steps**:

**1. Collect Module Definitions** (`collect_mod_defs/2`):

- Extract module name, exports, imports
- Collect attributes, records, structs
- Collect type definitions and specs
- Store function definitions

**2. Build Struct Definition** (`build_struct_def/1`):

If struct defined, create `__struct__/0` and `__struct__/1` functions:

```erlang
__struct__() -> #{__struct__ => module_name, field1 => default1, ...}.
__struct__(Fields) -> maps:merge(__struct__(), maps:from_list(Fields)).
```

Auto-export these functions.

**3. Build Info Function** (`build_info_func/1`):

Create `__info__/1` function (like Elixir):

```erlang
__info__(module) -> module_name;
__info__(functions) -> [{func, arity}, ...];
__info__(macros) -> [];
__info__(attributes) -> [...];
...
```

Auto-exported.

**4. Compile Attributes** (`compile_attributes/1`):

Generate Erlang attributes:

- Module name: `{attribute, Line, module, Name}`
- Exports: `{attribute, Line, export, [{F, A}, ...]}`
- Imports: `{attribute, Line, import, {Mod, [{F, A}, ...]}}`
- on_load: `{attribute, Line, on_load, {F, A}}`
- Custom attributes
- Type declarations
- Specs
- Records

**5. Compile Functions** (`compile_functions/1`):

For each function:

1. **Lambda lift** (via `lfe_codelift`) - nested functions become module-level
2. **Translate** (via `lfe_translate`) - LFE → Erlang AST
3. **Generate clauses** - multiple clauses for match-lambda
