# lfe_macro_export.erl - Export Macro Processor

**Purpose**: Handle macro exports between modules in the same file.

**Location**: `src/lfe_macro_export.erl`
**Size**: 288 LOC, 8.8KB

**Module Classification**: Compiler support, macro system

#### Public API

```erlang
export(Modules, Env, State) -> {ok, Env, State}
                               | {error, Errors, Warnings, State}
```

Process macro exports across modules. Located at `lfe_macro_export.erl:42-47`.

#### Export Mechanism

**Syntax**:

```lisp
(defmodule foo
  (export-macro when-test my-cond))

(defmodule bar
  (import-macro foo when-test))
```

**Process**:

1. Collect all `export-macro` declarations
2. Collect all `import-macro` declarations
3. For each import, copy macro binding from exporting module to importing module
4. Validate: exported macros must exist, no circular dependencies

**Implementation** (`lfe_macro_export.erl:67-158`):

```erlang
collect_exports(Modules) ->
    % Build map: ModuleName → ExportedMacros
    ...

collect_imports(Modules) ->
    % Build map: ModuleName → ImportedMacros
    ...

validate_imports(Imports, Exports) ->
    % Check all imports have corresponding exports
    ...

merge_macros(FromEnv, ToEnv, MacroNames) ->
    % Copy macro bindings between environments
    ...
```

#### Dependencies

- `lfe_env` - Environment operations
- `lfe_lib` - Utilities
- `lists`, `orddict`

#### Used By

- `lfe_comp` - Called during compilation if multi-module file

#### Special Considerations

**Scope**: Only works within a single file. Macros cannot be exported across file boundaries (by design - macros are compile-time only).

**Order Independence**: Modules can import macros from modules defined later in the same file.
