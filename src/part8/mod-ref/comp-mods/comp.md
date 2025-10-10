# lfe_comp.erl - Compilation Orchestrator

**Purpose**: Coordinate all compilation passes and manage the overall compilation pipeline.

**Location**: `src/lfe_comp.erl`
**Size**: 711 LOC, 27KB

**Module Classification**: Compiler core, orchestration layer

#### Public API

**Primary Entry Points**:

```erlang
file(FileName) -> CompRet
file(FileName, Options) -> CompRet
    CompRet = {ok, ModuleName, Binary, Warnings}
            | {ok, ModuleName, Warnings}
            | error | {error, Errors, Warnings}
```

Entry point for compiling LFE files. Located at `lfe_comp.erl:100-114`.

```erlang
forms(Forms) -> CompRet
forms(Forms, Options) -> CompRet
```

Compile pre-parsed forms directly. Located at `lfe_comp.erl:116-129`.

**Specialized Compilation**:

```erlang
is_lfe_file(FileName) -> boolean()
```

Check if file has `.lfe` extension. Located at `lfe_comp.erl:132`.

```erlang
format_error(Error) -> Chars
```

Format compiler errors for display. Located at `lfe_comp.erl:175-178`.

#### Compilation Pipeline

The `do_forms/1` function at `lfe_comp.erl:236-267` orchestrates seven distinct passes:

1. **File Splitting** (`do_split_file/1` at line 298): Separate multi-module files
2. **Export Macros** (`do_export_macros/1` at line 346): Process macro exports
3. **Macro Expansion** (`do_expand_macros/1` at line 369): Full macro expansion
4. **Linting** (`do_lfe_lint/1` at line 398): Semantic analysis
5. **Documentation** (`do_get_docs/1` at line 412): Extract EEP-48 docs
6. **Code Generation** (`do_lfe_codegen/1` at line 426): Generate Erlang AST
7. **Erlang Compilation** (`do_erl_comp/1` at line 458): Call Erlang compiler

Each pass can be stopped early via options: `to_split`, `to_expand`, `to_lint`, `to_core0`, `to_core`, `to_kernel`, `to_asm`.

#### Internal Structure

**Key Records** (defined in `lfe_comp.hrl`):

```erlang
-record(comp, {
    base="",         % Base filename
    ldir=".",        % LFE source directory
    lfile="",        % LFE filename
    odir=".",        % Output directory
    opts=[],         % Compiler options
    ipath=[],        % Include path
    cinfo=none,      % Compiler info (#cinfo{})
    module=[],       % Module name
    code=[],         % Generated code
    return=[],       % Return mode
    errors=[],       % Accumulated errors
    warnings=[],     % Accumulated warnings
    extra=[]         % Pass-specific options
}).
```

**File Splitting Algorithm** (`do_split_file/1` at lines 298-318):

The file splitter handles LFE's unique feature of allowing multiple modules per file:

1. Top-level macros are expanded to identify `define-module` forms
2. Forms before the first module become "pre-forms" available to all modules
3. Each module receives: `PreForms ++ ModuleForms`
4. A `FILE` macro is automatically injected with the source file path

#### Dependencies

**Direct Dependencies**:

- `lfe_io` (7 calls) - Error/warning reporting
- `lfe_macro` (4 calls) - Macro expansion
- `lfe_lib` (3 calls) - Utility functions
- `lfe_env` (2 calls) - Environment management
- `lfe_macro_export` (1 call) - Export macro processing
- `lfe_lint` (1 call) - Semantic analysis
- `lfe_docs` (1 call) - Documentation generation
- `lfe_codegen` (1 call) - Code generation

**Erlang Dependencies**:

- `compile` (Erlang compiler) - Final BEAM generation
- `lists`, `ordsets`, `orddict` - Data structures

#### Used By

- `lfec` (compiler script)
- `lfe_shell` (for `:c` command)
- `rebar3` hooks
- Any tool compiling LFE files

#### Key Algorithms

**Option Processing** (`lfe_comp.erl:180-217`):

```erlang
% Options can be:
% - Atoms: verbose, report, return, binary
% - Tuples: {outdir, Dir}, {i, IncludeDir}
% - Stop flags: to_expand, to_lint, to_core, etc.
```

**Error Aggregation**:

Errors and warnings are accumulated through all passes in the `#comp.errors` and `#comp.warnings` lists, then formatted and returned together.

#### Special Considerations

- **Multi-module files**: LFE allows multiple modules in one file, unlike Erlang
- **Pre-forms**: Forms before the first module are shared across all modules in the file
- **FILE macro**: Automatically defined to the source file path for each module
- **Incremental compilation**: Not currently supported; each compilation is full recompilation
- **Pass control**: Compilation can stop at intermediate stages for debugging

**Performance Note**: The compiler is dominated by macro expansion (30-40%) and Erlang compilation (40-50%) time.
