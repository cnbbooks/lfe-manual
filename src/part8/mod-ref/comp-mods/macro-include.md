# lfe_macro_include.erl - Include File Processor

**Purpose**: Handle `include-file` and `include-lib` macro forms.

**Location**: `src/lfe_macro_include.erl`
**Size**: 502 LOC, 18KB

**Module Classification**: Compiler support, macro system

#### Public API

```erlang
file(Name, St) -> {ok, Forms, State}
                | {error, Error, State}
```

Include a file by name. Located at `lfe_macro_include.erl:45`.

```erlang
lib(Name, St) -> {ok, Forms, State}
               | {error, Error, State}
```

Include a library file. Located at `lfe_macro_include.erl:55`.

#### Include Forms

**Include File**:

```lisp
(include-file "common-macros.lfe")
```

Searches:

1. Current directory
2. Directories in `-I` include path option

**Include Lib**:

```lisp
(include-lib "lfe/include/clj.lfe")
```

Searches:

1. Erlang lib directories (`code:lib_dir/1`)
2. Standard OTP application paths

#### File Resolution

**Include File Resolution** (`lfe_macro_include.erl:115-140`):

```erlang
resolve_include(File, IncludePath) ->
    % Try each directory in include path
    case try_paths(["."|IncludePath], File) of
        {ok, Path} -> Path;
        error -> {error, {include_file, File}}
    end.
```

**Include Lib Resolution** (`lfe_macro_include.erl:142-178`):

```erlang
resolve_include_lib("app/include/file.lfe") ->
    case code:lib_dir(app) of
        {error, bad_name} ->
            {error, {include_lib, "app not found"}};
        Dir ->
            Path = filename:join([Dir, "include", "file.lfe"]),
            check_file(Path)
    end.
```

#### File Reading and Parsing

**Process** (`lfe_macro_include.erl:89-113`):

1. Resolve file path
2. Read file contents
3. Parse into tokens (`lfe_scan:string/1`)
4. Parse tokens into forms (`lfe_parse:forms/1`)
5. Return forms for expansion

#### Dependencies

- `lfe_scan` - Lexical analysis
- `lfe_parse` - Parsing
- `lfe_io` - File I/O
- `filename`, `code` (Erlang) - Path resolution

#### Used By

- `lfe_macro` - Expands `include-file` and `include-lib` forms

#### Special Considerations

**Recursive Includes**: Supported (include files can include other files)

**Include Guards**: Not built-in; user must avoid circular includes manually

**Compile-Time Evaluation**: Included forms are expanded at compile time, so changes to included files require recompilation

**Performance**: Large include files slow compilation; prefer modular design
