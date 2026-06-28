# lfe_macro.erl - Macro Expansion Engine

**Purpose**: Expand macros into core LFE forms. This is the largest and most complex module in the compiler pipeline.

**Location**: `src/lfe_macro.erl`
**Size**: 1,432 LOC, 55KB

**Module Classification**: Compiler core, transformation layer

#### Public API

**Form Expansion**:

```erlang
expand_form(Form, Line, Env, State) ->
    {ok, Form, Env, State}
  | {error, Errors, Warnings, State}
```

Expand a single form. Located at `lfe_macro.erl:131-139`.

**File Form Expansion**:

```erlang
expand_fileform({Form, Line}, Env, State) ->
    {ok, {Form, Line}, Env, State}
  | {error, Errors, Warnings, State}
expand_fileforms(FileForms, Env, State) -> ...
```

Expand forms with line number annotations. Located at `lfe_macro.erl:141-151`.

**Macro State Management**:

```erlang
new() -> State
format_error(Error) -> Chars
```

Create new macro state and format errors. Located at `lfe_macro.erl:114-117`.

#### Macro State

**State Record** (defined in `lfe_macro.hrl`):

```erlang
-record(mac, {
    deep=true,                % Deep recursive expansion
    keep=true,                % Keep all forms (even unexpanded)
    module='-no-module-',     % Current module name
    line=1,                   % Current line number
    vc=0,                     % Variable counter (for gensym)
    fc=0,                     % Function counter (for gensym)
    file=[],                  % Source file name
    opts=[],                  % Compiler options
    ipath=[],                 % Include path
    errors=[],                % Accumulated errors
    warnings=[],              % Accumulated warnings
    unloadable=[]             % Unloadable macro modules
}).
```

#### Macro Types

**1. User-Defined Macros** (via `define-macro`):

```lisp
(define-macro when-positive (x body)
  `(if (> ,x 0) ,body 'undefined))

; Expands to macro function:
; (lambda (x body $ENV) ...)
```

Macros receive arguments + `$ENV` parameter for environment access.

**2. Pattern-Based Macros** (via `match-lambda`):

```lisp
(define-macro foo
  ([a b] `(list ,a ,b))
  ([a b c] `(tuple ,a ,b ,c)))
```

**3. Built-in Macros** (50+ macros):

Located at `lfe_macro.erl:700-1340`:

**Convenience Macros**:

- `c*r` family: `caar`, `cadr`, `cdar`, `cddr`, etc. (lines 1118-1184)
- Comparison operators: `!=`, `===`, `!==`, `/=` (lines 885-900)
- `list*` - List construction with tail (lines 908-917)
- `let*`, `flet*`, `fletrec*` - Sequential bindings (lines 927-1000)

**Common Lisp Style**:

- `defmodule`, `defun`, `defmacro` - CL-style definitions (lines 779-835)
- `defsyntax` - Syntax macro definition (line 837)

**Records and Structs**:

- `defrecord` - Record definition (calls `lfe_macro_record`, line 843)
- `defstruct` - Struct definition (calls `lfe_macro_struct`, line 849)

**Control Flow**:

- `do` loops - Iteration macro (lines 1024-1090)
- `fun` shortcuts - Lambda syntax sugar (lines 1095-1111)

**Match Specifications**:

- `ets-ms`, `trace-ms` - DSL for match specs (calls `lfe_ms`, lines 1186-1198)

**Query List Comprehensions**:

- `qlc` - QLC syntax (calls `lfe_qlc`, lines 1202-1221)

**Module Information**:

- `MODULE` - Current module name (line 1224)
- `LINE` - Current line number (line 1225)
- `FILE` - Current file path (line 1226)

**Call Syntax**:

- `:module:function` → `(call 'module 'function ...)` (lines 1230-1268)

#### Expansion Algorithm

**Main Expansion Loop** (`pass_form/3` at lines 180-213):

```erlang
pass_form([progn | Forms], Env, St) ->
    % Expand all forms in progn
    expand_forms(Forms, Env, St);

pass_form([eval-when-compile | Forms], Env, St) ->
    % Evaluate forms at compile time
    eval_forms(Forms, Env),
    {ok, [progn], Env, St};

pass_form(['include-file', File], Env, St) ->
    % Load and expand file contents
    include_file(File, Env, St);

pass_form(['define-macro', Name, Meta, Def], Env, St) ->
    % Add macro to environment
    add_macro(Name, Def, Env, St);

pass_form(Form, Env, St) ->
    % Expand form recursively
    expand_expr(Form, Env, St).
```

**Backquote Expansion** (`exp_backquote/2` at lines 1343-1408):

Implements R6RS-compliant quasiquotation:

```lisp
`(a ,b ,@c)
→ (list 'a b | c)

`(a b ,(+ 1 2))
→ (list 'a 'b 3)

`#(a ,b)
→ (tuple 'a b)
```

**Key feature**: Nested backquotes are handled correctly.

**Macro Application** (`exp_macro/4` at lines 574-641):

When a macro is encountered:

1. Look up macro definition in environment
2. Apply macro function to arguments + `$ENV`
3. Recursively expand the result
4. Track expansion depth to prevent infinite loops

**Environment Threading**:

The environment (`lfe_env`) is threaded through expansion:

```erlang
{ok, Form1, Env1, St1} = expand_form(Form0, Env0, St0),
{ok, Form2, Env2, St2} = expand_form(Next0, Env1, St1),
...
```

#### Special Forms (Not Expanded)

Core forms are preserved and passed through:

- **Data**: `quote`, `cons`, `car`, `cdr`, `list`, `tuple`, `binary`, `map`
- **Functions**: `lambda`, `match-lambda`, `let`, `let-function`, `letrec-function`
- **Control**: `progn`, `if`, `case`, `receive`, `catch`, `try`
- **Calls**: `function`, `call`, `funcall`

Located at `lfe_macro.erl:216-268`.

#### Dependencies

**LFE modules**:

- `lfe_env` - **Imported** (39 functions imported! - lines 79-87)
- `lfe_io` - I/O operations
- `lfe_lib` - Utilities
- `lfe_internal` - Form validation
- `lfe_macro_record` - Record macro generation
- `lfe_macro_struct` - Struct macro generation
- `lfe_macro_include` - File inclusion
- `lfe_eval` - For `eval-when-compile`

**Erlang stdlib**:

- `lists`, `ordsets`, `orddict`

#### Used By

- `lfe_comp` - Compilation pipeline
- `lfe_shell` - REPL macro expansion
- `lfe_eval` - Runtime macro expansion (for macros in interpreted code)

#### Key Algorithms

**Macro Hygiene** (or lack thereof):

LFE macros are **unhygienic** - they can capture variables from the calling context:

```lisp
(define-macro bad-swap (a b)
  `(let ((tmp ,a))
     (set ,a ,b)
     (set ,b tmp)))

; If caller has variable 'tmp', it will be captured!
```

**Solution**: Manual gensym using environment's variable counter:

```erlang
gen_variable(St) ->
    {erlang:list_to_atom("_G" ++ integer_to_list(St#mac.vc)),
     St#mac{vc = St#mac.vc + 1}}.
```

**Macro Recursion Detection** (`lfe_macro.erl:574-641`):

The expander tracks expansion depth and detects cycles:

```erlang
-define(MAX_EXPAND, 1000).

expand_with_depth(Form, Depth, Env, St) when Depth > ?MAX_EXPAND ->
    {error, "Macro expansion depth exceeded", St};
expand_with_depth(Form, Depth, Env, St) ->
    ...
```

**File Inclusion** (`lfe_macro_include.erl`):

The `include-file` and `include-lib` forms:

1. Resolve file path (absolute or relative to include path)
2. Read and parse file
3. Expand all forms from file
4. Insert into current expansion

**Eval-When-Compile**:

Forms in `eval-when-compile` are evaluated using `lfe_eval`:

```lisp
(eval-when-compile
  (defun helper () 'compiled)
  (io:format "Compiling!~n"))
```

The side effects occur at compile time, but forms are also included in output.

#### Special Considerations

**Performance Hotspot**:

Macro expansion is 30-40% of compilation time. Deeply nested macros or heavy quasiquotation can slow compilation significantly.

**Environment Import**:

`lfe_macro` is the **only** module that imports from `lfe_env`:

```erlang
-import(lfe_env, [new/0, add_vbinding/3, is_vbound/2, ...]).
```

This tight coupling reflects the heavy use of environment operations.

**Error Messages**:

Macro expansion errors can be cryptic because:

1. Original source location is lost after expansion
2. Generated code may not correspond to source
3. Nested macro expansions obscure the error source

**Macro Export** (`lfe_macro_export.erl`):

Macros can be exported between modules in the same file:

```lisp
(defmodule foo
  (export-macro when-positive))

(defmodule bar
  (import-macro foo when-positive))
```

**Record and Struct Macros**:

These delegate to specialized modules:

- `lfe_macro_record` - Generates 9+ macros per record
- `lfe_macro_struct` - Generates struct access macros
