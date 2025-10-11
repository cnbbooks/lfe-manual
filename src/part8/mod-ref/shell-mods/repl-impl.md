# lfe_shell.erl - REPL Implementation

**Purpose**: Interactive Read-Eval-Print Loop with environment management, command execution, and module slurping.

**Location**: `src/lfe_shell.erl`
**Size**: 1,188 LOC, 41KB

**Module Classification**: Shell core, interactive environment

This is the **5th largest module** in the codebase and provides the primary user interface to LFE.

#### Public API

**Server Mode**:

```erlang
start() -> Pid
start(State) -> Pid
server() -> no_return()
server(State) -> no_return()
```

Start shell server process. Located at `lfe_shell.erl:110-131`.

**Script Execution**:

```erlang
run_script(File, Args) -> no_return()
run_script(File, Args, State) -> no_return()
run_strings(Strings) -> {Value, State}
run_strings(Strings, State) -> {Value, State}
```

Execute scripts or string expressions. Lines 66-105.

**State Management**:

```erlang
new_state(ScriptName, Args) -> State
new_state(ScriptName, Args, Env) -> State
upd_state(ScriptName, Args, State) -> State
```

Create and update shell state. Lines 279-300.

#### Shell State

**State Record** (line 63-64):

```erlang
-record(state, {
    curr,      % Current environment
    save,      % Saved environment (before slurp)
    base,      % Base environment (predefined)
    slurp=false % Are we in slurped state?
}).
```

**Three Environments**:

1. **Base**: Predefined shell functions, macros, variables (never changes)
2. **Current**: Active environment with user definitions
3. **Save**: Snapshot before `(slurp ...)` for `(unslurp)`

#### Shell Commands

**Built-in Functions** (added at lines 322-359):

```lisp
(c file)           ; Compile and load
(cd dir)           ; Change directory
(ec file)          ; Compile Erlang file
(ep expr)          ; Print in Erlang format
(epp expr)         ; Pretty print in Erlang format
(help)             ; Show help (alias: h)
(h mod)            ; Show module documentation
(h mod func)       ; Show function documentation
(h mod func arity) ; Show specific arity documentation
(i)                ; System information
(l modules)        ; Load modules
(ls dir)           ; List directory
(clear)            ; Clear screen
(m)                ; List loaded modules
(m mod)            ; Module information
(memory)           ; Memory stats
(p expr)           ; Print expression
(pp expr)          ; Pretty print expression
(pwd)              ; Print working directory
(q)                ; Quit (alias: exit)
(flush)            ; Flush messages
(regs)             ; Registered processes
(nregs)            ; Named registered processes
(uptime)           ; Node uptime
```

**Built-in Forms** (added at lines 365-389):

```lisp
(set pattern expr)                      ; Pattern match and bind
(set pattern (when guard) expr)         ; With guard
(slurp file)                            ; Load file into shell
(unslurp)                               ; Revert slurp
(run file)                              ; Execute shell script
(reset-environment)                     ; Reset to base environment
```

**Built-in Macros**:

- `(c ...)` - Compile with arguments
- `(doc mod)`, `(doc mod func)`, `(doc mod func arity)` - Documentation
- `(l ...)`, `(ls ...)`, `(m ...)` - Variadic list handling

#### Shell Variables

**Expression History** (lines 302-320):

```lisp
+    ; Last expression entered
++   ; Second-to-last expression
+++  ; Third-to-last expression

*    ; Last value
**   ; Second-to-last value
***  ; Third-to-last value

-    ; Current expression (being evaluated)

$ENV ; Current environment
```

Updated after each evaluation via `update_shell_vars/3`.

#### Evaluation Architecture

**Two-Process Design** (`server_loop/2` and `eval_loop/2` at lines 133-440):

```
Shell Process              Evaluator Process
-------------              -----------------
Read input
  ↓
{eval_expr, Form} -------→ Receive form
                           Evaluate
                           ↓
{eval_value, Val, St} ←--- Send result
  ↓
Display value
Update state
```

**Why Two Processes?**

1. **Isolation**: Evaluator crashes don't kill shell
2. **Interruption**: Can kill evaluator (Ctrl-C) without losing shell state
3. **Timeout**: Can timeout long-running evaluations

**Error Handling** (`shell_eval/3` at lines 150-167):

```erlang
shell_eval(Form, Eval0, St0) ->
    Eval0 ! {eval_expr, self(), Form},
    receive
        {eval_value, Eval0, _Value, St1} ->
            {Eval0, St1};
        {eval_error, Eval0, {Class, Reason, Stack}} ->
            % Evaluator crashed
            receive {'EXIT', Eval0, normal} -> ok end,
            report_exception(Class, Reason, Stack),
            Eval1 = start_eval(St0),
            {Eval1, St0};
        {'EXIT', Eval0, Reason} ->
            % Evaluator was killed
            report_exception(exit, Reason, []),
            Eval1 = start_eval(St0),
            {Eval1, St0}
    end.
```

#### Slurping

**Purpose**: Load an LFE file's functions and macros into the shell without compiling.

**Slurp Process** (`slurp/2` at lines 559-601):

1. Save current environment
2. Read and parse file
3. Macro expand forms
4. Lint forms
5. Extract functions, macros, imports, records
6. Add to environment
7. Mark state as slurped

**Unslurp** (`unslurp/1` at lines 550-557):

Revert to saved environment before slurp.

**Module Collection** (`collect_module/2` at lines 628-642):

Extracts from parsed forms:

- Module name
- Exported functions
- Function definitions
- Imports (from, rename)
- Record definitions

**Example**:

```lisp
lfe> (slurp "mylib.lfe")
{ok, mylib}

lfe> (my-function 42)  ; Now available
84

lfe> (unslurp)
ok

lfe> (my-function 42)
** exception error: undefined function my-function/1
```

#### Banner and Prompt

**Banner** (`banner/0,1,2` at lines 243-259):

Colorful ASCII art LFE banner displayed on startup:

```
   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/lfe/lfe
   \     l    |_/    |
    \   r     /      |   LFE v2.2.0 (abort with ^G)
     `-E___.-'
```

**Prompt** (`prompt/0` at lines 169-200):

Customizable via `-prompt` flag:

```bash
erl -user lfe_init -prompt "my-prompt> "
erl -user lfe_init -prompt classic  # Old-style "> "
```

Supports `~node` placeholder for distributed nodes.

#### Documentation Display

**Paged Output** (`paged_output/2` at lines 1154-1176):

Documentation output is paged (30 lines per page) with "More (y/n)?" prompts.

**Format Selection** (`get_module_doc/2` etc. at lines 1108-1148):

Handles both:

- LFE format docs (via `lfe_shell_docs`)
- Erlang native format docs (via `shell_docs` in OTP 23+)

#### Dependencies

**LFE modules**:

- `lfe_eval` - Expression evaluation
- `lfe_env` - Environment management
- `lfe_macro` - Macro expansion
- `lfe_comp` - File compilation
- `lfe_lint` - Linting
- `lfe_io` - I/O operations
- `lfe_edlin_expand` - Command-line expansion
- `lfe_docs`, `lfe_shell_docs` - Documentation
- `lfe_error` - Error formatting

**Erlang stdlib**:

- `c` module - Many shell commands delegate to `c`
- `code`, `file`, `io`

#### Used By

- `lfe_init` - Default shell
- `erl -user lfe_init` - Interactive sessions
- `lfescript` - Script execution

#### Key Algorithms

**REPL Loop** (`server_loop/2` at lines 133-144):

```erlang
server_loop(Eval0, St0) ->
    Prompt = prompt(),
    {Ret, Eval1} = read_expression(Prompt, Eval0, St0),
    case Ret of
        {ok, Form} ->
            {Eval2, St1} = shell_eval(Form, Eval1, St0),
            server_loop(Eval2, St1);
        {error, E} ->
            list_errors([E]),
            server_loop(Eval1, St0)
    end.
```

**Form Evaluation** (`eval_form_1/2` at lines 465-501):

Special handling for:

- `(set ...)` - Pattern matching with bindings
- `(slurp ...)` - File slurping
- `(unslurp)` - Revert slurp
- `(run ...)` - Run script file
- `(reset-environment)` - Revert to base
- `(define-record ...)` - Add record
- `(define-function ...)` - Add function
- `(define-macro ...)` - Add macro
- Regular expressions - Evaluate

#### Special Considerations

**Slurp vs Compile**:

- `(slurp "file.lfe")` - Load into shell (interpreted)
- `(c "file.lfe")` - Compile to BEAM (faster)

Slurping is useful for interactive development; compilation for production.

**History Storage**: Not persistent across sessions (unlike some Erlang shells).

**Command Completion**: Integrated with `lfe_edlin_expand` for tab completion.

**Performance**: Slurped code runs slower (interpreted) than compiled code.
