# lfescript.erl - Script Runner

**Purpose**: Execute LFE scripts (lfescript files) with command-line argument handling.

**Location**: `src/lfescript.erl`
**Size**: 189 LOC, 6.3KB

**Module Classification**: Runtime infrastructure, script execution

#### Public API

```erlang
start() -> ok
start(Args) -> ok
```

Main entry point for lfescript execution. Located at `lfescript.erl:47-52`.

#### Script Format

**Lfescript File**:

```lisp
#!/usr/bin/env lfescript
;; -*- mode: lfe -*-
;;! -smp enable -setcookie mycookie

(defun main (args)
  (lfe_io:format "Args: ~p~n" (list args)))
```

**Shebang Line**: `#!/usr/bin/env lfescript`

**Mode Line**: `;; -*- mode: lfe -*-` (optional, for editors)

**Emulator Flags**: `;;!` followed by erl flags (optional, line 2 or 3)

#### Script Execution

**Process** (`start/1` at lines 54-89):

1. Parse script file
2. Extract emulator flags (`;;!` line)
3. Read and expand forms
4. Execute `main/1` function with arguments

```erlang
start(Args) ->
    [Script|ScriptArgs] = Args,
    {ok, Forms} = lfe_io:read_file(Script),
    % Expand macros
    {ok, EForms, Env, _} = lfe_macro:expand_forms(Forms, lfe_env:new(), ...),
    % Create environment with main/1
    Env1 = eval_script_forms(EForms, Env),
    % Call main/1
    Main = lfe_env:get_fbinding(main, 1, Env1),
    lfe_eval:apply(Main, [ScriptArgs], Env1).
```

#### Emulator Flags

**Flag Parsing** (`extract_emulator_flags/1` at lines 101-134):

```erlang
% Shebang line must be first
parse_flags(["#!/usr/bin/env lfescript" | Lines]) ->
    parse_flags_rest(Lines);

% Skip mode line
parse_flags_rest([";; -*- mode: lfe -*-" | Lines]) ->
    parse_flags_rest(Lines);

% Extract flags
parse_flags_rest([";;!" ++ Flags | _Lines]) ->
    string:tokens(Flags, " \t");

parse_flags_rest(_) ->
    [].  % No flags
```

**Example**:

```lisp
#!/usr/bin/env lfescript
;;! -smp enable -setcookie secret -name node1

; Script runs with: erl -smp enable -setcookie secret -name node1
```

#### Error Handling

**Script Errors**:

Exceptions during script execution are caught and formatted:

```erlang
try
    run_script(Script, Args)
catch
    Class:Reason:Stack ->
        lfe_error:format_exception(Class, Reason, Stack),
        halt(127)
end.
```

**Exit Codes**:

- `0` - Success
- `127` - Error

#### Dependencies

**LFE modules**:

- `lfe_io` - File reading
- `lfe_macro` - Macro expansion
- `lfe_eval` - Script execution
- `lfe_env` - Environment
- `lfe_error` - Error formatting

**Erlang stdlib**:

- `file`, `string`, `lists`

#### Used By

- `lfescript` shell script (wrapper)
- Direct execution via shebang

#### Special Considerations

**Compilation**:

Scripts are **not** compiled to BEAM. They're interpreted via `lfe_eval`.

**Performance**:

Script startup is slower than compiled modules:

- Parsing
- Macro expansion
- Interpretation overhead

**Use Cases**:

- Build scripts
- Admin tasks
- One-off utilities

**main/1 Required**:

Script must define `main/1`:

```lisp
(defun main (args)
  ; Script logic here
  )
```

**Arguments**:

`args` is a list of strings:

```bash
./script.lfe foo bar baz
; args = ["foo", "bar", "baz"]
```
