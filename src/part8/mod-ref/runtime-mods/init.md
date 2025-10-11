# lfe_init.erl - Runtime Initialization

**Purpose**: Initialize LFE runtime environment when starting Erlang with LFE as the default shell.

**Location**: `src/lfe_init.erl`
**Size**: 118 LOC, 4.1KB

**Module Classification**: Runtime infrastructure, launcher

#### Public API

```erlang
start() -> ok
```

Entry point for `-user lfe_init`. Located at `lfe_init.erl:49-74`.

#### Startup Modes

**Interactive Shell**:

```bash
erl -user lfe_init
```

Starts LFE REPL as default shell.

**Eval Mode**:

```bash
erl -user lfe_init -e "(+ 1 2)"
```

Evaluates expression and exits.

**Script Mode**:

```bash
erl -user lfe_init script.lfe arg1 arg2
```

Runs script with arguments.

#### Implementation

**Main Entry** (`start/0` at lines 49-74):

```erlang
start() ->
    OTPRelease = erlang:system_info(otp_release),
    Repl = case init:get_argument(repl) of
        {ok, [[R|_]]} -> list_to_atom(R);
        _Other -> ?DEFAULT_REPL  % lfe_shell
    end,
    case collect_args(init:get_plain_arguments()) of
        {[],[]} ->
            % Start interactive shell
            if OTPRelease >= "26" ->
                user_drv:start(#{initial_shell => {Repl,start,[]}});
               true ->
                user_drv:start(['tty_sl -c -e',{Repl,start,[]}])
            end;
        {Evals,Script} ->
            % Run evals and/or script
            if OTPRelease >= "26" ->
                user_drv:start(#{initial_shell => noshell});
               true ->
                user:start()
            end,
            run_evals_script(Repl, Evals, Script)
    end.
```

**OTP Version Handling**:

OTP 26 changed `user_drv` API:

- **OTP <26**: `user_drv:start(['tty_sl -c -e', {Repl,start,[]}])`
- **OTP ≥26**: `user_drv:start(#{initial_shell => {Repl,start,[]}})`

**Argument Collection** (`collect_args/1` at lines 76-86):

```erlang
collect_args(Args) ->
    {Evals, Rest} = collect_evals(Args, []),
    {Evals, Rest}.

collect_evals(["-e", Eval|Args], Evals) ->
    collect_evals(Args, [Eval|Evals]);
collect_evals(["-lfe_eval", Eval|Args], Evals) ->
    collect_evals(Args, [Eval|Evals]);
collect_evals(Args, Evals) ->
    {lists:reverse(Evals), Args}.
```

**Script Execution** (`run_evals_script/3` at lines 88-97):

```erlang
run_evals_script(Repl, Evals, Script) ->
    Run = fun () ->
        St = Repl:run_strings(Evals),  % Run eval strings
        case Script of
            [F|As] -> Repl:run_script(F, As, St);
            [] -> {[],St}
        end
    end,
    spawn_link(fun () -> run_script(Repl, Run) end).
```

**Error Handling** (`run_script/2` at lines 102-118):

```erlang
run_script(_Repl, Run) ->
    try
        Run(),
        timer:sleep(1),
        init:stop(?OK_STATUS)  % Exit 0
    catch
        ?CATCH(Class, Error, Stack)
            Skip = fun (M, _F, _A) -> M =/= lfe_eval end,
            Format = fun (T, I) -> lfe_io:prettyprint1(T, 15, I, 80) end,
            Cs = lfe_error:format_exception(Class, Error, Stack,
                                            Skip, Format, 1),
            io:put_chars(Cs),
            halt(?ERROR_STATUS)  % Exit 127
    end.
```

**Exit Codes**:

- `0` - Success
- `127` - Error

#### Custom REPL

**Specify Alternative REPL**:

```bash
erl -user lfe_init -repl my_custom_repl
```

Default: `lfe_shell`

#### Dependencies

**LFE modules**:

- `lfe_shell` (or custom REPL)
- `lfe_error` - Error formatting
- `lfe_io` - Pretty printing

**Erlang stdlib**:

- `user_drv` - Terminal driver
- `init` - System initialization

#### Used By

- `erl` - When started with `-user lfe_init`
- `lfescript` - Indirectly for script execution

#### Special Considerations

**OTP Compatibility**:

The OTP 26 check ensures compatibility across Erlang versions.

**Process Spawning**:

Script execution spawns a separate process to isolate crashes.

**Exit Codes**:

Standard Unix convention:

- `0` = success
- Non-zero = error (127 for exceptions)

**Terminal Setup**:

`-c -e` flags enable proper terminal handling (command-line editing, escape sequences).
