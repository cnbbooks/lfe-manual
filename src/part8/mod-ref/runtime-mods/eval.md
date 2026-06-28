# lfe_eval.erl - Expression Evaluator

**Purpose**: Interpret and execute LFE expressions at runtime. This is the **third-largest** module in the codebase and implements a complete LFE interpreter.

**Location**: `src/lfe_eval.erl`
**Size**: 2,004 LOC, 68KB

**Module Classification**: Runtime core, interpreter

#### Public API

**Expression Evaluation**:

```erlang
expr(Sexpr) -> Value
expr(Sexpr, Env) -> Value
```

Evaluate a single expression. Located at `lfe_eval.erl:107-109`.

**Multiple Expressions**:

```erlang
exprs([Sexpr]) -> [Value]
exprs([Sexpr], Env) -> [Value]
```

Evaluate list of expressions. Located at `lfe_eval.erl:113-115`.

**Body Evaluation**:

```erlang
body([Sexpr]) -> Value
body([Sexpr], Env) -> Value
```

Evaluate sequence, return last value. Located at `lfe_eval.erl:119-121`.

**Function Application**:

```erlang
apply(Function, Args) -> Value
apply(Function, Args, Env) -> Value
```

Apply function to arguments. Located at `lfe_eval.erl:125-127`.

**Pattern Matching**:

```erlang
match(Pattern, Value, Env) -> {yes, Bindings} | no
match_when(Pattern, Value, Guards, Env) -> {yes, Body, Bindings} | no
```

Pattern matching with optional guards. Located at `lfe_eval.erl:131-133`.

#### Evaluation Strategy

**Call-by-Value Semantics**:

All arguments are evaluated **before** function application:

```erlang
eval_expr([Fun|Args], Env) when is_atom(Fun) ->
    Vals = eval_list(Args, Env),  % Evaluate args first
    eval_fun_call(Fun, length(Args), Vals, Env).
```

**Left-to-Right Evaluation**:

Arguments evaluated in order:

```erlang
eval_list([E|Es], Env) ->
    [eval_expr(E, Env) | eval_list(Es, Env)].
```

**Eager Evaluation**:

No lazy evaluation. All sub-expressions evaluated immediately.

#### Core Form Evaluation

**Data Constructors** (`eval_expr/2` at lines 191-224):

```erlang
eval_expr([quote, E], _Env) -> E;
eval_expr([cons, H, T], Env) ->
    [eval_expr(H, Env) | eval_expr(T, Env)];
eval_expr([list|Es], Env) ->
    eval_list(Es, Env);
eval_expr([tuple|Es], Env) ->
    list_to_tuple(eval_list(Es, Env));
eval_expr([binary|Segs], Env) ->
    eval_binary(Segs, Env);
eval_expr([map|Assocs], Env) ->
    eval_map(Assocs, Env);
```

**Closures** (`eval_lambda/2` at lines 743-792):

```erlang
eval_lambda([lambda, Args|Body], Env) ->
    % Capture current environment
    Apply = fun (Vals) -> apply_lambda(Args, Body, Vals, Env) end,
    Arity = length(Args),
    make_lambda(Arity, Apply).

% Create Erlang fun with specific arity
make_lambda(0, Apply) -> fun () -> Apply([]) end;
make_lambda(1, Apply) -> fun (A) -> Apply([A]) end;
make_lambda(2, Apply) -> fun (A,B) -> Apply([A,B]) end;
...
% Up to arity 20
make_lambda(20, Apply) -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) ->
    Apply([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) end.
```

**Note**: Arity limit of 20 (matches Erlang's `erl_eval`).

**Let Bindings** (`eval_let/2` at lines 846-868):

```erlang
eval_let([Bindings|Body], Env0) ->
    Env1 = eval_let_bindings(Bindings, Env0),
    eval_body(Body, Env1).

eval_let_bindings([[Pat, Expr]|Rest], Env0) ->
    Val = eval_expr(Expr, Env0),  % Evaluate in OLD env
    {yes, NewBindings} = match(Pat, Val, Env0),
    Env1 = add_vbindings(NewBindings, Env0),
    eval_let_bindings(Rest, Env1).
```

**Control Flow** (`eval_if/2` at lines 812-828):

```erlang
eval_if([Test, Then, Else], Env) ->
    case eval_expr(Test, Env) of
        true -> eval_expr(Then, Env);
        false -> eval_expr(Else, Env);
        _ -> error(badarg)  % Non-boolean test
    end.
```

**Case Expression** (`eval_case/2` at lines 916-951):

```erlang
eval_case([Expr|Clauses], Env) ->
    Val = eval_expr(Expr, Env),
    eval_case_clauses(Clauses, Val, Env).

eval_case_clauses([[Pat|Body]|Clauses], Val, Env) ->
    case match_when(Pat, Val, Body, Env) of
        {yes, BodyExprs, Bindings} ->
            eval_body(BodyExprs, add_vbindings(Bindings, Env));
        no ->
            eval_case_clauses(Clauses, Val, Env)
    end.
```

**Receive Expression** (`eval_receive/2` at lines 1001-1068):

```erlang
eval_receive([['after', Timeout|TimeoutBody]], Env) ->
    T = eval_expr(Timeout, Env),
    receive after T -> eval_body(TimeoutBody, Env) end;

eval_receive(Clauses, Env) ->
    receive
        Msg -> eval_receive_clauses(Clauses, Msg, Env)
    end.

eval_receive_clauses([[Pat|Body]|Clauses], Msg, Env) ->
    case match_when(Pat, Msg, Body, Env) of
        {yes, BodyExprs, Bindings} ->
            eval_body(BodyExprs, add_vbindings(Bindings, Env));
        no ->
            % Put message back, try next clause
            self() ! Msg,
            eval_receive_clauses(Clauses, Msg, Env)
    end.
```

**Note**: Failed pattern matches put message back in mailbox!

#### Function Calls

**Local Function Call** (`eval_fun_call/4` at lines 394-413):

```erlang
eval_fun_call(Fun, Arity, Args, Env) ->
    case lfe_env:get_fbinding(Fun, Arity, Env) of
        {yes, M, F} ->
            % Imported function
            erlang:apply(M, F, Args);
        {yes, Def} ->
            % Local function binding
            eval_apply(Def, Args, Env);
        no ->
            % Try LFE BIFs, then Erlang BIFs
            eval_bif(Fun, Arity, Args, Env)
    end.
```

**BIF Lookup Priority**:

1. Locally bound functions (let-function, letrec-function)
2. Imported functions
3. LFE BIFs (lfe module)
4. Erlang BIFs (erlang module)

**Remote Call** (`eval_expr/2` at lines 317-322):

```erlang
eval_expr([call, Mod, Fun|Args], Env) ->
    M = eval_expr(Mod, Env),
    F = eval_expr(Fun, Env),
    Vals = eval_list(Args, Env),
    erlang:apply(M, F, Vals).
```

**Funcall** (`eval_expr/2` at lines 329-333):

```erlang
eval_expr([funcall, FunExpr|Args], Env) ->
    Fun = eval_expr(FunExpr, Env),
    Vals = eval_list(Args, Env),
    eval_apply_expr(Fun, Vals, Env).
```

#### Pattern Matching

**Pattern Match** (`match/3` at lines 1104-1242):

```erlang
% Literals must match exactly
match([quote, Lit], Val, _Env) ->
    if Lit =:= Val -> {yes, []};
       true -> no
    end;

% Variables bind
match(Var, Val, _Env) when is_atom(Var) ->
    {yes, [{Var, Val}]};

% Don't-care
match('_', _Val, _Env) ->
    {yes, []};

% Cons cells
match([cons, H, T], [Hval|Tval], Env) ->
    case match(H, Hval, Env) of
        {yes, Hbs} ->
            case match(T, Tval, Env) of
                {yes, Tbs} -> {yes, Hbs ++ Tbs};
                no -> no
            end;
        no -> no
    end;

% Tuples
match([tuple|Pats], Val, Env) when is_tuple(Val) ->
    match_list(Pats, tuple_to_list(Val), Env);

% Maps
match([map|Assocs], Val, Env) when is_map(Val) ->
    match_map_assocs(Assocs, Val, Env);

% Binaries
match([binary|Segs], Val, Env) when is_binary(Val) ->
    match_binary(Segs, Val, Env);

% Aliases
match(['=', Pat1, Pat2], Val, Env) ->
    case match(Pat1, Val, Env) of
        {yes, Bs1} ->
            case match(Pat2, Val, Env) of
                {yes, Bs2} -> {yes, Bs1 ++ Bs2};
                no -> no
            end;
        no -> no
    end.
```

**Guard Evaluation** (`eval_guard/2` at lines 1279-1335):

```erlang
eval_guard([['andalso'|Tests]], Env) ->
    eval_guard_and(Tests, Env);
eval_guard([['orelse'|Tests]], Env) ->
    eval_guard_or(Tests, Env);
eval_guard([[Test]], Env) ->
    eval_guard_test(Test, Env).

eval_guard_test(Test, Env) ->
    Val = eval_expr(Test, Env),
    Val =:= true.  % Must be exactly 'true'
```

#### Records and Structs

**Record Operations** (`eval_expr/2` at lines 226-241):

```erlang
% Create record
eval_expr([record, Name|FieldVals], Env) ->
    {ok, Fields} = lfe_env:get_record(Name, Env),
    make_record_tuple(Name, Fields, FieldVals, Env);

% Access field
eval_expr(['record-field', Rec, Name, Field], Env) ->
    RecVal = eval_expr(Rec, Env),
    get_record_field(RecVal, Name, Field, Env);

% Update record
eval_expr(['record-update', Rec, Name|Updates], Env) ->
    RecVal = eval_expr(Rec, Env),
    update_record_tuple(RecVal, Name, Updates, Env).
```

**Struct Operations** (`eval_expr/2` at lines 242-256):

```erlang
% Create struct
eval_expr([struct, Mod|FieldVals], Env) ->
    Mod:'__struct__'(eval_pairs(FieldVals, Env));

% Test struct
eval_expr(['is-struct', Struct, Mod], Env) ->
    StructVal = eval_expr(Struct, Env),
    lfe_struct:is(StructVal, Mod);

% Access field
eval_expr(['struct-field', Struct, Mod, Field], Env) ->
    StructVal = eval_expr(Struct, Env),
    lfe_struct:fetch(StructVal, Mod, Field).
```

#### Comprehensions

**List Comprehension** (`eval_list_comp/3` at lines 1367-1452):

```erlang
eval_list_comp(Qualifiers, Expr, Env) ->
    eval_lc_quals(Qualifiers, Expr, Env, []).

eval_lc_quals([['<-', Pat, ListExpr]|Quals], Expr, Env, Acc) ->
    List = eval_expr(ListExpr, Env),
    lists:foldr(fun (Elem, Acc1) ->
        case match(Pat, Elem, Env) of
            {yes, Bindings} ->
                Env1 = add_vbindings(Bindings, Env),
                eval_lc_quals(Quals, Expr, Env1, Acc1);
            no -> Acc1
        end
    end, Acc, List);

eval_lc_quals([TestExpr|Quals], Expr, Env, Acc) ->
    case eval_expr(TestExpr, Env) of
        true -> eval_lc_quals(Quals, Expr, Env, Acc);
        false -> Acc
    end;

eval_lc_quals([], Expr, Env, Acc) ->
    [eval_expr(Expr, Env) | Acc].
```

**Binary Comprehension**: Similar structure, builds binaries instead of lists.

#### Dependencies

**LFE modules**:

- `lfe_env` (39 calls!) - Heavy environment usage
- `lfe_io` (18 calls) - I/O operations
- `lfe_internal` (14 calls) - Type checking
- `lfe_lib` (10 calls) - Utilities
- `lfe_macro` (4 calls) - Macro expansion
- `lfe_eval_bits` (4 calls) - Binary evaluation
- `lfe_bits` (1 call) - Bitstring specs

**Erlang stdlib**:

- `lists`, `maps`, `erlang`

#### Used By

- `lfe_shell` - REPL evaluation
- `lfe_macro` - `eval-when-compile`
- `lfescript` - Script execution
- `lfe_init` - Runtime initialization

#### Key Algorithms

**Closure Application** (`apply_lambda/4` at lines 800-802):

```erlang
apply_lambda(Args, Body, Vals, CapturedEnv) ->
    Env1 = bind_args(Args, Vals, CapturedEnv),
    eval_body(Body, Env1).

bind_args(['_'|As], [_|Vals], Env) ->
    bind_args(As, Vals, Env);  % Ignore don't-care
bind_args([Arg|As], [Val|Vals], Env) ->
    bind_args(As, Vals, lfe_env:add_vbinding(Arg, Val, Env));
bind_args([], [], Env) ->
    Env.
```

**Let-Function** (`eval_let_function/2` at lines 870-883):

```erlang
eval_let_function([Bindings|Body], Env0) ->
    % Add function definitions to environment
    Fun = fun ([Name, Lambda], E) ->
        add_lexical_func(Name, Lambda, Env0, E)
    end,
    Env1 = lists:foldl(Fun, Env0, Bindings),
    eval_body(Body, Env1).
```

**Letrec-Function** (`eval_letrec_function/2` at lines 889-899):

```erlang
eval_letrec_function([Bindings|Body], Env0) ->
    % Create recursive environment
    Fbs = extract_function_bindings(Bindings),
    Env1 = make_letrec_env(Fbs, Env0),
    eval_body(Body, Env1).

make_letrec_env(FuncBindings, Env) ->
    % Each function sees the full environment (mutual recursion)
    ...
```

#### Special Considerations

**Performance**:

Interpreted code is **10-100x slower** than compiled code:

- No optimization
- Environment lookups
- Pattern matching overhead

**Arity Limit**:

Closures support up to **arity 20** (lines 758-792).

**Tail Call Optimization**:

TCO relies on Erlang's TCO (body-recursive functions in tail position).

**Memory Usage**:

Each closure captures its environment → potential memory leaks if not careful.

**Error Handling**:

Runtime errors throw exceptions with stack traces pointing to evaluator, not source.

**Use Cases**:

- REPL (interactive development)
- `eval-when-compile` (compile-time code execution)
- Scripts (lfescript)
- Testing (eval test expressions)
