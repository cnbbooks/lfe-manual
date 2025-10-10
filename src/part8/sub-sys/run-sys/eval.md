# The Evaluator (lfe_eval.erl)

**Purpose**: Interpret and execute LFE code at runtime without compilation.

**Module**: `lfe_eval.erl` (2,004 LOC) - Location: `src/lfe_eval.erl`

**Public API**:

```erlang
expr(Sexpr) -> Value
expr(Sexpr, Env) -> Value

exprs([Sexpr]) -> [Value]
exprs([Sexpr], Env) -> [Value]

body([Sexpr]) -> Value  % Evaluates sequence, returns last
body([Sexpr], Env) -> Value

apply(Function, Args) -> Value
apply(Function, Args, Env) -> Value

match(Pattern, Value, Env) -> {yes, Bindings} | no
match_when(Pattern, Value, Guards, Env) -> {yes, Body, Bindings} | no
```

**Evaluation strategy**:

From src/lfe_eval.erl:185-349:

- **Eager evaluation** (call-by-value, not lazy)
- **Left-to-right** argument evaluation
- **Tail call optimization** via Erlang's TCO

**Core evaluation loop**:

```erlang
eval_expr(Sexpr, Env) -> Value

% Literals
eval_expr(Atom, Env) when is_atom(Atom) -> Atom;
eval_expr(Number, Env) when is_number(Number) -> Number;

% Variables
eval_expr(Var, Env) when is_atom(Var) ->
    lfe_env:fetch_vbinding(Var, Env);

% Special forms
eval_expr([quote, E], _) -> E;
eval_expr([cons, H, T], Env) -> [eval_expr(H, Env) | eval_expr(T, Env)];
eval_expr([lambda|_]=L, Env) -> eval_lambda(L, Env);
eval_expr([if, Test, Then, Else], Env) -> eval_if(Test, Then, Else, Env);
eval_expr([case, Expr|Clauses], Env) -> eval_case(Expr, Clauses, Env);

% Function calls
eval_expr([Fun|Args], Env) when is_atom(Fun) ->
    Vals = eval_list(Args, Env),  % Eager evaluation
    eval_fun_call(Fun, length(Args), Vals, Env);
```

**Data constructors**:

```erlang
eval_expr([quote, E], _) -> E

eval_expr([cons, H, T], Env) ->
    [eval_expr(H, Env) | eval_expr(T, Env)]

eval_expr([list|Es], Env) ->
    eval_list(Es, Env)

eval_expr([tuple|Es], Env) ->
    list_to_tuple(eval_list(Es, Env))

eval_expr([binary|Segs], Env) ->
    eval_binary(Segs, Env)

eval_expr([map|KVs], Env) ->
    eval_map(KVs, Env)
```

**Closures**:

From src/lfe_eval.erl:743-792:

```erlang
eval_lambda([lambda, Args|Body], Env) ->
    % Capture current environment
    Apply = fun (Vals) -> apply_lambda(Args, Body, Vals, Env) end,
    Arity = length(Args),
    make_lambda(Arity, Apply)

make_lambda(Arity, Apply) ->
    case Arity of
        0 -> fun () -> Apply([]) end;
        1 -> fun (A) -> Apply([A]) end;
        2 -> fun (A, B) -> Apply([A, B]) end;
        % ... up to arity 20
        _ -> eval_error({argument_limit, Arity})
    end
```

**Key insight**: Closures are Erlang funs that **capture the environment**.

**Closure application**:

```erlang
apply_lambda(Args, Body, Vals, CapturedEnv) ->
    % Extend captured environment with arguments
    Env1 = bind_args(Args, Vals, CapturedEnv),
    eval_body(Body, Env1)
```

**Match-lambda** (multi-clause functions):

```erlang
eval_match_lambda(['match-lambda'|Clauses], Env) ->
    Apply = fun (Vals) -> apply_match_lambda(Clauses, Vals, Env) end,
    make_lambda(match_lambda_arity(Clauses), Apply)

apply_match_lambda([[Pats|Body]|Clauses], Vals, Env) ->
    % Try to match patterns against values
    case match_when([list|Pats], Vals, Body, Env) of
        {yes, BodyExprs, Bindings} ->
            eval_body(BodyExprs, lfe_env:add_vbindings(Bindings, Env));
        no ->
            apply_match_lambda(Clauses, Vals, Env)  % Try next clause
    end;
apply_match_lambda([], _, _) ->
    eval_error(function_clause)
```

**Let bindings**:

```erlang
eval_let([Bindings|Body], Env0) ->
    Env1 = eval_let_bindings(Bindings, Env0),
    eval_body(Body, Env1)

% Each binding: [Pattern, Expr] or [Pattern, (when Guards), Expr]
Val = eval_expr(Expr, Env0),
{yes, NewBindings} = match(Pattern, Val, Env0),
Env1 = add_vbindings(NewBindings, Env0)
```

**Control flow**:

```erlang
eval_expr(['progn'|Body], Env) ->
    eval_body(Body, Env)  % Sequence evaluation

eval_expr(['if', Test, Then, Else], Env) ->
    case eval_expr(Test, Env) of
        true -> eval_expr(Then, Env);
        false -> eval_expr(Else, Env);
        _ -> error(badarg)  % Non-boolean test
    end

eval_expr(['case', Expr|Clauses], Env) ->
    Val = eval_expr(Expr, Env),
    eval_case_clauses(Val, Clauses, Env)

eval_expr(['receive'|Clauses], Env) ->
    eval_receive(Clauses, Env)

eval_expr(['try'|Body], Env) ->
    eval_try(Body, Env)
```

**Function calls**:

```erlang
eval_expr([call, Mod, Fun|Args], Env) ->
    M = eval_expr(Mod, Env),
    F = eval_expr(Fun, Env),
    Vals = eval_list(Args, Env),
    erlang:apply(M, F, Vals)

eval_expr([funcall, F|Args], Env) ->
    Fun = eval_expr(F, Env),
    Vals = eval_list(Args, Env),
    eval_apply_expr(Fun, Vals, Env)

eval_expr([Fun|Args], Env) when is_atom(Fun) ->
    Arity = length(Args),
    Vals = eval_list(Args, Env),
    eval_fun_call(Fun, Arity, Vals, Env)
```

**Function binding lookup** (src/lfe_eval.erl:598-612):

Priority order:

1. **Locally bound functions** (from `let-function`, `letrec-function`)
2. **Imported functions** (from module imports)
3. **LFE BIFs** (auto-imported from `lfe` module)
4. **Erlang BIFs** (auto-imported from `erlang` module)
5. **Error** if not found
