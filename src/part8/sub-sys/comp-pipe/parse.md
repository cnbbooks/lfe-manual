# Stage 2: Syntactic Analysis (lfe_parse.erl)

**Purpose**: Convert token stream into S-expressions (LFE's native AST).

**Module**: `lfe_parse.erl` (284 LOC)

**Why so small?** S-expressions are trivial to parse. The grammar is:

```
Sexpr → Atom | Number | String | Binary | List | Tuple | Map | Quote
List  → '(' Sexpr* ')'  |  '[' Sexpr* ']'
Quote → "'" Sexpr | "`" Sexpr | "," Sexpr | ",@" Sexpr
Hash  → "#(" Sexpr* ")" | "#B(" Bitsegs ")" | "#M(" Pairs ")"
```

**Parser type**: LL(1) shift-reduce parser with explicit state stack.

**Parser state**:

```erlang
-record(spell1, {
    line = none,    % Current line number
    st = [],        % State stack
    vs = []         % Value stack
}).
```

**Parsing algorithm** (from `spell1/2` at line 147-192):

```
1. Read token
2. Consult parse table: table(State, Token) → Action
3. Action = shift   → Push state, push value, continue
4. Action = reduce  → Pop values, apply reduction rule, push result
5. Repeat until accept or error
```

**Parse table excerpt** (lines 195-227):

```erlang
%% State transitions
table([start|_], {'(', _}) -> {shift, 1, s_list};
table([start|_], {'[', _}) -> {shift, 1, s_bracket};
table([start|_], {symbol, _, S}) -> {shift_reduce, 0, 1, S};
table([start|_], {number, _, N}) -> {shift_reduce, 0, 1, N};
table([start|_], {string, _, S}) -> {shift_reduce, 0, 1, S};
...
```

**Example transformation**:

```lisp
;; Tokens
[{'(', 1}, {symbol, 1, hello}, {symbol, 1, world}, {')', 1}]

;; S-expression produced
[hello, world]
```

**Special forms handling**:

```erlang
%% Quote: 'expr → [quote, expr]
table([start|_], {'\'', _}) -> {shift, 2, s_quote};

%% Backquote: `expr → [backquote, expr]
table([start|_], {'`', _}) -> {shift, 2, s_backquote};

%% Comma: ,expr → [comma, expr]
table([start|_], {',', _}) -> {shift, 2, s_comma};

%% Comma-at: ,@expr → [comma-at, expr]
table([start|_], {',@', _}) -> {shift, 2, s_comma_at};
```
