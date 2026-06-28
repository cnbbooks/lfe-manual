# lfe_parse.erl - S-Expression Parser

**Purpose**: Transform token stream into s-expression Abstract Syntax Trees (ASTs).

**Location**: `src/lfe_parse.erl`
**Size**: 284 LOC, 11KB

**Module Classification**: Compiler frontend, syntactic analysis

#### Public API

```erlang
form(Tokens) -> {ok, Line, Sexpr, RestTokens}
              | {more, Continuation}
              | {error, ErrorInfo, RestTokens}
```

Parse a single top-level form from tokens. Located at `lfe_parse.erl:46-48`.

```erlang
forms(Tokens) -> {ok, Forms}
               | {error, ErrorInfo, RestTokens}
```

Parse all forms from a token stream. Located at `lfe_parse.erl:50-52`.

#### Parser Type

**LL(1) shift-reduce parser** with explicit state and value stacks.

**Parser State Record**:

```erlang
-record(spell1, {
    line=none,    % Current line number
    st=[],        % State stack
    vs=[]         % Value stack
}).
```

Located at `lfe_parse.erl:38-43`.

#### Grammar

The parser handles standard S-expression grammar with LFE extensions:

**Basic Forms**:

- Lists: `(a b c)`, `[a b c]`
- Atoms: `foo`, `foo-bar`, `|quoted|`
- Numbers: `42`, `3.14`
- Strings: `"hello"`
- Binaries: `#"bytes"`

**Special Syntax**:

- Quote: `'expr` → `(quote expr)`
- Backquote: `` `expr `` → `(backquote expr)`
- Comma: `,expr` → `(comma expr)`
- Comma-at: `,@expr` → `(comma-at expr)`
- Tuple: `#(a b)` → Tuple `{a, b}`
- Binary: `#B(42)` → Binary construction
- Map: `#M(a 1)` → Map `#{a => 1}`
- Function ref: `#'mod:func/2` → `(function mod func 2)`

**Improper Lists** (dotted pairs):

```lisp
(a . b)         ; Cons cell
(a b . c)       ; Improper list
```

#### Parsing Algorithm

**Shift-Reduce with Table** (`lfe_parse.erl:107-229`):

The parser uses a `table/2` function encoding the parsing table:

```erlang
table(State, Token) -> {shift, NewState}
                     | {reduce, RuleNumber}
                     | {accept, RuleNumber}
                     | {error, Error}
```

**Reduction Rules** (`reduce/2` at lines 238-262):

- Rule 0: Accept top-level form
- Rules 1-4: Extract token values
- Rule 5: Function literal `#'F/A`
- Rule 6: Eval literal `#.expr`
- Rules 7-10: Quote transformations
- Rules 11-12: List construction
- Rule 13: Tuple construction
- Rule 14: Binary construction
- Rule 15: Map construction
- Rules 16-22: Cons cell handling

#### Special Form Processing

**Function References** (`make_fun/1` at lines 277-295):

Transforms `#'function/arity` and `#'module:function/arity`:

```lisp
#'foo/2              → (function foo 2)
#'lists:map/2        → (function lists map 2)
#'Mod:func/1         → (function Mod func 1)
```

**Eval-at-Read** (`eval_expr/2` at lines 307-320):

The `#.` form evaluates expressions at read time:

```lisp
#.(+ 1 2)           → 3
#.(list 'a 'b)      → (a b)
```

**Note**: Eval-at-read is **limited** - only safe operations allowed.

**Binary Construction** (`make_bin/2` at lines 324-334):

Transforms binary syntax:

```lisp
#B(42)              → Binary with byte 42
#B(42 (f 32))       → 32-bit float
#B((str "hello"))   → Binary string
```

**Map Construction** (`make_map/2` at lines 338-361):

Transforms map syntax:

```lisp
#M(a 1 b 2)         → #{a => 1, b => 2}
```

#### Dependencies

**Erlang stdlib**:

- `lists` - List manipulation
- No other dependencies

**LFE modules**: None (parser is self-contained)

#### Used By

- `lfe_comp` - Compilation pipeline
- `lfe_shell` - REPL input parsing
- `lfe_io:read*` - Reading s-expressions from text

#### Key Algorithms

**Parse Stack Management**:

The parser maintains two stacks:

1. **State stack** (`st`): Parsing states
2. **Value stack** (`vs`): Reduced values

```erlang
parse_1(Tokens, St0) ->
    case scan_token(Tokens, St0) of
        {shift, NewState, St1} ->
            parse_1(RestTokens, St1);
        {reduce, Rule, St1} ->
            St2 = reduce(Rule, St1),
            parse_1(Tokens, St2);
        {accept, St1} ->
            {ok, value(St1)};
        {error, Error} ->
            {error, Error}
    end.
```

**Error Recovery**:

The parser provides error messages with line numbers and context:

```erlang
{error, {Line, lfe_parse, ["unexpected ", T]}, RestTokens}
```

#### Special Considerations

**No Macro Expansion**:

The parser produces raw s-expressions. Macros are **not** expanded here - that happens in `lfe_macro`.

**Improper List Handling**:

Dotted pairs are represented as two-element tuples:

```lisp
(a . b)  → [cons, a, b]
```

**Tuple vs List**:

- Parentheses: `(a b c)` → List
- Brackets: `[a b c]` → List
- Hash-paren: `#(a b c)` → Tuple

**Quote Simplification**:

Multiple quote forms are preserved:

```lisp
''a     → (quote (quote a))
`',a    → (backquote (comma a))
```

**Performance**:

- O(n) parsing time
- Minimal memory usage
- No backtracking (LL(1) grammar)
