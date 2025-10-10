# Stage 1: Lexical Analysis (lfe_scan.erl)

**Purpose**: Convert source text into a token stream.

**Module**: `lfe_scan.erl` (897 LOC)

**Entry point**:

```erlang
string(String, StartLine) -> {ok, Tokens, LastLine} | {error, Error, LastLine}
string(String, StartLine, Options) -> {ok, Tokens, LastLine} | {error, Error, LastLine}
```

**Token format**:

```erlang
Token :: {TokenType, Line} | {TokenType, Line, Value}

TokenType ::
    '(' | ')' | '[' | ']' | '.' |           % Delimiters
    '\'' | '`' | ',' | ',@' |                % Quote operators
    '#(' | '#.' | '#\'' | '#B(' | '#M(' |   % Hash forms
    symbol | number | string | binary        % Literals
```

**Example transformation**:

```lisp
;; Input
(defun hello (name)
  (io:format "Hello, ~s!~n" (list name)))

;; Tokens produced
[{'(', 1},
 {symbol, 1, defun},
 {symbol, 1, hello},
 {'(', 1},
 {symbol, 1, name},
 {')', 1},
 {'(', 2},
 {':', 2},
 {symbol, 2, io},
 {symbol, 2, format},
 {string, 2, "Hello, ~s!~n"},
 {'(', 2},
 {symbol, 2, list},
 {symbol, 2, name},
 {')', 2},
 {')', 2},
 {')', 2}]
```

**Special features**:

1. **Triple-quoted strings**: `"""multi\nline"""` (lines 450-500)
2. **Sigils**: `#"binary string"`, `#B(binary)`, `#M(map)`
3. **Based numbers**: `#2r1010` (binary), `#16rFF` (hex), `#8r777` (octal)
4. **Character literals**: `#\a`, `#\newline`
5. **Quoted symbols**: `|complex-symbol-name|`
6. **Elixir module hack**: `#Emodule` → `'Elixir.module'`

**Scanner state**:

```erlang
-record(lfe_scan, {
    % Currently unused, reserved for future stateful scanning
}).
```

The scanner is **continuation-based** (like Erlang's `erl_scan`), supporting incremental parsing:

```erlang
token(Cont, Chars) ->
    {done, Result, Rest} | {more, Continuation}
```

This enables:

- REPL interaction (partial input buffering)
- Streaming compilation
- Error recovery
