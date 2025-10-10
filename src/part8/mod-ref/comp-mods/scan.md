# lfe_scan.erl - Lexical Scanner

**Purpose**: Convert raw LFE source text into a stream of tokens for parsing.

**Location**: `src/lfe_scan.erl`
**Size**: 897 LOC, 35KB

**Module Classification**: Compiler frontend, lexical analysis

#### Public API

**Primary Scanning Functions**:

```erlang
string(String) -> {ok, Tokens, Line} | {error, ErrorInfo, Line}
string(String, StartLine) -> Result
string(String, StartLine, Options) -> Result
    Tokens = [Token]
    Token = {Type, Line, Value}
    Type = symbol | number | string | binary | '(' | ')' | '[' | ']' | ...
```

Scan a complete string. Located at `lfe_scan.erl:66-68`.

**Incremental Scanning** (for REPL):

```erlang
token(Continuation, Chars) -> {more, Continuation1}
                             | {done, Result, RestChars}
tokens(Continuation, Chars) -> {more, Continuation1}
                              | {done, Result, RestChars}
```

Continuation-based scanning for streaming input. Located at `lfe_scan.erl:45-54`.

#### Token Types

**Delimiters**: `'('`, `')'`, `'['`, `']'`, `'.'`

**Special Syntactic Markers**:

- `'\''` - Quote (`'expr`)
- `'`'` - Backquote (`` `expr ``)
- `','` - Unquote (`,expr`)
- `',@'` - Unquote-splicing (`,@expr`)

**Hash Forms**:

- `'#('` - Tuple literal `#(a b c)`
- `'#.'` - Eval-at-read `#.(+ 1 2)`
- `'#B('` - Binary literal `#B(42 (f 32))`
- `'#M('` - Map literal `#M(a 1 b 2)`
- `'#\''` - Function reference `#'module:function/arity`

**Literals**:

- `symbol` - Atoms and identifiers (`foo`, `foo-bar`, `|complex symbol|`)
- `number` - Integers and floats (`42`, `3.14`, `#2r1010`, `#16rFF`)
- `string` - String literals (`"hello"`, `"""multi\nline"""`)
- `binary` - Binary strings (`#"bytes"`)

#### Token Structure

Each token is a tuple: `{Type, Line, Value}` or `{Type, Line}` for delimiters.

**Examples**:

```erlang
{symbol, 1, foo}
{number, 1, 42}
{string, 2, "hello"}
{'(', 1}
```

#### Special Features

**Triple-Quoted Strings** (`lfe_scan.erl:394-450`):

Supports Python/Elixir-style triple-quoted strings:

```lisp
"""
Multi-line string
with "quotes" inside
"""
```

**Quoted Symbols** (`lfe_scan.erl:350-365`):

Allows arbitrary characters in symbol names:

```lisp
|complex-symbol-name!@#$|
|with spaces|
```

**Based Numbers** (`lfe_scan.erl:473-493`):

Supports bases 2-36:

```lisp
#2r1010      ; Binary
#8r755       ; Octal
#16rDEADBEEF ; Hexadecimal
#36rZZZ      ; Base-36
```

**Character Literals** (`lfe_scan.erl:552-573`):

```lisp
#\a    ; Character 'a'
#\n    ; Newline
#\x41  ; Hex character code
```

**Elixir Module Name Hack** (`lfe_scan.erl:542-549`):

Transforms `#Emodule` → `'Elixir.module'` for Elixir interop:

```lisp
#EEnum  ; Becomes 'Elixir.Enum'
```

#### Comment Handling

**Line Comments** (`lfe_scan.erl:277-282`):

```lisp
; This is a comment
;; Also a comment
```

**Block Comments** (`lfe_scan.erl:283-311`):

```lisp
#|
Block comment
can span multiple lines
|#
```

**Nested Block Comments**: Supported via counter tracking.

#### Internal Structure

**Scanner State**:

```erlang
-record(lfe_scan, {}).  % Currently unused, reserved for future
```

The scanner uses functional continuation passing for incremental parsing rather than explicit state records.

**Key Functions**:

- `scan/3` (line 136): Main dispatch loop
- `scan_symbol/4` (line 236): Symbol scanning
- `scan_number/4` (line 452): Number parsing
- `scan_string/5` (line 375): String literal handling
- `scan_comment/3` (line 277): Comment skipping

#### Dependencies

**Erlang stdlib**:

- `lists` - List operations
- `string` - String manipulation
- `unicode` - UTF-8 handling

**No LFE module dependencies** - Scanner is self-contained.

#### Used By

- `lfe_parse` - Consumes token stream
- `lfe_comp` - Via parse (indirectly)
- `lfe_shell` - For REPL input
- `lfe_io:read*` functions

#### Key Algorithms

**Continuation-Based Scanning** (`lfe_scan.erl:45-54`):

The scanner supports incremental input for REPL use:

```erlang
{more, Continuation}  % Need more input
{done, {ok, Token, Line}, RestChars}  % Token complete
```

This allows reading from a terminal where input arrives character-by-character.

**Symbol Validation** (`lfe_scan.erl:601-645`):

Symbols must:

- Not start with digits (unless quoted)
- Not contain special delimiters (unless quoted)
- Support Unicode characters
- Handle reserved characters in quoted form

**Number Parsing** (`lfe_scan.erl:452-535`):

Supports:

- Decimal integers: `42`, `-17`
- Floats: `3.14`, `1.5e10`, `6.022e23`
- Based integers: `#16rFF`, `#2r1010`
- Sign handling for all numeric forms

#### Special Considerations

**Performance**:

- Scanner is O(n) in input length
- Minimal memory allocation (continuation-based)
- Hot path: symbol scanning (most common token)

**Unicode Support**:

- Full UTF-8 support in strings and comments
- Symbol names can include Unicode characters
- Proper handling of multi-byte sequences

**Error Recovery**:

- Errors include line numbers for reporting
- Invalid characters reported with context
- Unterminated strings/comments detected

**Compatibility**:

- Inherited structure from Erlang's `erl_scan`
- Extended with LFE-specific features (hash forms, quoted symbols)
