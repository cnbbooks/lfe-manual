# lfe_io.erl - I/O Interface

**Purpose**: Unified interface for reading and writing LFE S-expressions with delegation to specialized formatters.

**Location**: `src/lfe_io.erl`
**Size**: 275 LOC, 9.6KB

**Module Classification**: I/O core, API facade

#### Public API

**File Reading**:

```erlang
parse_file(FileName) -> {ok, [{Sexpr, Line}]} | {error, Error}
parse_file(FileName, StartLine) -> Result
```

Parse file returning sexprs with line numbers. Located at `lfe_io.erl:72-77`.

```erlang
read_file(FileName) -> {ok, [Sexpr]} | {error, Error}
read_file(FileName, StartLine) -> Result
```

Read file returning sexprs only. Located at `lfe_io.erl:98-103`.

**Interactive Reading**:

```erlang
read() -> {ok, Sexpr} | {error, Error} | eof
read(Prompt) -> Result
read(IoDevice, Prompt) -> Result
```

Read single sexpr with optional prompt. Located at `lfe_io.erl:146-154`.

```erlang
read_line() -> {ok, Sexpr} | {error, Error} | eof
read_line(Prompt) -> Result
read_line(IoDevice, Prompt) -> Result
```

Line-oriented reading (for REPL). Located at `lfe_io.erl:165-182`.

**String Reading**:

```erlang
read_string(String) -> {ok, [Sexpr]} | {error, Error}
```

Parse string into sexprs. Located at `lfe_io.erl:221-226`.

**Writing**:

```erlang
print(Sexpr) -> ok
print(IoDevice, Sexpr) -> ok
print1(Sexpr) -> [char()]
print1(Sexpr, Depth) -> [char()]
```

Compact printing. Delegates to `lfe_io_write`. Lines 233-237.

```erlang
prettyprint(Sexpr) -> ok
prettyprint(IoDevice, Sexpr) -> ok
prettyprint1(Sexpr) -> [char()]
prettyprint1(Sexpr, Depth, Indentation, LineLength) -> [char()]
```

Pretty printing. Delegates to `lfe_io_pretty`. Lines 244-253.

**Formatted Output**:

```erlang
format(Format, Args) -> ok
format(IoDevice, Format, Args) -> ok
format1(Format, Args) -> [char()]
```

Formatted output (LFE's printf). Delegates to `lfe_io_format`. Lines 261-273.

#### Reading Architecture

**Token-Based Reading** (`with_token_file/3` at lines 123-138):

```erlang
with_token_file(File, DoFunc, Line) ->
    % 1. Open file
    {ok, Fd} = file:open(File, [read]),
    % 2. Scan tokens
    {ok, Tokens, LastLine} = io:request(Fd, {get_until, lfe_scan, tokens, [Line]}),
    % 3. Apply processing function
    Result = DoFunc(Tokens, LastLine),
    % 4. Close file
    file:close(Fd),
    Result.
```

**Incremental Scanning** (`scan_sexpr/2` at lines 192-216):

Continuation-based scanning for interactive use:

```erlang
scan_sexpr({ScanCont, ParseCont}, Chars, Line) ->
    case lfe_scan:token(ScanCont, Chars, Line) of
        {done, {ok, Token, Line1}, Rest} ->
            % Got token, try parsing
            case lfe_parse:sexpr(ParseCont, [Token]) of
                {ok, _, Sexpr, _} -> {done, {ok, Sexpr}, Rest};
                {more, ParseCont1} -> scan_sexpr_1([], ParseCont1, Rest, Line1)
            end;
        {more, ScanCont1} ->
            {more, {ScanCont1, ParseCont}}
    end.
```

#### Dependencies

**LFE modules**:

- `lfe_scan` - Lexical scanning
- `lfe_parse` - Parsing
- `lfe_io_write` - Compact writing
- `lfe_io_pretty` - Pretty printing
- `lfe_io_format` - Formatted output

**Erlang stdlib**:

- `file`, `io`, `unicode`

#### Used By

- `lfe_comp` - File reading
- `lfe_shell` - REPL I/O
- `lfe_eval` - Reading expressions
- User code - Public API

#### Special Considerations

**Line Numbering**: Parse functions preserve line numbers for error reporting.

**Unicode**: Full UTF-8 support in reading and writing.

**History**: The `get_line/2` function integrates with Erlang's line history for the REPL.

**Facade Pattern**: This module doesn't implement formatting itself; it delegates to specialized modules.
