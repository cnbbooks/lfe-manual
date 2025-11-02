# lfe_error.erl - Error Formatting

**Purpose**: Format exceptions and errors for user-friendly display.

**Location**: `src/lfe_error.erl`
**Size**: 171 LOC, 5.7KB

**Module Classification**: Utilities, error handling

#### Public API

```erlang
format_exception(Class, Reason, StackTrace) -> Chars
format_exception(Class, Reason, StackTrace, SkipFun, FormatFun, Indentation) -> Chars
```

Format exception with stack trace. Located at `lfe_error.erl:39-52`.

**Parameters**:

- `Class` - `error`, `throw`, or `exit`
- `Reason` - Exception reason
- `StackTrace` - List of stack frames
- `SkipFun` - Function to filter stack frames
- `FormatFun` - Function to format terms
- `Indentation` - Initial indentation level

#### Exception Formatting

**Output Format**:

```
exception error: badarg
  in (lfe_eval:eval_expr/2 line 345)
  in (lfe_shell:eval_form/2 line 502)
```

**Stack Trace Filtering** (`filter_stack/2` at lines 87-112):

Allows skipping internal frames:

```erlang
SkipFun = fun(Module, _Function, _Arity) ->
    Module == lfe_eval orelse Module == lfe_shell
end
```

**Term Formatting** (`format_term/3` at lines 134-156):

Custom formatting for exception data:

```erlang
FormatFun = fun(Term, Indent) ->
    lfe_io:prettyprint1(Term, 15, Indent, 80)
end
```

#### Exception Classes

**Error** (runtime errors):

```lisp
exception error: badarg
```

**Throw** (user throws):

```lisp
exception throw: my_exception
```

**Exit** (process exits):

```lisp
exception exit: normal
```

#### Stack Frame Format

**Erlang Stack Frame**:

```erlang
{Module, Function, Arity, [{file, File}, {line, Line}]}
```

**Formatted**:

```
in (Module:Function/Arity line Line)
in (Module:Function/Arity File:Line)
```

#### Dependencies

**LFE modules**:

- `lfe_io` - Term formatting

**Erlang stdlib**:

- `lists`, `string`

#### Used By

- `lfe_shell` - Exception reporting
- `lfe_init` - Script error reporting
- `lfescript` - Script errors

#### Special Considerations

**User-Friendly**: Formats for human readability, not machine parsing.

**Filtering**: Can hide internal implementation details from users.

**Customizable**: Format function allows LFE-style or Erlang-style output.
