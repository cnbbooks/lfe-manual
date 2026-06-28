# Error Patterns

## Provider Error Macro

**Definition**:

```erlang
-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).
```

**Usage**:

```erlang
throw(?PRV_ERROR({missing_file, Filename}))
```

**Format Error Callback**:

```erlang
format_error({missing_file, Filename}) ->
    io_lib:format("File not found: ~ts", [Filename]).
```

## Error Tuple Returns

**Pattern**:

```erlang
-spec do(State) -> {ok, NewState} | {error, Reason}.
```

**Providers** return:

- `{ok, State}`: Success
- `{error, Reason}`: Failure

## Throws vs Returns

**Throws**: Immediate abortion

```erlang
throw(?PRV_ERROR(Reason))
```

**Returns**: Allow caller to decide

```erlang
{error, Reason}
```
