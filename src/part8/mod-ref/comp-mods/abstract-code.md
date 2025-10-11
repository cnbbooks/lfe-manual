# lfe_abstract_code.erl - Debug Info Provider

**Purpose**: Provide debug_info callback for Erlang tools (Dialyzer, debugger, cover, xref).

**Location**: `src/lfe_abstract_code.erl`
**Size**: 51 LOC, 1.7KB

**Module Classification**: Integration, tool support

#### Public API

```erlang
debug_info(Format, Module, Data, Options) ->
    {ok, Result}
  | {error, Reason}
```

Erlang compiler callback for debug information. Located at `lfe_abstract_code.erl:37-52`.

#### Supported Formats

**`erlang_v1`** - Erlang Abstract Format:

Returns the LFE-generated Erlang AST directly.

**`core_v1`** - Core Erlang:

Converts AST to Core Erlang using `compile:noenv_forms/2`.

#### Implementation

```erlang
debug_info(_Format, _Module, {none, _CompOpts}, _Opts) ->
    {error, missing};  % No debug info stored

debug_info(erlang_v1, _Mod, {AbstrCode, _CompilerOpts}, _Opts) ->
    {ok, AbstrCode};  % Return AST as-is

debug_info(core_v1, _Mod, {AbstrCode, CompilerOpts}, Opts) ->
    % Convert to Core Erlang
    CoreOpts = add_core_returns(delete_reports(CompilerOpts ++ Opts)),
    try compile:noenv_forms(AbstrCode, CoreOpts) of
        {ok, _, Core, _} -> {ok, Core};
        _Error -> {error, failed_conversion}
    catch
        error:_E -> {error, failed_conversion}
    end;

debug_info(_Format, _, _Data, _) ->
    {error, unknown_format}.  % Unknown format requested
```

#### Storage in BEAM

**Debug Info Chunk**:

When compiling with debug info, LFE stores:

```erlang
{debug_info, {lfe_abstract_code, {AbstractCode, CompilerOpts}}}
```

This is stored in the BEAM file's debug_info chunk.

#### Tool Integration

**Dialyzer**:

Requests `erlang_v1` or `core_v1` format for type analysis:

```erlang
{ok, Core} = lfe_abstract_code:debug_info(core_v1, Mod, DebugInfo, [])
% Dialyzer analyzes Core Erlang for types
```

**Debugger**:

Requests `erlang_v1` for step debugging:

```erlang
{ok, AST} = lfe_abstract_code:debug_info(erlang_v1, Mod, DebugInfo, [])
% Debugger uses AST for breakpoints, stepping
```

**Cover (Code Coverage)**:

Requests `erlang_v1` for coverage analysis:

```erlang
{ok, AST} = lfe_abstract_code:debug_info(erlang_v1, Mod, DebugInfo, [])
% Cover instruments AST for coverage tracking
```

#### Dependencies

**Erlang compiler**:

- `compile:noenv_forms/2` - Core Erlang conversion

#### Used By

- Dialyzer
- Erlang debugger
- Cover
- Xref
- Any tool querying debug_info

#### Special Considerations

**Compile Options**:

To include debug info:

```bash
lfec +debug_info mymodule.lfe
```

Without `+debug_info`, tools cannot analyze the module.

**Format Evolution**:

If Erlang adds new debug_info formats, this module needs updates.

**Security**:

Debug info includes source-level information. Strip it for production releases:

```bash
lfec mymodule.lfe  # No debug info by default
```
