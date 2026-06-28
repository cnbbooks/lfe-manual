# lfe_edlin_expand.erl - Command Line Expansion

**Purpose**: Provide tab completion and expansion for the LFE shell.

**Location**: `src/lfe_edlin_expand.erl`
**Size**: 232 LOC, 7.8KB

**Module Classification**: Shell support, command completion

#### Public API

```erlang
expand(Before) -> {yes, Expansion, Matches} | no
```

Expand command-line input. Located at `lfe_edlin_expand.erl:42-45`.

**Parameters**:

- `Before` - String before cursor
- **Returns**:
  - `{yes, Expansion, Matches}` - Possible expansions
  - `no` - No completions available

#### Completion Types

**Module Name Completion**:

```lisp
lfe> (lists:m<TAB>
→ lists:map  lists:max  lists:member  ...
```

**Function Name Completion**:

```lisp
lfe> (list<TAB>
→ lists  list-comprehension  ...
```

**Variable Name Completion**:

```lisp
lfe> my-var<TAB>
→ my-variable-name
```

**Local Bindings**: Completes from current environment.

#### Implementation

**Tokenization** (`expand/1` at lines 47-89):

1. Parse `Before` string into tokens
2. Identify last token (completion target)
3. Determine context (module, function, variable)
4. Find matches
5. Return common prefix + alternatives

**Module/Function Matching** (`match_module_functions/2` at lines 134-178):

```erlang
match_module_functions(Prefix, Env) ->
    % Get all loaded modules
    Modules = code:all_loaded(),
    % Get module exports
    Exports = [Mod:module_info(exports) || {Mod, _} <- Modules],
    % Filter by prefix
    filter_matches(Prefix, flatten(Exports)).
```

**Local Binding Matching** (`match_local_bindings/2` at lines 182-214):

Searches current environment for matching variable names.

#### Dependencies

**LFE modules**:

- `lfe_env` - Access to bindings
- `lfe_scan` - Tokenization

**Erlang stdlib**:

- `code`, `lists`, `string`

#### Used By

- `lfe_shell` - Via `io:setopts([{expand_fun, Fun}])`

#### Special Considerations

**Performance**: Completion must be fast (< 100ms) for responsive UX.

**Context Sensitivity**: Understands LFE syntax to provide relevant completions.

**Module Loading**: Only completes for loaded modules (doesn't search filesystem).
