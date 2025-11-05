# Custom Compilers

## Compiler Behavior

Implement `rebar_compiler` behavior (see [Source File Compilation](/part6/tooling/rebar3/internals/srcflcomp)):

```erlang
-module(my_compiler).
-behaviour(rebar_compiler).

-export([context/1, needed_files/4, dependencies/3, compile/4, clean/2]).

context(AppInfo) ->
    #{src_dirs => ["priv/templates"],
      include_dirs => [],
      src_ext => ".template",
      out_mappings => [{".html", "priv/static"}],
      dependencies_opts => []}.

needed_files(Graph, Files, Mappings, AppInfo) ->
    % Determine which files need compilation
    {{[], []}, {Files, []}}.

dependencies(Source, SourceDir, InDirs) ->
    % Return list of dependencies
    [].

compile(Source, Mappings, Config, Opts) ->
    % Compile source to target
    % Return: ok | {ok, Warnings} | error | {error, Errors, Warnings}
    ok.

clean(Files, AppInfo) ->
    % Clean compiled artifacts
    ok.
```

## Register Custom Compiler

**In Plugin**:

```erlang
init(State) ->
    State1 = rebar_state:prepend_compilers(State, [my_compiler]),
    {ok, State1}.
```

**Compiler Order**:

- Custom compilers can be prepended or appended
- Prepend: Run before built-in compilers (for source generation)
- Append: Run after built-in compilers (for final artifacts)
