# Compiler Architecture

## Compiler Behavior

All compilers implement the `rebar_compiler` behavior:

```erlang
-callback context(rebar_app_info:t()) -> Context :: map().
-callback needed_files(Graph, FoundFiles, Mappings, AppInfo) -> NeededFiles.
-callback dependencies(Source, SourceDir, InDirs) -> [Dependency].
-callback dependencies(Source, SourceDir, InDirs, DepOpts) -> [Dependency].
-callback compile(Source, Mappings, Config, Opts) -> Result.
-callback compile_and_track(Source, Mappings, Config, Opts) -> TrackedResult.
-callback clean(Files, AppInfo) -> ok.
```

## Compiler Context

The `context/1` callback provides configuration:

```erlang
#{src_dirs => ["src", "lib"],           % Where to find sources
  include_dirs => ["/abs/path/include"], % Include directories
  src_ext => ".erl",                     % Source extension
  out_mappings => [{".beam", "ebin"}],   % Output extension → directory
  dependencies_opts => [...]}            % Options for dependency scanning
```

## Needed Files

The `needed_files/4` callback determines what to compile:

**Returns**:

```erlang
{{FirstFiles, FirstOpts},    % Must compile first (sequentially)
 {RestFiles, RestOpts}}      % Can compile after FirstFiles
```

**or**:

```erlang
{{FirstFiles, FirstOpts},
 {{Sequential, Parallel}, RestOpts}}  % Split for parallelization
```

**FirstFiles** typically include:

- Parse transforms
- Files listed in `erl_first_files` configuration

**Sequential** files have incoming dependencies (other files depend on them)

**Parallel** files are independent and can compile concurrently
