# Common Errors

## Compilation Errors

**Syntax Error**:

```
Compiling src/my_module.erl failed
src/my_module.erl:10: syntax error before: '}'
```

**Missing Include**:

```
src/my_module.erl:5: can't find include file "missing.hrl"
```

**Undefined Function**:

```
src/my_module.erl:15: function foo/1 undefined
```

## Dependency Errors

**Circular Dependencies**:

```
Dependency cycle(s) detected:
applications: app1 app2 app3 depend on each other
```

**Missing Package**:

```
Package not found in registry: nonexistent-1.0.0
```

**Bad Constraint**:

```
Unable to parse version for package jsx: ~~>3.1.0
```

## Configuration Errors

**Invalid rebar.config**:

```
Error reading config file rebar.config: {Line, erl_parse, ["syntax error before: ", "']'"]}
```

**Invalid Profile**:

```
Profile config must be a list but for profile 'test' config given as: {test, value}
```

## Application Errors

**Invalid App Name**:

```
Invalid ebin/my_app.app: name of application (wrong_name) must match filename.
```

**Missing Module**:

```
Module my_module listed in my_app.app but beam file not found
```

## DAG/Build Errors

**DAG Restoration Failure**:

```
Failed to restore _build/default/.rebar3/rebar_compiler_erl/source.dag file. Discarding it.
```

(Warning only, triggers rebuild)

**Missing Artifact**:

```
Missing artifact path/to/expected/file.beam
```
