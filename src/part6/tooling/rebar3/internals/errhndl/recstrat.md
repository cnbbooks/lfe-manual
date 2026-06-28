# Recovery Strategies

## Compilation Failures

**Strategy**: Fix source code, recompile

**Incremental**: Only failed files recompile

**Clean Build**:

```bash
rebar3 clean
rebar3 compile
```

## Dependency Issues

**Update Registry**:

```bash
rebar3 update
```

**Unlock Dependency**:

```bash
rebar3 unlock DEPNAME
rebar3 compile
```

**Clear Dependency Cache**:

```bash
rm -rf _build/default/lib/DEPNAME
rebar3 compile
```

## Configuration Errors

**Validate Syntax**:

```bash
erl -eval "file:consult(\"rebar.config\")." -noshell -s init stop
```

**Check Profile Format**:

```erlang
% Correct:
{profiles, [{test, [{deps, [meck]}]}]}.

% Incorrect:
{profiles, [{test, {deps, [meck]}}]}.  % Not a list!
```

## DAG/Cache Issues

**Clear DAG Cache**:

```bash
rm -rf _build/default/.rebar3/
rebar3 compile
```

**Clear All Caches**:

```bash
rm -rf _build/
rm -rf ~/.cache/rebar3/
rebar3 compile
```

## Network Issues

**Offline Mode** (use only cached):

```bash
rebar3 compile --offline
```

**Retry with Timeout**:

```erlang
% In ~/.config/rebar3/rebar.config:
{hex, [{http_timeout, 300000}]}.  % 5 minutes
```
