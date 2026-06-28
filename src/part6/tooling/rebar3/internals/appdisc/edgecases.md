# Edge Cases

## Hidden Directories

**Behavior**: Directories starting with `.` are ignored

**Example**: `.git/`, `.rebar3/` not scanned

---

## Symbolic Links

**Behavior**: Followed during directory scanning

**Use Case**: Link to shared libraries

**Warning**: Can cause duplicates if not careful

---

## Mixed Erlang/Elixir Projects

**Scenario**: Both `.app.src` and `mix.exs` in same directory

**Behavior**: Prefers `.app` > `.app.src.script` > `.app.src` > `mix.exs`

---

## Application Without rebar.config

**Behavior**: Uses parent/state configuration

**Common**: Dependencies often don't have own config

---

## Disabled Applications

**Configuration**: Use `enable` option

**Example**:

```erlang
{app_name, [
    {enable, false}
]}.
```

**Behavior**: Application discovered but not built
