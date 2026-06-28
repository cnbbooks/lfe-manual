# Edge Cases

## Empty ebin Directory

**Scenario**: No `.beam` files compiled yet

**Behavior**: `modules` list will be empty `[]`

**Valid**: For library applications without modules

---

## .app.src.script Evaluation

**File**: `.app.src.script`

**Content**: Erlang code that returns application term

**Example**:

```erlang
%% .app.src.script
Env = os:getenv("BUILD_ENV"),
Vsn = case Env of
    "prod" -> "1.0.0";
    _ -> "dev"
end,
{application, my_app, [
    {vsn, Vsn},
    {description, "My App"}
]}.
```

**Evaluation**: Executed as Erlang code, result used

---

## Extra Src Dirs Modules

**Configuration**:

```erlang
{extra_src_dirs, ["test"]}.
```

**Behavior**:

- Modules in `test/` compiled to separate location
- Not included in main `.app` modules list
- Prevents test modules from being included in releases

---

## Git Describe Format

**Command**: `git describe --tags`

**Possible Outputs**:

- `"v1.2.3"`: Exact tag
- `"v1.2.3-5-gabc123"`: 5 commits after tag, commit abc123
- `"abc123"`: No tags, just commit SHA

**Handling**: rebar3 parses and formats appropriately

---

## Missing Version in .app.src

**Scenario**: No `vsn` field in `.app.src`

**Error**: `Failed to get app value 'vsn' from 'src/my_app.app.src'`

**Required**: vsn must be present

---

## Module List with Placeholder

**Common Pattern**:

```erlang
{modules, []}  % Will be filled automatically
```

**Also Valid**:

```erlang
{modules, modules}  % Placeholder atom
```

Both replaced with actual module list
