# Edge Cases

## Conflicting Dependency Versions

**Scenario**: Two dependencies require different versions of the same package

**Behavior**:

- First encountered version wins
- Later conflicts are warned about but skipped
- Lock file preserves the chosen version

**Warning**:

```
Skipping jsx (git source) as an app of the same name has already been fetched
```

**Solution**:

- Check if all dependencies can work with one version
- May need to upgrade/downgrade parent dependencies

---

## Dependency at Multiple Levels

**Scenario**: Same dependency appears at different depths

**Example**:

```
my_app → dep_a → dep_common (level 2)
my_app → dep_b → dep_common (level 2)
my_app → dep_common (level 1)
```

**Behavior**:

- First occurrence wins (usually shallowest level)
- Lock file records the shallowest level
- Deeper occurrences skipped

---

## Upgrade Mode

**Triggered by**: `rebar3 upgrade`

**Behavior**:

- Ignore lock file constraints
- Fetch latest versions matching constraints
- Update lock file with new versions

**Effect on Warnings**:

- Suppresses "already seen" warnings
- Shows upgrade activity

---

## Checkout Dependencies

**Location**: `_checkouts/dep_name/`

**Behavior**:

- Always used, regardless of lock file
- Always compiled (never treated as binary)
- Never added to lock file
- Overrides any version specification

**Use Cases**:

- Local development on a dependency
- Testing unreleased dependency changes
- Temporary patches

**Warning**:

```
App my_dep is a checkout dependency and cannot be locked.
```

---

## Empty Lock File

**Behavior**: All dependencies resolved from scratch

**When This Happens**:

- First time running rebar3
- After deleting `rebar.lock`
- In a new clone of the repository (if lock file not committed)

---

## Profile Dependencies Don't Lock

**Behavior**: Only default profile dependencies are locked

**Reason**: Different profiles used in different contexts

**Example**:

```erlang
{profiles, [
    {test, [{deps, [meck]}]}
]}.
```

**Running**: `rebar3 as test compile`

**Result**: `meck` resolved but NOT added to `rebar.lock`

---

## Transitive Dependency Version Pinning

**Scenario**: Lock file pins version of transitive dependency

**Example**:

```erlang
% rebar.config
{deps, [{cowboy, "2.9.0"}]}.

% rebar.lock includes:
{<<"ranch">>, {git, "...", {ref, "..."}}, 1}
```

**Behavior**:

- Even though ranch is not direct dependency, it's locked
- Ensures exact same version tree on every build
- Provides true reproducibility
