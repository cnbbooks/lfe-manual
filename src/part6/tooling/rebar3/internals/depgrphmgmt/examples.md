# Example Scenarios

## Scenario 1: Initial Build

**State**: No DAG file exists

**Flow**:

1. `init/4`: Create empty DAG
2. `populate_sources/5`: Add all sources
   - All files new
   - Scan all dependencies
   - Build complete graph
3. `propagate_stamps/1`: Propagate timestamps
4. Compilation proceeds
5. `store_artifact/4`: Track each `.beam`
6. `maybe_store/5`: Save DAG to disk

**Result**: Full compilation, DAG saved

---

## Scenario 2: Incremental Build (No Changes)

**State**: Valid DAG exists, no files modified

**Flow**:

1. `init/4`: Restore DAG from disk
2. `populate_sources/5`: Check all sources
   - All timestamps match
   - No scans needed
3. `propagate_stamps/1`: Skip (not dirty)
4. `needed_files/4`: No files need compilation
5. Compilation skipped
6. `maybe_store/5`: Skip save (not dirty)

**Result**: No compilation, instant completion

---

## Scenario 3: Single File Modified

**Files**:

```
src/mod_a.erl (modified)
src/mod_b.erl (unchanged)
src/mod_c.erl (unchanged, includes mod_a.hrl)
include/mod_a.hrl (unchanged)
```

**DAG**:

```
mod_a.erl (timestamp updated)
mod_b.erl → ...
mod_c.erl → mod_a.hrl
mod_a.hrl (unchanged)
```

**Flow**:

1. `populate_sources/5`: Detect mod_a.erl changed
2. Update vertex timestamp
3. `propagate_stamps/1`: No propagation (mod_a.hrl unchanged)
4. Compilation: Only mod_a.erl
5. `store_artifact/4`: Update mod_a.beam
6. Save DAG

**Result**: One file compiled

---

## Scenario 4: Header File Modified

**Files**:

```
include/types.hrl (modified)
src/mod_a.erl → types.hrl
src/mod_b.erl → types.hrl
src/mod_c.erl (independent)
```

**Flow**:

1. `populate_deps/3`: Refresh types.hrl timestamp
2. `propagate_stamps/1`:
   - types.hrl timestamp: 100 (new)
   - mod_a.erl depends on types.hrl
   - Update mod_a.erl timestamp: 100
   - mod_b.erl depends on types.hrl
   - Update mod_b.erl timestamp: 100
   - mod_c.erl independent: unchanged
3. Compilation: mod_a.erl, mod_b.erl
4. Save DAG

**Result**: Two files compiled

---

## Scenario 5: Transitive Dependencies

**Files**:

```
include/base.hrl (modified)
include/types.hrl → base.hrl
src/mod_a.erl → types.hrl
```

**Flow**:

1. Refresh base.hrl: timestamp 100
2. Propagate:
   - types.hrl depends on base.hrl: update to 100
   - mod_a.erl depends on types.hrl: update to 100
3. Compilation: mod_a.erl

**Result**: Change propagates through chain

---

## Scenario 6: Compiler Options Changed

**Initial**:

```erlang
{erl_opts, [debug_info]}.
```

**DAG Metadata**: `[{compiler_version, "8.0"}, {options, [debug_info]}]`

**Changed**:

```erlang
{erl_opts, [debug_info, inline]}.
```

**Flow**:

1. `init/4`: Restore DAG
2. Check critical metadata
3. New options: `[debug_info, inline]`
4. Mismatch detected (in compiler, not DAG module)
5. All files marked for recompilation
6. DAG updated with new metadata
7. Save DAG

**Result**: Full rebuild with new options

---

## Scenario 7: Inter-App Dependencies

**Apps**:

```
app_common: no deps
app_api: depends on app_common (parse_transform)
app_web: depends on app_api
```

**DAG** (file-level):

```
app_api/src/api_mod.erl → app_common/src/my_transform.erl
app_web/src/web_mod.erl → app_api/include/api.hrl
```

**Flow**:

1. `compile_order/4`: Build app-level DAG
2. Resolve file deps to apps:
   - api_mod.erl in app_api
   - my_transform.erl in app_common
   - Add edge: app_api → app_common
3. Similar for app_web → app_api
4. Interleave sort
5. Result: `[app_common, app_api, app_web]`

**Result**: Apps compiled in correct order

---

## Scenario 8: DAG Version Upgrade

**Scenario**: rebar3 upgraded, DAG version changes from 3 to 4

**Flow**:

1. `init/4`: Try restore DAG
2. Read file: `#dag{vsn = 3, ...}`
3. Version mismatch: 3 ≠ 4
4. `status/4` returns `bad_vsn`
5. Delete old DAG file
6. Create new empty DAG
7. Full rebuild

**Result**: Clean rebuild after upgrade
