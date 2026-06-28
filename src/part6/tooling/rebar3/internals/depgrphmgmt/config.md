# Configuration

## No Direct Configuration

DAGs are managed automatically. However, behavior affected by:

**DAG Version**: `?DAG_VSN = 4`

- Hardcoded in `rebar_compiler_dag.erl`
- Change invalidates all cached DAGs

**Critical Metadata**:

- For `rebar_compiler_erl`: includes compiler version
- Mismatch triggers rebuild
