# State Modification

## No Direct State Modifications

Compilation doesn't modify rebar state directly. Changes are to:

- File system (creates `.beam` files, etc.)
- DAGs (updated and saved to disk)
- App info records (may be updated with compilation metadata)

## DAG Updates

For each compiled file:

- Add/update artifact vertices in DAG
- Store artifact metadata (options, compiler version)
- Update timestamps
- Save DAG to `_build/PROFILE/.rebar3/COMPILER/source[_LABEL].dag`
