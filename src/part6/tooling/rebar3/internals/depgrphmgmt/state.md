# State Modification

## DAG Internal State

**Dirty Bit**: Special vertex `'$r3_dirty_bit'`

- Added when DAG modified
- Checked before saving
- Cleared after save

**Modification Operations**:

- Adding/updating vertices
- Adding/deleting edges
- Pruning files
- Storing artifacts

## Persistence

**Saved Data**:

- All vertices with labels (files + timestamps/metadata)
- All edges with labels (dependencies + artifacts)
- Version number (currently 4)
- Critical metadata

**Not Saved**:

- Dirty bit
- Transient state
