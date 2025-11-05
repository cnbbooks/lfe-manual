# DAG Structure

## Vertices

**Three Types**:

1. **Source Files**: `.erl`, `.yrl`, `.xrl`, `.mib`
   - Label: `LastModified` (timestamp)
   - Example: `{"/path/to/file.erl", 1234567890}`

2. **Header/Include Files**: `.hrl`, other included files
   - Label: `LastModified` (timestamp)
   - Example: `{"/path/to/file.hrl", 1234567890}`

3. **Artifact Files**: `.beam`, `.bin`, compiled outputs
   - Label: `{artifact, Metadata}`
   - Metadata includes: compiler options, compiler version
   - Example: `{"/path/to/file.beam", {artifact, [{compiler_version, "8.0"}, ...]}}`

## Edges

**Two Types**:

1. **Dependency Edges**: Source/header → dependency
   - Label: `[]` (empty list)
   - Direction: File points TO what it depends ON
   - Example: `my_module.erl → types.hrl`

2. **Artifact Edges**: Artifact → source
   - Label: `artifact` (atom)
   - Direction: Artifact points TO source that created it
   - Example: `my_module.beam → my_module.erl`

## Special Vertex

**Dirty Bit**: `'$r3_dirty_bit'`

- Presence indicates DAG has been modified
- Used to determine if save is needed
- Not a real file
