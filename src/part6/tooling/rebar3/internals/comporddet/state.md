# State Modification

## No Direct State Modifications

This stage is purely computational - it doesn't modify state directly. Instead:

- Takes input list of applications
- Returns reordered list
- Calling code updates state with sorted list

## Where Sorted Lists Are Stored

**For Dependencies**:

- Returned from `find_cycles/1`
- Stored in temporary variable
- Passed to `cull_compile/2`
- Final result stored as `deps_to_build` in state

**For Project Apps**:

- Returned from `compile_order/1`
- Used immediately for compilation
- Not permanently stored in state
