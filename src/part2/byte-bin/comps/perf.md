# Performance Considerations

Binary comprehensions compile to efficient code, but there are still considerations:

1. **Memory usage**: Each intermediate binary allocation takes memory. For very large data, consider streaming approaches.

2. **Avoid excessive copying**: Binary manipulation in Erlang/LFE is optimized for append operations. Prepending or inserting in the middle is expensive.

3. **Use binary comprehensions for transformation, not iteration**: If you're just checking conditions without producing output, use regular recursion or `lists:foldl`.
