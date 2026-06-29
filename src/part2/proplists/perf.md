# Performance Considerations

Proplists are lists, which means their performance characteristics are list characteristics:

* **Lookup**: O(n) — must traverse from the head until finding the key
* **Multiple lookups**: O(n*m) where m is the number of lookups
* **Deletion**: O(n) — must traverse the entire list
* **Insertion**: O(1) at the head (via `cons`), but this doesn't help if you need to update a value

For small option lists (fewer than ~20 entries), this doesn't matter. For larger collections or when lookup performance is critical, use maps or consider:

* Converting to a map once and using `maps` functions thereafter
* Using `proplists:to_map/1` at the boundary between external and internal representations
* Caching frequently accessed values rather than repeatedly calling `get_value/2`

The rule of thumb: if you're calling `get_value/2` in a loop or frequently within a hot path, you're probably using the wrong data structure.
