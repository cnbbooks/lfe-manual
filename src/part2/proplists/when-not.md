# When Not to Use Proplists

Proplists are not appropriate for:

* **Large collections**: Anything over ~50 entries should probably be a map
* **Frequent updates**: Use maps or records
* **Schema-validated data**: Use records with well-defined fields
* **High-performance lookups**: Use maps, ETS tables, or process dictionaries (though the last should be used with the sort of caution normally reserved for handling radioactive materials)
* **Data with complex types as keys**: Use maps, which support any fully ground term as a key
