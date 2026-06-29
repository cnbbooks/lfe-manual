# Maps vs. Dicts and Other Erlang Data Structures

Maps effectively obsolete several older dictionary implementations:

* **dict**: The original ordered dictionary module. Maps are faster and have native syntax.
* **orddict**: Ordered dictionary as a sorted list. Maps provide similar ordering with better performance.
* **gb_trees**: General balanced trees. Use these only if you need minimum/maximum key operations frequently.

For almost all associative data needs, maps are now the default choice, much like how smartphones rendered pocket calculators quaint.
