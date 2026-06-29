# Guidelines for Proplist Usage

Based on collective wisdom accumulated over decades of Erlang development:

1. **Use proplists for function options** unless the options are numerous (>20) or performance-critical
2. **Always provide default values** using either `get_value/3` or the append-defaults pattern
3. **Document expected options** clearly, especially boolean shorthands
4. **Consider maps for new APIs** but don't feel compelled to break compatibility
5. **Use normalization** when accepting user input that might use variant keys
6. **Validate required options** early in your function, not deep in the logic
7. **Don't nest proplists deeply**—if you find yourself doing `get_value` on a `get_value` result, you probably want records or maps
8. **Remember that atoms as booleans must be lowercase**—`'True` and `'true` are different atoms
