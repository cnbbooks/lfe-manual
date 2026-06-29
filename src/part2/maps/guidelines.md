# Practical Guidelines

Here's the accumulated wisdom for working with maps in LFE:

1. **Use `=>` for initial key creation, `:=` for updates**. This catches typos and signals intent.

2. **Use maps for options** when they're mostly pairs. The `maps:merge/2` pattern for defaults is elegant.

3. **Use records for known, fixed structures** where performance matters. Process loop state, for instance.

4. **Pattern match freely**. You needn't match all keys—maps don't mind selective attention.

5. **Remember that maps are ordered**. This makes their behavior predictable and their output deterministic.

6. **Consider maps for any associative data** unless you have specific reasons (like needing min/max operations frequently) to use something else.
