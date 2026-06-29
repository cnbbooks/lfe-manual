# Proplists vs. Maps: A Philosophical Comparison

The arrival of maps didn't obsolete proplists, though it did raise uncomfortable questions about their continued existence. Maps offer:

* **Native syntax**: `#M(key value)` vs. `'((key value))`
* **Efficient access**: O(log n) vs. O(n) for `get_value/2`
* **Pattern matching**: Clean destructuring in function heads
* **Type safety**: The `:=` operator catches typos

Yet proplists retain certain advantages:

* **Simplicity**: They're just lists. No special syntax, no compiler involvement.
* **Legacy compatibility**: Decades of Erlang code uses proplists for options.
* **Literal options**: `[read, write, append]` looks cleaner than `#{read => true, write => true, append => true}`
* **Order-based overriding**: Cons new options onto the front to override defaults

The general wisdom, as articulated by the ancients (and by the Learn You Some Erlang documentation), is this:

* Use **maps** for options that are mostly pairs, especially in new code. The `maps:merge/2` pattern for defaults is elegant.
* Use **proplists** for compatibility, when working with existing APIs, or when literal atoms dominate the option list.
* Use **records** for known, fixed structures where performance matters.

As one sage source notes: "Option lists that are mostly pairs will be replaced by maps. On the other hand, literal options such as read, write, or append will remain much nicer with proplists."
