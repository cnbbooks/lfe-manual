## An Important Property: Immutability

Like all data structures in LFE, binaries are immutable. Once created, they cannot be modified. If you need to "change" a binary, you create a new one. This might seem inefficient until you realize that the BEAM VM is rather clever about this—it can often share unchanged portions of the data structure and only allocate new space for the parts that actually differ.

This immutability has profound implications for concurrent programming, which is rather the point of Erlang and LFE. You never need to worry about another process modifying a binary out from under you, because it simply cannot happen. The binary you have is the binary you'll always have, unchanging and eternal, like a particularly stubborn philosophy professor or the value of *e*.

In practice, this means you can pass binaries between processes with abandon, secure in the knowledge that they won't suddenly transform into something else when you're not looking. In a world of concurrent processes and message passing, this guarantee is worth its weight in gold. Or at least in silicon.
