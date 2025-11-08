# term-to-binary: The Universal Serializer

This is where things get interesting enough that people write papers about it:

```lfe
(term-to-binary any-erlang-term)
```

This function takes *any* Erlang/LFE term—atoms, integers, lists, tuples, maps, binaries, other terms nested arbitrarily deep—and converts it into a binary representation. This binary uses the "Erlang external term format," which is precisely specified and designed to be efficient both in space and encoding/decoding time.

```lfe
lfe> (term-to-binary '#(test 12 true (1 2 3)))
#B(131 104 4 100 0 4 116 101 115 116 97 12 100 0 4 116 114
   117 101 107 0 3 1 2 3)
```

Those numbers might look like line noise, but they're actually a carefully structured representation of the tuple `#(test 12 true (1 2 3))`. The format includes type tags, length specifications, and the actual data, all packed efficiently into as few bytes as possible.

This is extraordinarily useful for:

- Storing complex data structures in files
- Sending data structures over network sockets
- Implementing persistence layers
- Debugging (though the binary output is not particularly human-readable without tools)
- Implementing distributed Erlang (which uses this format for inter-node communication)

The format is version-stable, meaning binaries created with older versions of Erlang can generally be read by newer versions. This is the kind of forward compatibility that database designers dream about and which distributed systems rely upon to avoid version upgrade nightmares where everything must be upgraded simultaneously or face the consequences (those consequences typically involving angry phone calls at 3 AM and rapidly updating one's résumé).
