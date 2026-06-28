# Why Not Just Use Lists?

A reasonable question. After all, you *could* represent binary data as a list of integers from 0 to 255. Many languages force you to do exactly that. But consider:

1. **Space efficiency**: A list needs space for both the data and the links between elements. A binary just needs space for the data. For large data structures, this difference can be substantial enough to matter, even in the age of abundant RAM.

2. **Access patterns**: Lists are optimized for sequential access from the front. Binaries can be indexed directly. If you need to extract the 47th byte, you don't have to walk through the first 46 elements holding hands with yourself.

3. **Message passing**: When you send a binary in a message, you're sending a reference. When you send a list, you're sending... well, the entire list, links and all. For large data structures, one of these is significantly less expensive than the other.

4. **Pattern matching performance**: Pattern matching on binary structures is highly optimized in the BEAM VM. It's the kind of optimization that the Erlang team spent years perfecting because they needed it for telecommunications. You get the benefits of their decades of effort for free.

Lists are wonderful data structures. They have their place, and that place is wherever you need flexible, dynamic, recursively-defined collections of things. But for raw bytes and bits? Binaries are simply better suited to the task, in the same way that a screwdriver is better suited to turning screws than is a particularly determined hammer.
