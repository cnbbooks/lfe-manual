# Memory Representation

In memory, a binary is essentially a sequence of bytes (which are, if we're being pedantic, sequences of 8 bits each). The system represents them efficiently—more efficiently, in fact, than lists. This might come as a surprise if you've been taught that lists are the fundamental data structure and everything else is just lists in disguise. But consider: a list in Erlang (and thus LFE) is a linked list, where each element requires not only its own storage but also a pointer to the next element. A binary is more like a C array: a contiguous block of memory with minimal overhead.

For small binaries (less than 64 bytes), the data is typically stored directly in the process heap. For larger binaries, they're stored in a shared memory area and processes keep references to them. This means that when you send a large binary in a message between processes, you're not copying the entire thing—you're just passing a reference. It's rather like instead of photocopying a lengthy document and mailing it, you simply tell someone where the filing cabinet is.

This reference-based approach has pleasant implications for garbage collection as well. Large binaries can be shared among many processes without being duplicated, and they're only freed when the last reference disappears. It's communism for data structures, but the kind that actually works.
