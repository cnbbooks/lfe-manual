# Why Serialize

There are several compelling reasons to transform your beautiful terms into ugly byte sequences:

1. **Network Transmission**: You want to send your data to another machine. Networks, being made of copper and light and various other stubbornly physical substances, deal in bytes.

2. **Persistent Storage**: You want your data to survive the heat death of your process. Files, being even more stubbornly physical than networks, also deal in bytes.

3. **Time Travel**: You want to save your data now and read it later. Future-you appreciates past-you's foresight, assuming the format hasn't changed. (It won't. Probably.)

4. **Distributed Erlang**: The distributed Erlang protocol uses this format extensively. Every time you send a message to a process on another node, serialization happens behind the scenes, like a helpful but invisible stage crew.
