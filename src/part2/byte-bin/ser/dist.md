# The Distributed Angle: Cross-Node Communication

When you send a message to a process on another Erlang node, serialization happens automatically:

```lfe
;; On node 'earth@localhost'
lfe> (set msg (tuple 'urgent-message "Vogon fleet incoming" (tuple 'eta 2 'minutes)))
#(urgent-message "Vogon fleet incoming" #(eta 2 minutes))

lfe> (! (tuple 'defense-system 'mars@localhost) msg)
#(urgent-message "Vogon fleet incoming" #(eta 2 minutes))
```

The runtime serializes `msg`, sends the bytes over the network, and deserializes on the receiving node. You don't see this happening—it's like quantum mechanics but more reliable.
