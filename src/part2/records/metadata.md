# Record Metadata: Asking Questions About Records

LFE inherits Erlang's `record-info` mechanism for querying record metadata:

```lfe
lfe> (record-info 'fields 'person)
(name age phone)

lfe> (record-info 'size 'person)
4  ; three fields plus the tag atom
```

Note that these must be literal atoms—you can't use variables. The compiler needs to know at compile time what you're asking about, because records don't exist at runtime. It's a bit like asking someone to describe a dream: the description exists, but the dream itself is merely a neural firing pattern that's long since dissipated.
