# Practical Considerations: Size and Compression

Binary serialization can produce fairly compact representations, but for truly large data structures, you might want compression:

```lfe
lfe> (set large-data (lists:duplicate 1000 (tuple 'answer 42 'question 'unknown)))
(#(answer 42 question unknown) #(answer 42 question unknown) ...)

lfe> (set uncompressed (term-to-binary large-data))
#B(131 108 0 0 3 232 104 ...)

lfe> (set compressed (term-to-binary large-data (list 'compressed)))
#B(131 80 0 0 0 195 120 ...)

lfe> (tuple (byte-size uncompressed) (byte-size compressed))
#(28004 199)
```

That's roughly 140x compression, which would please even the notoriously space-conscious Vogons (though they'd still complain about the inefficiency of storing the meaning of life in the first place).
