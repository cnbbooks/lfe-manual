# Pattern Matching with Proplists

Because proplists are just lists, you can pattern match on them directly, though the experience is less elegant than with maps:

```lfe
lfe> (set `(,first . ,rest) '((timeout 5000) (retry 3)))
((timeout 5000) (retry 3))
lfe> first
(timeout 5000)

lfe> (set `((,key ,value) . ,_) '((active true) (buffer 1024)))
((active true) (buffer 1024))
lfe> key
active
lfe> value
true
```

In practice, you're more likely to use the `proplists` module functions rather than direct pattern matching, because proplists lack the structural guarantees that make pattern matching pleasant. They're fundamentally lists with conventions, not types with contracts.
