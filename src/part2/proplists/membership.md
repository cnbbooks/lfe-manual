# Membership and Keys

## proplists:is_defined/2

A simple existential check that returns a boolean without drama:

```lfe
lfe> (proplists:is_defined 'timeout '((timeout 5000) (retry 3)))
true

lfe> (proplists:is_defined 'verbose '((timeout 5000) (retry 3)))
false

lfe> (proplists:is_defined 'binary '(read write binary))
true
```

This answers the question "does this key exist?" which is simpler than "what is this key's value?" and thus requires less cosmic contemplation.

## proplists:get_keys/1

Returns an unordered list of all keys used in the proplist, without duplicates:

```lfe
lfe> (proplists:get_keys '((a 1) (b 2) (c 3) (a 99)))
(c b a)

lfe> (proplists:get_keys '(read write (mode binary) read))
(mode write read)
```

The keys are returned in whatever order the implementation finds convenient, which is to say, don't count on any particular ordering. If you need ordering, you've wandered into map territory.
