# Creating Arrays

The most straightforward way to birth an array into existence is to simply call it into being with `array:new`:

```lfe
lfe> (array:new)
#(array 10 0 undefined 10)
```

This creates an extendible array with initial size zero, though the representation you see is an implementation detail subject to change without notice, like the venue for a surprise party. The important bit is that you now have an array, ready and waiting to store things at numeric indices.

You can also create an array with a specific initial size:

```lfe
lfe> (array:new 100)
#(array 10 100 undefined 10)
lfe> (array:size (array:new 100))
100
```

This creates a fixed-size array with 100 slots, all initially set to `undefined`—the default value that serves as the placeholder for "nothing here yet, move along."

If `undefined` doesn't suit your philosophical outlook, you can specify your own default value:

```lfe
lfe> (array:new '(#(default 0)))
#(array 10 0 0 10)
lfe> (array:new 50 '(#(default "vacant")))
#(array 10 50 "vacant" 10)
```

You can also build arrays from lists, which is a bit like converting a pleasant country walk into a precisely mapped coordinate system:

```lfe
lfe> (array:from_list '(1 2 3 4 5))
#(array 10 5 undefined 10)
lfe> (array:get 0 (array:from_list '(arthur ford trillian)))
arthur
lfe> (array:get 2 (array:from_list '(arthur ford trillian)))
trillian
```

Or from ordered dictionaries (which are lists of index-value tuples), for those occasions when you've already done half the work:

```lfe
lfe> (array:from_orddict '(#(0 "first") #(5 "sixth") #(10 "eleventh")))
#(array 10 11 undefined 10)
```

Note that this creates an array large enough to hold index 10, with undefined values filling the gaps like polite placeholders at a dinner party.
