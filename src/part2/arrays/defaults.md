# Default Values and Philosophy

The default value of an array is set at creation time and cannot be changed afterward—it's baked into the array's very essence like the speed of light into spacetime. This default serves as the placeholder for uninitialized entries:

```lfe
lfe> (let ((arr (array:new 5)))
       (array:default arr))
undefined
lfe> (let ((arr (array:new 5 '(#(default 0)))))
       (array:default arr))
0
```

This creates an interesting philosophical question: if you set an entry to the default value, is it different from leaving it unset? The answer, according to arrays, is "no":

```lfe
lfe> (let* ((arr1 (array:new 10))
            (arr2 (array:set 5 undefined arr1)))
       (array:sparse_to_orddict arr2))
()
```

The sparse operations treat explicitly-set-to-default-value entries exactly the same as never-touched entries. This is either profound wisdom about the nature of existence or a minor implementation detail, depending on your philosophical orientation.
