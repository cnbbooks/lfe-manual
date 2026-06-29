# Basic Operations

**Setting values:**

The primary way to modify an array is with `array:set`, which doesn't actually modify anything (this is still functional programming, after all) but rather returns a new array with the specified element changed:

```lfe
lfe> (let ((arr (array:new 10)))
       (array:set 5 "hello" arr))
#(array 10 10 undefined 10)
lfe> (let* ((arr1 (array:new 10))
            (arr2 (array:set 5 "hello" arr1)))
       (array:get 5 arr2))
"hello"
```

If you set an index beyond the current size in an extendible array, it grows automatically, like a particularly accommodating houseplant:

```lfe
lfe> (let* ((arr1 (array:new))
            (arr2 (array:set 100 "far away" arr1)))
       (array:size arr2))
101
```

However, if the array has fixed size (more on this shortly), attempting to set beyond the boundary will result in the sort of error message that makes you reconsider your life choices:

```lfe
lfe> (let* ((arr1 (array:new 10))
            (arr2 (array:fix arr1)))
       (array:set 10 "beyond the pale" arr2))
exception error: badarg
```

**Getting values:**

Retrieving values is straightforward enough, though you must remember the zero-based indexing lest you find yourself off by one in that peculiar way that suggests you're from a parallel universe where counting starts at one:

```lfe
lfe> (let ((arr (array:from_list '(red green blue yellow))))
       (list (array:get 0 arr)
             (array:get 2 arr)))
(red blue)
```

Accessing an unset entry returns the default value:

```lfe
lfe> (let ((arr (array:new 10)))
       (array:get 7 arr))
undefined
lfe> (let ((arr (array:new 10 '(#(default "empty")))))
       (array:get 7 arr))
"empty"
```

And accessing beyond the end of an extendible array also returns the default:

```lfe
lfe> (let* ((arr1 (array:new))
            (arr2 (array:set 5 "here" arr1)))
       (array:get 100 arr2))
undefined
```

**Checking the size:**

The `array:size` function returns the number of entries, which is also one more than the highest accessible index (thanks to zero-based indexing providing us with these delightful arithmetic gymnastics):

```lfe
lfe> (array:size (array:new 42))
42
lfe> (let* ((arr1 (array:new))
            (arr2 (array:set 17 "surprise" arr1)))
       (array:size arr2))
18
```

**Resetting values:**

You can reset an entry back to the default value without changing the array's size:

```lfe
lfe> (let* ((arr1 (array:from_list '(1 2 3 4 5)))
            (arr2 (array:reset 2 arr1)))
       (list (array:get 2 arr2)
             (array:size arr2)))
(undefined 5)
```

This is distinct from setting the value to `undefined`—a philosophical distinction that matters only if your default value happens to be something other than `undefined`, in which case it matters quite a lot.

**Getting the sparse size:**

For arrays that have many default-valued entries, `array:sparse_size` returns the position after the last non-default value:

```lfe
lfe> (let* ((arr1 (array:new 100))
            (arr2 (array:set 50 "last one" arr1)))
       (list (array:size arr2)
             (array:sparse_size arr2)))
(100 51)
```

This can be useful for understanding how much of your array is actually being used versus how much is just cosmic background radiation.
