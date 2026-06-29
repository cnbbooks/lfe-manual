# Conversion Operations

**To and from lists:**

Arrays and lists can be converted back and forth, though this is an O(n) operation that involves visiting every element like an overly thorough census taker:

```lfe
lfe> (array:to_list (array:from_list '(1 2 3 4 5)))
(1 2 3 4 5)
lfe> (let ((arr (array:new 5)))
       (array:to_list arr))
(undefined undefined undefined undefined undefined)
```

For sparse arrays, you can use `array:sparse_to_list` to skip the default values:

```lfe
lfe> (let* ((arr1 (array:new 100))
            (arr2 (array:set 10 "here" arr1))
            (arr3 (array:set 50 "there" arr2)))
       (array:sparse_to_list arr3))
("here" "there")
```

**To and from ordered dictionaries:**

Ordered dictionaries (lists of `#(index value)` tuples) can be converted to and from arrays:

```lfe
lfe> (array:to_orddict (array:from_list '(a b c)))
(#(0 a) #(1 b) #(2 c))
lfe> (let* ((arr1 (array:new 10))
            (arr2 (array:set 3 "three" arr1))
            (arr3 (array:set 7 "seven" arr2)))
       (array:sparse_to_orddict arr3))
(#(3 "three") #(7 "seven"))
```
