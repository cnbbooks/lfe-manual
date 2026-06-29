# Fixed vs. Extendible Arrays

Arrays come in two philosophical varieties: fixed and extendible. This is the difference between a house and a TARDIS—one has immutable boundaries, the other expands as needed.

By default, arrays are extendible:

```lfe
lfe> (let ((arr (array:new)))
       (array:is_fix arr))
false
```

You can create a fixed-size array explicitly:

```lfe
lfe> (array:is_fix (array:new 50))
true
lfe> (array:is_fix (array:new '(fixed)))
true
lfe> (array:is_fix (array:new 50 '(fixed)))
true
```

Or you can fix an existing array in place (metaphorically speaking):

```lfe
lfe> (let* ((arr1 (array:new))
            (arr2 (array:set 10 "something" arr1))
            (arr3 (array:fix arr2)))
       (array:is_fix arr3))
true
```

Once fixed, an array refuses to grow, which means attempting to set beyond its boundaries results in the program's equivalent of a stern look and a finger wag:

```lfe
lfe> (let* ((arr1 (array:new 10))
            (arr2 (array:fix arr1)))
       (array:set 10 "too far" arr2))
exception error: badarg
```

You can unfix an array with `array:relax`, which is either a function name or lifestyle advice:

```lfe
lfe> (let* ((arr1 (array:new 10))
            (arr2 (array:relax arr1)))
       (array:is_fix arr2))
false
```
