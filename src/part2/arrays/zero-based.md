# The Zero-Based Heresy

Here's where arrays commit their most controversial act: they use zero-based indexing. In a language where tuples start at 1, where `lists:nth` counts from 1, where the very notion of "first" implies the number 1, arrays boldly declare that the first element lives at index 0.

This is not an accident. This is not a bug. This is a deliberate design choice, presumably made by someone who spent too much time programming in C and not enough time at Erlang philosophy seminars. The official documentation acknowledges this heresy with remarkable sangfroid: "Arrays uses zero-based indexing. This is a deliberate design choice and differs from other Erlang data structures."

```lfe
lfe> (let ((arr (array:from_list '(a b c d e))))
       (array:get 0 arr))
a
lfe> (let ((arr (array:from_list '(a b c d e))))
       (array:get 4 arr))
e
```

Element 0 is `a`. Element 4 is `e`. The fifth element is at index 4. Try not to think about this too hard after consuming any amount of Pan Galactic Gargle Blasters.
