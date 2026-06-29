# gb_trees: The Balanced Perfectionist

## In Which We Discover That Trees Solve Problems You Didn't Know You Had

General Balanced Trees represent a different philosophy from both orddict and dict. Where orddict prioritizes transparency and dict prioritizes pragmatic hash-based lookup, gb_trees prioritize structural elegance and the specific capabilities that balanced binary trees provide. They implement Prof. Arne Andersson's algorithm for maintaining balance, which sounds impressive at parties and actually is impressive when you need it.

The key insight of gb_trees is that balanced binary trees offer logarithmic lookup, insertion, and deletion, combined with efficient ordered traversal and the ability to quickly find minimum and maximum elements. If you need these specific capabilities—particularly the min/max operations—gb_trees might be your solution even in the age of maps.

## The Nature of General Balanced Trees

A balanced binary tree maintains itself in a configuration where the height of the tree is proportional to log(n) of the number of elements. This means that as your dataset grows, the number of steps required to find any element grows very slowly. A tree with 1,000 elements requires about 10 steps to find anything. A tree with 1,000,000 elements requires about 20 steps.

The "general balanced" part means the algorithm makes certain trade-offs: deletions don't trigger rebalancing (which is fine since deletions don't increase height), and the balance condition has been slightly relaxed from the theoretical ideal to simplify implementation and improve practical performance.

## Creating GB Trees: The Foundation

```lfe
;; Empty tree
(set t1 (gb_trees:empty))
#(0 nil)  ; Size 0, no nodes

;; Insert elements
(set t2 (gb_trees:insert 'b 2 t1))
(set t3 (gb_trees:insert 'a 1 t2))
(set t4 (gb_trees:insert 'c 3 t3))

;; From a list
(set t5 (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3))))
```

Note the `from_orddict` function—it requires an ordered list of key-value tuples. If your list isn't ordered, use `orddict:from_list` first to sort it.

## The Dual-Mode API: Smart vs. Naive

GB Trees provide two sets of functions: "smart" functions that assume you know what you're doing, and "naive" functions that check everything carefully. The smart functions are faster but will crash if your assumptions are wrong. The naive functions are safer but do extra work.

### Smart Mode: For When You're Certain

**gb_trees:insert/3** - Insert a new key (crashes if key exists):

```lfe
(let ((t (gb_trees:empty)))
  (gb_trees:insert 'a 1 t))
;; Success - key was new

(let ((t (gb_trees:insert 'a 1 (gb_trees:empty))))
  (gb_trees:insert 'a 2 t))
;; ** exception error: key exists
```

**gb_trees:get/2** - Get a value (crashes if key missing):

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2)))))
  (gb_trees:get 'a t))
1

(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2)))))
  (gb_trees:get 'z t))
;; ** exception error: key not found
```

**gb_trees:update/3** - Update existing key (crashes if key missing):

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2)))))
  (gb_trees:update 'a 42 t))
;; Success - returns tree with a->42

(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2)))))
  (gb_trees:update 'z 42 t))
;; ** exception error: key not found
```

**gb_trees:delete/2** - Delete existing key (crashes if key missing):

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2)))))
  (gb_trees:delete 'a t))
;; Success - returns tree without 'a

(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2)))))
  (gb_trees:delete 'z t))
;; ** exception error: key not found
```

### Naive Mode: For When You're Prudent

**gb_trees:enter/3** - Insert or update:

```lfe
(let ((t (gb_trees:empty)))
  (gb_trees:enter 'a 1 t))
;; Creates new entry

(let ((t (gb_trees:from_orddict '(#(a 1)))))
  (gb_trees:enter 'a 2 t))
;; Updates existing entry to a->2
```

**gb_trees:lookup/2** - Safe lookup:

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2)))))
  (gb_trees:lookup 'a t))
#(value 1)

(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2)))))
  (gb_trees:lookup 'z t))
none
```

**gb_trees:delete_any/2** - Delete if present:

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2)))))
  (gb_trees:delete_any 'a t))
;; Deletes 'a

(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2)))))
  (gb_trees:delete_any 'z t))
;; Does nothing, returns original tree
```

### Which Mode Should You Use?

Use **smart mode** when:
- You're in a hot path where performance matters
- You've just checked for the key's existence
- The key's presence/absence is guaranteed by program logic
- You want crashes to indicate programming errors

Use **naive mode** when:
- You're unsure whether a key exists
- You're prototyping or debugging
- Correctness trumps performance
- You prefer handling errors as data rather than exceptions

## Ordered Operations: The Tree's Special Powers

This is where gb_trees shine. Because they maintain a balanced tree structure, they can efficiently provide ordered access:

### gb_trees:smallest/1 and gb_trees:largest/1

Get the minimum or maximum key-value pair:

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(m 13) #(z 26)))))
  (gb_trees:smallest t))
#(a 1)

(let ((t (gb_trees:from_orddict '(#(a 1) #(m 13) #(z 26)))))
  (gb_trees:largest t))
#(z 26)
```

These are O(log n) operations—remarkably efficient even for large trees.

### gb_trees:take_smallest/1 and gb_trees:take_largest/1

Get and remove the extreme elements:

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3)))))
  (gb_trees:take_smallest t))
#(a 1 #(2 #(b 2 nil nil) #(c 3 nil nil)))
;; Returns: {Key, Value, TreeWithoutThatKey}

(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3)))))
  (gb_trees:take_largest t))
#(c 3 #(2 #(a 1 nil nil) #(b 2 nil nil)))
```

Perfect for implementing priority queues or processing elements in order.

### gb_trees:smaller/2 and gb_trees:larger/2

Find the next smaller or larger key:

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(m 13) #(z 26)))))
  (gb_trees:smaller 'm t))
#(a 1)

(let ((t (gb_trees:from_orddict '(#(a 1) #(m 13) #(z 26)))))
  (gb_trees:larger 'm t))
#(z 26)

;; Returns 'none' if no such key exists
(let ((t (gb_trees:from_orddict '(#(a 1) #(m 13) #(z 26)))))
  (gb_trees:smaller 'a t))
none
```

These operations are useful for range queries and ordered traversal.

## Iteration: The Iterator Pattern

Unlike dict and orddict which provide `fold`, gb_trees use an iterator pattern:

```lfe
(defun print-all (tree)
  (let ((iter (gb_trees:iterator tree)))
    (print-iter iter)))

(defun print-iter (iter)
  (case (gb_trees:next iter)
    ('none 'done)
    ((tuple key val next-iter)
     (progn
       (lfe_io:format "~p -> ~p~n" (list key val))
       (print-iter next-iter)))))

;; Usage
(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3)))))
  (print-all t))
;; Prints:
;; a -> 1
;; b -> 2
;; c -> 3
```

The iterator approach gives you more control than fold, at the cost of requiring manual recursion. You can also create iterators that start at specific positions:

```lfe
;; Start iteration from a specific key
(let* ((t (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3) #(d 4))))
       (iter (gb_trees:iterator_from 'b t)))
  ;; This iterator will produce b, c, d (but not a)
  (gb_trees:next iter))
#(b 2 #(iterator ...))
```

You can also specify iteration order (ascending or descending):

```lfe
;; Iterate in reverse order
(let* ((t (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3))))
       (iter (gb_trees:iterator t 'reversed)))
  (gb_trees:next iter))
#(c 3 #(iterator ...))
```

## Transformation

### gb_trees:map/2

Transform all values:

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3)))))
  (gb_trees:map 
    (lambda (k v) (* v v))
    t))
;; Returns tree with squared values
```

Unfortunately, there's no built-in filter or fold function. If you need those, you'll need to implement them using iterators or convert to a list, process it, and convert back.

## Utility Functions

### gb_trees:size/1

```lfe
(gb_trees:size (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3))))
3
```

This is O(1)—the size is stored in the tree structure.

### gb_trees:is_empty/1

```lfe
(gb_trees:is_empty (gb_trees:empty))
true
```

### gb_trees:keys/1 and gb_trees:values/1

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3)))))
  (gb_trees:keys t))
(a b c)

(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3)))))
  (gb_trees:values t))
(1 2 3)
```

Both return lists in ascending key order.

### gb_trees:to_list/1

```lfe
(gb_trees:to_list (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3))))
(#(a 1) #(b 2) #(c 3))
```

Returns the key-value pairs as a list of tuples, in ascending order.

### gb_trees:balance/1

Rebalance a tree:

```lfe
(let ((t (gb_trees:from_orddict '(#(a 1) #(b 2) #(c 3)))))
  ;; After many deletions...
  (gb_trees:balance t))
```

You rarely need this—trees maintain reasonable balance automatically. Use it only if you've done many deletions and want to optimize the tree structure.

## When GB Trees Are The Right Choice

Choose gb_trees when:

1. **You need min/max operations**: The `smallest` and `largest` functions are killer features.

2. **You need ordered iteration**: Iterating through elements in key order is efficient.

3. **You need range queries**: The `smaller` and `larger` functions enable efficient range operations.

4. **You know your access patterns**: The smart/naive dichotomy lets you optimize hot paths.

5. **You're implementing tree-specific algorithms**: Priority queues, ordered sets, and certain scheduling algorithms benefit from tree structure.

6. **You need proven algorithmic guarantees**: O(log n) operations across the board.

## When To Choose Something Else

Avoid gb_trees when:

- You just need basic key-value storage (use maps)
- You need the simplest possible solution (use orddict for small datasets)
- Fold operations are central to your code (dict or maps are better)
- You don't need ordered access
- The learning curve seems excessive for your use case

## GB Trees vs. Maps: A Nuanced Comparison

Maps have largely superseded both dict and gb_trees for general key-value storage. However, gb_trees retain advantages in specific scenarios:

**GB Trees win when you need**:
- Efficient min/max key operations
- Ordered iteration without sorting
- Range queries
- Tree-specific algorithms

**Maps win when you need**:
- Simple, readable code
- Pattern matching in function heads
- Native syntax support
- Better integration with modern Erlang code
- General-purpose key-value storage

The performance characteristics are similar enough that algorithmic requirements, not raw speed, should drive the choice.
