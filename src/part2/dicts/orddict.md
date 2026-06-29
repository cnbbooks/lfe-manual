# orddict: The Ordered Dictionary

## In Which Simplicity Becomes A Virtue

The `orddict` module is remarkably straightforward: it's a list of `{Key, Value}` tuples kept in ascending order by key. That's the entire data structure. There's no hidden magic, no opaque internals, no clever hashing schemes. It's just a sorted list, and it makes no apologies for this fact.

This transparency is both its greatest strength and its fundamental limitation. Because it's just a list, you can pattern-match on it, print it, inspect it visually, and generally treat it like the simple structure it is. Because it's just a list, operations like lookup require scanning through elements one by one until you find what you're looking for (or don't), which makes it unsuitable for large datasets.

## Creating Orddicts: The Art of Sorted Pairs

In LFE, you create orddicts using the `orddict` module functions:

```lfe
;; An empty orddict - the beginning of all things
(orddict:new)
()

;; Building one up from nothing
(set d1 (orddict:store 'b 2 (orddict:new)))
((b 2))

(set d2 (orddict:store 'a 1 d1))
((a 1) (b 2))

;; Note the automatic ordering - 'a' comes before 'b'
(set d3 (orddict:store 'c 3 d2))
((a 1) (b 2) (c 3))

;; Or create from a list in one go
(set d4 (orddict:from_list '(#(z 26) #(a 1) #(m 13))))
((a 1) (m 13) (z 26))
```

Notice how the keys are automatically sorted regardless of insertion order. This is orddict's defining characteristic—it maintains sorted order at all times, trading some insertion performance for predictable, ordered output.

## The Core Operations: CRUD Without The Drama

The `orddict` module provides the standard suite of dictionary operations, with function names that are pleasantly self-explanatory:

### orddict:store/3

Store a key-value pair, updating if the key exists, inserting if it doesn't:

```lfe
(set d1 (orddict:store 'name "Arthur" (orddict:new)))
((name "Arthur"))

(set d2 (orddict:store 'age 42 d1))
((age 42) (name "Arthur"))

;; Updating an existing key
(set d3 (orddict:store 'age 43 d2))
((age 43) (name "Arthur"))
```

### orddict:find/2

Search for a key, returning `{ok, Value}` or `error`:

```lfe
(orddict:find 'name '#((age 42) (name "Arthur")))
#(ok "Arthur")

(orddict:find 'species '#((age 42) (name "Arthur")))
error
```

This is the polite version of lookup—it never crashes, just admits defeat gracefully when the key isn't present.

### orddict:fetch/2

The impolite version that assumes the key exists:

```lfe
(orddict:fetch 'name '#((age 42) (name "Arthur")))
"Arthur"

(orddict:fetch 'species '#((age 42) (name "Arthur")))
;; ** exception error: {badkey,species}
```

Use `fetch` when the key's absence would indicate a programming error. Use `find` when you're genuinely uncertain.

### orddict:is_key/2

The existential inquiry:

```lfe
(orddict:is_key 'name '#((age 42) (name "Arthur")))
true

(orddict:is_key 'species '#((age 42) (name "Arthur")))
false
```

### orddict:erase/2

Remove a key-value pair:

```lfe
(orddict:erase 'age '#((age 42) (name "Arthur")))
((name "Arthur"))
```

Idempotent—erasing a non-existent key is perfectly fine and changes nothing.

## Iteration and Transformation

### orddict:fold/3

The workhorse of orddict processing. Fold a function over all key-value pairs:

```lfe
(orddict:fold 
  (lambda (key val acc) 
    (cons (tuple key (* val 2)) acc))
  '()
  '#((a 1) (b 2) (c 3)))
((c 6) (b 4) (a 2))
```

Note that fold processes elements in key order, but cons-ing them into an accumulator reverses the order. If you want to maintain order, you'll need to reverse at the end or use a different accumulation strategy.

### orddict:map/2

Transform all values while preserving keys and structure:

```lfe
(orddict:map 
  (lambda (key val) (+ val 100))
  '#((a 1) (b 2) (c 3)))
((a 101) (b 102) (c 103))
```

The mapping function receives both key and value, but only the returned value is used. The key remains unchanged.

### orddict:filter/2

Keep only the key-value pairs that satisfy a predicate:

```lfe
(orddict:filter 
  (lambda (key val) (> val 5))
  '#((a 3) (b 7) (c 2) (d 9)))
((b 7) (d 9))
```

## The Special Append Operations

Orddicts provide two specialized functions for accumulating values in lists:

### orddict:append/3

Append a value to a list associated with a key:

```lfe
(let* ((d1 (orddict:store 'files '() (orddict:new)))
       (d2 (orddict:append 'files "file1.txt" d1))
       (d3 (orddict:append 'files "file2.txt" d2)))
  (orddict:fetch 'files d3))
("file1.txt" "file2.txt")
```

This assumes the current value (if any) is a list. If the key doesn't exist, it creates a new list with the value. If the key exists but the value isn't a list, prepare for an exception.

### orddict:append_list/3

Append an entire list of values:

```lfe
(let* ((d1 (orddict:store 'files '("a.txt") (orddict:new)))
       (d2 (orddict:append_list 'files '("b.txt" "c.txt") d1)))
  (orddict:fetch 'files d2))
("a.txt" "b.txt" "c.txt")
```

These functions save you from the ceremony of fetching, appending, and storing manually. They're particularly useful when building up collections of values keyed by some attribute.

## Merging and Combining

### orddict:merge/3

Merge two orddicts, using a function to resolve conflicts:

```lfe
(let ((d1 '#((a 1) (b 2) (c 3)))
      (d2 '#((b 20) (c 30) (d 4))))
  (orddict:merge 
    (lambda (key v1 v2) (+ v1 v2))
    d1 
    d2))
((a 1) (b 22) (c 33) (d 4))
```

The merge function is called only for keys that exist in both dictionaries. Keys unique to either dictionary are included as-is.

## Utility Functions

### orddict:size/1

Count the key-value pairs:

```lfe
(orddict:size '#((a 1) (b 2) (c 3)))
3
```

### orddict:is_empty/1

Check for emptiness:

```lfe
(orddict:is_empty (orddict:new))
true

(orddict:is_empty '#((a 1)))
false
```

### orddict:fetch_keys/1

Get all keys:

```lfe
(orddict:fetch_keys '#((a 1) (b 2) (c 3)))
(a b c)
```

### orddict:to_list/1 and orddict:from_list/1

Convert to and from list representation:

```lfe
(orddict:to_list '#((a 1) (b 2)))
(#(a 1) #(b 2))

(orddict:from_list '(#(z 26) #(a 1) #(m 13)))
((a 1) (m 13) (z 26))
```

The `from_list` function sorts the input and removes duplicates (keeping the first occurrence of each key).

## Update Operations: The Functional Approach

### orddict:update/3

Update an existing key's value using a function:

```lfe
(orddict:update 
  'count 
  (lambda (n) (+ n 1))
  '#((count 5) (name "test")))
((count 6) (name "test"))
```

If the key doesn't exist, this crashes. It's for when you know the key must be present.

### orddict:update/4

Update with a default if the key doesn't exist:

```lfe
(orddict:update 
  'count 
  (lambda (n) (+ n 1))
  1
  '#((name "test")))
((count 1) (name "test"))
```

This version uses the default value (1 in this case) when the key is missing, then applies the function to that default.

### orddict:update_counter/3

Specialized for incrementing numeric values:

```lfe
(orddict:update_counter 'score 10 '#((score 5)))
((score 15))

(orddict:update_counter 'score 10 '#((name "test")))
((name "test") (score 10))
```

Adds the increment to the existing value, or uses the increment as the initial value if the key doesn't exist.

## Pattern Matching on Orddicts

Because orddicts are just lists, you can pattern-match on them directly in function clauses and `case` expressions:

```lfe
(defun get-name 
  (('()) 'no-name)
  (((cons (tuple 'name n) _rest)) n)
  (((cons _ rest)) (get-name rest)))

(get-name '#((age 42) (name "Zaphod")))
"Zaphod"
```

However, this breaks the abstraction and ties your code to the internal representation. It's generally better to use the `orddict` module functions, which remain valid even if the internal representation changes (though given orddict's age and simplicity, such a change is approximately as likely as discovering that pi has been wrong all along).

## When Orddict Is The Right Choice

Despite being the simplest of the key-value stores, orddict has its place:

1. **Small datasets**: For fewer than ~75 elements, orddict performs acceptably and provides crystal-clear inspection.

2. **Debugging**: When you need to see exactly what's in your data structure without ceremony, orddict obliges.

3. **Ordered output**: If you need keys in sorted order and you're using the structure itself (not just calling `fetch_keys`), orddict maintains order naturally.

4. **Simple configuration**: For configuration data or small option sets, orddict's transparency is a feature.

5. **Teaching**: When explaining associative data structures, orddict is mercifully simple.

## When To Graduate Beyond Orddict

Move to `dict` or `maps` when:

- You have more than 75 key-value pairs
- Lookup performance becomes measurable
- You're building a long-lived production system
- You need better algorithmic complexity
- Your benchmarks show orddict as a bottleneck

The transition is usually straightforward since the API is very similar across these modules.
