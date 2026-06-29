# dict: The Pragmatic Workhorse (Historical Division)

## In Which We Acknowledge A Module's Graceful Obsolescence

The `dict` module occupies an interesting position in Erlang's history. For many years, it was *the* solution for large key-value stores—a hash-based dictionary with good performance characteristics and a clean API. Then maps arrived in R17, providing similar functionality with better syntax and native support, and `dict` found itself in the position of a veteran general suddenly outranked by a younger officer with shinier medals.

Here's the essential truth: if you're writing new code and you can use maps, use maps. The `dict` module isn't deprecated, isn't broken, and isn't going away—but it's no longer the first choice for new development. It remains relevant primarily for maintaining existing codebases and for those rare situations where you're constrained to pre-R17 Erlang.

## The Dict Data Structure: Opaque By Design

Unlike orddicts, which are transparently lists, dicts are opaque structures. You cannot pattern-match on them, you cannot inspect them without dict module functions, and you shouldn't try to construct them manually. This opacity was intentional—it allows the implementation to use sophisticated internal structures (hash tables with dynamic resizing) without exposing those details to user code.

The practical implication is that all dict operations must go through the module's API. There's no cheating by accessing the internal structure directly.

## Creating Dicts: The Standard Ceremony

```lfe
;; Empty dict
(set d1 (dict:new))
#(dict ...)  ; Opaque structure - don't examine too closely

;; Building up
(set d2 (dict:store 'a 1 d1))
(set d3 (dict:store 'b 2 d2))
(set d4 (dict:store 'c 3 d3))

;; From a list
(set d5 (dict:from_list '(#(a 1) #(b 2) #(c 3))))
```

The API should look familiar—it mirrors `orddict` quite deliberately, making transitions between the two relatively painless.

## Core Operations: The Same But Different

### dict:store/3

Store or update a key-value pair:

```lfe
(dict:store 'name "Ford" (dict:new))
;; Returns a dict containing name -> "Ford"

(let ((d (dict:store 'age 200 (dict:new))))
  (dict:store 'age 201 d))
;; Updates age to 201
```

### dict:find/2 and dict:fetch/2

The familiar search operations:

```lfe
(let ((d (dict:from_list '(#(a 1) #(b 2)))))
  (dict:find 'a d))
#(ok 1)

(let ((d (dict:from_list '(#(a 1) #(b 2)))))
  (dict:find 'z d))
error

(let ((d (dict:from_list '(#(a 1) #(b 2)))))
  (dict:fetch 'a d))
1

(let ((d (dict:from_list '(#(a 1) #(b 2)))))
  (dict:fetch 'z d))
;; ** exception error: bad_key
```

### dict:is_key/2

Check for key existence:

```lfe
(let ((d (dict:from_list '(#(a 1) #(b 2)))))
  (dict:is_key 'a d))
true
```

### dict:erase/2

Remove a key:

```lfe
(let ((d (dict:from_list '(#(a 1) #(b 2) #(c 3)))))
  (dict:erase 'b d))
;; Returns dict with only a and c
```

## Iteration and Transformation

### dict:fold/3

Fold a function over all key-value pairs:

```lfe
(let ((d (dict:from_list '(#(a 1) #(b 2) #(c 3)))))
  (dict:fold 
    (lambda (k v acc) (+ acc v))
    0
    d))
6
```

Note that unlike orddict, the iteration order is undefined. You cannot rely on keys being processed in any particular sequence.

### dict:map/2

Transform values:

```lfe
(let ((d (dict:from_list '(#(a 1) #(b 2) #(c 3)))))
  (dict:map 
    (lambda (k v) (* v v))
    d))
;; Returns dict with squared values: a->1, b->4, c->9
```

### dict:filter/2

Keep only matching pairs:

```lfe
(let ((d (dict:from_list '(#(a 1) #(b 2) #(c 3) #(d 4)))))
  (dict:filter 
    (lambda (k v) (=:= 0 (rem v 2)))
    d))
;; Returns dict with only even values: b->2, d->4
```

## Append Operations

Like orddict, dict provides specialized functions for list accumulation:

### dict:append/3

```lfe
(let* ((d1 (dict:store 'items '() (dict:new)))
       (d2 (dict:append 'items "first" d1))
       (d3 (dict:append 'items "second" d2)))
  (dict:fetch 'items d3))
("first" "second")
```

### dict:append_list/3

```lfe
(let* ((d1 (dict:store 'items '("a") (dict:new)))
       (d2 (dict:append_list 'items '("b" "c") d1)))
  (dict:fetch 'items d2))
("a" "b" "c")
```

## Merging and Updating

### dict:merge/3

Merge two dicts with a conflict resolution function:

```lfe
(let ((d1 (dict:from_list '(#(a 1) #(b 2))))
      (d2 (dict:from_list '(#(b 3) #(c 4)))))
  (dict:merge 
    (lambda (k v1 v2) (max v1 v2))
    d1 
    d2))
;; Returns dict: a->1, b->3, c->4
```

### dict:update/3 and dict:update/4

Update with function:

```lfe
;; Requires key to exist
(let ((d (dict:from_list '(#(count 5)))))
  (dict:update 'count (lambda (n) (+ n 1)) d))
;; Returns dict with count->6

;; Provides default if key missing
(let ((d (dict:new)))
  (dict:update 'count (lambda (n) (+ n 1)) 0 d))
;; Returns dict with count->0 (default was used, then function applied)
```

### dict:update_counter/3

Increment numeric values:

```lfe
(let ((d (dict:from_list '(#(score 10)))))
  (dict:update_counter 'score 5 d))
;; Returns dict with score->15
```

## Utility Functions

### dict:size/1

```lfe
(dict:size (dict:from_list '(#(a 1) #(b 2) #(c 3))))
3
```

### dict:is_empty/1

```lfe
(dict:is_empty (dict:new))
true
```

### dict:fetch_keys/1

```lfe
(dict:fetch_keys (dict:from_list '(#(a 1) #(b 2) #(c 3))))
;; Returns list of keys: (a b c) - but order is undefined
```

### dict:to_list/1

```lfe
(dict:to_list (dict:from_list '(#(a 1) #(b 2))))
;; Returns list: (#(a 1) #(b 2)) - order undefined
```

## Key Differences from Orddict

1. **Ordering**: Dict provides no ordering guarantees. Keys come out in arbitrary order.

2. **Performance**: Dict uses hash tables internally, providing O(log n) or better average-case performance for most operations, compared to orddict's O(n).

3. **Opacity**: You cannot pattern-match on dict structures or inspect them without using module functions.

4. **Size**: Dict excels with larger datasets (75+ elements) where orddict becomes sluggish.

## Why Dict Still Exists (And Why You Probably Shouldn't Use It)

The dict module remains in the standard library for three reasons:

1. **Legacy code**: Enormous amounts of Erlang code was written before maps existed. Removing or deprecating dict would break that code.

2. **Compatibility**: Some projects must remain compatible with older Erlang versions that predate maps.

3. **It's not actually broken**: Dict works fine. It's just superseded by something better.

For new code, use maps instead. Maps provide:
- Better syntax (native literal support)
- Pattern matching in function heads
- Better performance in many cases
- First-class support from the VM
- More modern APIs

The only reason to choose dict over maps for new code is if you're working in a pre-R17 environment, in which case you have my sympathy and my respect for maintaining legacy systems.

## Migrating from Dict to Maps

The transition is usually straightforward:

```lfe
;; Dict style
(let ((d (dict:from_list '(#(a 1) #(b 2)))))
  (dict:fetch 'a d))

;; Maps style
(let ((m (maps:from_list '(#(a 1) #(b 2)))))
  (maps:get 'a m))

;; Or even simpler with map literals
(mref '#M(a 1 b 2) 'a)
```

Most dict operations have direct maps equivalents. The main challenges arise with code that relies on `dict:fold` behavior or specific iteration characteristics, but these are usually manageable.
