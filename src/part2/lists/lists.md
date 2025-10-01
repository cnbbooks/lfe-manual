# Lists

In the beginning, there was the cons cell. And from the cons cell came forth lists, which were chains of cons cells stretching out across memory like improbability particles across the fabric of spacetime—each pointing to the next with what can only be described as dogged determination. And lo, these lists were good, for they were immutable, they were elegant, and most importantly, they made recursion look less like wizardry and more like afternoon tea.

Lists are the fundamental sequential data structure in LFE, inherited from both Erlang's practical heritage and Lisp's theoretical elegance. They are singly-linked lists built from cons cells (as explored in the previous chapter), making them ideally suited for functional programming patterns where you process data from front to back, building new structures as you go rather than mutating old ones.

## Creating Lists

The most straightforward way to create a list is with the quote:

```lisp
lfe> '(1 2 3 4 5)
(1 2 3 4 5)
lfe> '(arthur ford trillian zaphod)
(arthur ford trillian zaphod)
lfe> '()
()
```

You can also use the `list` function:

```lisp
lfe> (list 1 2 3 4 5)
(1 2 3 4 5)
lfe> (list 'arthur 'ford 'trillian 'zaphod)
(arthur ford trillian zaphod)
lfe> (list)
()
```

Lists can contain elements of mixed types:

```lisp
lfe> '(42 "meaning" of life 42.0)
(42 "meaning" of life 42.0)
lfe> (list 'atom 42 3.14 "string" #b101010 '(nested list))
(atom 42 3.14 "string" 42 (nested list))
```

Building lists with `cons`:

```lisp
lfe> (cons 1 '())
(1)
lfe> (cons 1 (cons 2 (cons 3 '())))
(1 2 3)
lfe> (cons 'first '(second third))
(first second third)
```

## List Operations

**Accessing elements:**

```lisp
lfe> (car '(1 2 3 4 5))
1
lfe> (cdr '(1 2 3 4 5))
(2 3 4 5)
lfe> (cadr '(1 2 3 4 5))  ; equivalent to (car (cdr ...))
2
lfe> (cddr '(1 2 3 4 5))  ; equivalent to (cdr (cdr ...))
(3 4 5)
```

LFE provides Common Lisp-style accessor combinations up to four levels deep:

```lisp
lfe> (caddr '(1 2 3 4))   ; third element
3
lfe> (cadddr '(1 2 3 4))  ; fourth element
4
```

**Finding length:**

```lisp
lfe> (length '(1 2 3 4 5))
5
lfe> (length '())
0
lfe> (length '(a (nested (list structure)) here))
3
```

**Appending lists:**

```lisp
lfe> (++ '(1 2 3) '(4 5 6))
(1 2 3 4 5 6)
lfe> (++ '(a) '(b) '(c) '(d))
(a b c d)
lfe> (append '(1 2) '(3 4) '(5 6))
(1 2 3 4 5 6)
```

Note that `++` is the operator form while `append` is the function form. Both are O(n) operations where n is the total length of all lists except the last one.

**Reversing lists:**

```lisp
lfe> (lists:reverse '(1 2 3 4 5))
(5 4 3 2 1)
lfe> (lists:reverse '())
()
```

**Taking and dropping elements:**

```lisp
lfe> (lists:sublist '(1 2 3 4 5) 3)
(1 2 3)
lfe> (lists:sublist '(1 2 3 4 5) 2 3)
(2 3 4)
lfe> (lists:nthtail 2 '(1 2 3 4 5))
(3 4 5)
```

**Membership testing:**

```lisp
lfe> (lists:member 3 '(1 2 3 4 5))
(3 4 5)
lfe> (lists:member 6 '(1 2 3 4 5))
false
```

Note that `lists:member` returns the tail of the list starting with the found element, or `false` if not found.

**Finding elements:**

```lisp
lfe> (lists:nth 1 '(a b c d))
a
lfe> (lists:nth 3 '(a b c d))
c
```

Be aware that `lists:nth` uses 1-based indexing (Erlang convention), unlike 0-based indexing common in many languages.

**Sorting:**

```lisp
lfe> (lists:sort '(5 2 8 1 9 3))
(1 2 3 5 8 9)
lfe> (lists:sort '(zaphod arthur ford trillian))
(arthur ford trillian zaphod)
lfe> (lists:sort (lambda (a b) (> a b)) '(5 2 8 1 9 3))
(9 8 5 3 2 1)
```

## Higher-Order List Functions

**Mapping:**

```lisp
lfe> (lists:map (lambda (x) (* x 2)) '(1 2 3 4 5))
(2 4 6 8 10)
lfe> (lists:map #'atom_to_list/1 '(arthur ford trillian))
("arthur" "ford" "trillian")
```

**Filtering:**

```lisp
lfe> (lists:filter (lambda (x) (> x 3)) '(1 2 3 4 5 6))
(4 5 6)
lfe> (lists:filter #'is_atom/1 '(1 arthur 2 ford 3))
(arthur ford)
```

**Folding (reducing):**

```lisp
lfe> (lists:foldl #'+/2 0 '(1 2 3 4 5))
15
lfe> (lists:foldl (lambda (x acc) (cons x acc)) '() '(1 2 3))
(3 2 1)
lfe> (lists:foldr (lambda (x acc) (cons x acc)) '() '(1 2 3))
(1 2 3)
```

The difference between `foldl` (fold left) and `foldr` (fold right) is the direction of traversal. `foldl` is tail-recursive and generally more efficient, while `foldr` processes from right to left.

**Flatmapping:**

```lisp
lfe> (lists:flatmap (lambda (x) (list x (* x 2))) '(1 2 3))
(1 2 2 4 3 6)
```

**Zipping:**

```lisp
lfe> (lists:zip '(1 2 3) '(a b c))
((1 a) (2 b) (3 c))
lfe> (lists:zip3 '(1 2 3) '(a b c) '(x y z))
((1 a x) (2 b y) (3 c z))
```

## List Comprehensions

LFE supports powerful list comprehensions that combine filtering, mapping, and Cartesian products:

```lisp
lfe> (lc ((<- x '(1 2 3 4 5))) (* x 2))
(2 4 6 8 10)

lfe> (lc ((<- x '(1 2 3 4 5 6)) (> x 3)) (* x x))
(16 25 36)

lfe> (lc ((<- x '(1 2 3)) (<- y '(a b))) (tuple x y))
(#(1 a) #(1 b) #(2 a) #(2 b) #(3 a) #(3 b))
```

The `<-` operator draws elements from a list, and optional guard conditions filter elements before the body expression is evaluated.

## Pattern Matching with Lists

List pattern matching is one of the most elegant features in LFE:

```lisp
lfe> (set (list first second) '(1 2))
(1 2)
lfe> first
1
lfe> second
2

lfe> (set (cons head tail) '(1 2 3 4))
(1 2 3 4)
lfe> head
1
lfe> tail
(2 3 4)

lfe> (set `(,a ,b . ,rest) '(1 2 3 4 5))
(1 2 3 4 5)
lfe> a
1
lfe> b
2
lfe> rest
(3 4 5)
```

## Predicates

To test if a value is a list, first include the Common Lisp compatibility library:

```lisp
lfe> (include-lib "lfe/include/cl.lfe")
lfe> (listp '(1 2 3))
true
lfe> (listp '())
true
lfe> (listp 42)
false
```

Clojure-style predicates:

```lisp
lfe> (include-lib "lfe/include/clj.lfe")
lfe> (list? '(1 2 3))
true
lfe> (list? "string")
false
```

Standard Erlang predicate:

```lisp
lfe> (is_list '(1 2 3))
true
lfe> (is_list '())
true
lfe> (is_list 42)
false
```

## Performance Considerations

Understanding the performance characteristics of list operations is crucial for writing efficient LFE code:

- **Prepending** (`cons`) is O(1) — constant time, very fast
- **Appending** (`++`, `append`) is O(n) — requires traversing the entire first list
- **Length** is O(n) — must traverse the entire list
- **Accessing by index** is O(n) — must traverse to the position
- **Reversing** is O(n) — must traverse the entire list

For these reasons, idiomatic LFE code often:

- Builds lists in reverse order then reverses once at the end
- Uses cons instead of append when possible
- Avoids repeated length calculations
- Uses pattern matching instead of index access

Example of the reverse-and-accumulate pattern:

```lisp
(defun process-list (lst)
  (process-list-helper lst '()))

(defun process-list-helper
  (('() acc) (lists:reverse acc))
  (((cons h t) acc)
   (let ((processed (* h 2)))
     (process-list-helper t (cons processed acc)))))
```

## Common Patterns

**Collecting results:**

```lisp
(defun collect-evens
  (('()) '())
  (((cons h t)) (when (== (rem h 2) 0))
   (cons h (collect-evens t)))
  (((cons _ t))
   (collect-evens t)))
```

**Processing pairs:**

```lisp
(defun process-pairs
  (('()) '())
  (((list x)) (list x))
  (((list x y . rest))
   (cons (+ x y) (process-pairs rest))))
```

**Searching:**

```lisp
(defun find-first
  ((_ '()) 'not-found)
  ((pred (cons h t))
   (case (funcall pred h)
     ('true h)
     ('false (find-first pred t)))))
```
