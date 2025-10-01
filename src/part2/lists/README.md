# Lists and Strings

## Lists

In the beginning, there was the cons cell. And from the cons cell came forth lists, which were chains of cons cells stretching out across memory like improbability particles across the fabric of spacetime—each pointing to the next with what can only be described as dogged determination. And lo, these lists were good, for they were immutable, they were elegant, and most importantly, they made recursion look less like wizardry and more like afternoon tea.

Lists are the fundamental sequential data structure in LFE, inherited from both Erlang's practical heritage and Lisp's theoretical elegance. They are singly-linked lists built from cons cells (as explored in the previous chapter), making them ideally suited for functional programming patterns where you process data from front to back, building new structures as you go rather than mutating old ones.

### Creating Lists

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

### List Operations

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

### Higher-Order List Functions

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

### List Comprehensions

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

### Pattern Matching with Lists

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

### Predicates

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

### Performance Considerations

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

### Common Patterns

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

## Strings

In LFE, as in Erlang, strings are simply lists of integers representing character codes. This might seem unusual if you're coming from languages with dedicated string types, but it's a perfectly sensible representation that fits naturally with list processing. A string is just a list where each element happens to be an integer in the valid character range.

<div class="alert alert-info">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    Strings vs. Binaries
  </h4>
  <p class="mb-0">
    While strings-as-lists are the traditional representation, modern Erlang and LFE code increasingly uses binaries for text due to their superior memory efficiency and performance with large text. Binaries will be covered in detail in a later chapter. For now, we focus on list-based strings as they're simpler to understand and still widely used for smaller text operations.
  </p>
</div>

### Creating Strings

String literals are enclosed in double quotes:

```lisp
lfe> "Hello, World!"
"Hello, World!"
lfe> "The answer is 42"
"The answer is 42"
lfe> ""
""
```

Under the hood, these are lists of integers:

```lisp
lfe> (== "ABC" '(65 66 67))
true
lfe> (== "ABC" (list 65 66 67))
true
```

You can see this more clearly by forcing LFE to show the underlying representation:

```lisp
lfe> (io:format "~w~n" '("ABC"))
[65,66,67]
ok
```

Character literals make this relationship explicit:

```lisp
lfe> (list #\A #\B #\C)
"ABC"
lfe> #\A
65
```

### String Operations

Since strings are lists, all list operations work on strings:

```lisp
lfe> (++ "Hello, " "World!")
"Hello, World!"
lfe> (lists:reverse "stressed")
"desserts"
lfe> (length "Hello")
5
```

**Accessing characters:**

```lisp
lfe> (car "Hello")
72
lfe> (cdr "Hello")
"ello"
lfe> (lists:nth 2 "Hello")
101
```

**Case conversion:**

```lisp
lfe> (string:uppercase "hello world")
"HELLO WORLD"
lfe> (string:lowercase "HELLO WORLD")
"hello world"
lfe> (string:titlecase "hello world")
"Hello World"
```

**Trimming whitespace:**

```lisp
lfe> (string:trim "  hello  ")
"hello"
lfe> (string:trim "  hello  " 'leading)
"hello  "
lfe> (string:trim "  hello  " 'trailing)
"  hello"
```

**Splitting and joining:**

```lisp
lfe> (string:split "one,two,three" "," 'all)
("one" "two" "three")
lfe> (string:split "a:b:c:d" ":")
("a" "b:c:d")
lfe> (string:split "a:b:c:d" ":" 'all)
("a" "b" "c" "d")

lfe> (string:join '("one" "two" "three") ", ")
"one, two, three"
```

**Searching:**

```lisp
lfe> (string:find "hello world" "world")
"world"
lfe> (string:find "hello world" "universe")
'nomatch

lfe> (string:str "hello world" "world")
7
lfe> (string:str "hello world" "universe")
0
```

Note that `string:str` returns 1-based position or 0 if not found.

**Replacing:**

```lisp
lfe> (string:replace "hello world" "world" "universe")
"hello universe"
lfe> (string:replace "a,b,c,d" "," ":" 'all)
"a:b:c:d"
```

**Checking prefixes and suffixes:**

```lisp
lfe> (string:prefix "hello world" "hello")
"world"
lfe> (string:prefix "hello world" "goodbye")
'nomatch

lfe> (lists:prefix "hello" "hello world")
true
lfe> (lists:prefix "goodbye" "hello world")
false

lfe> (lists:suffix "world" "hello world")
true
```

### String Predicates

Testing for empty strings:

```lisp
lfe> (string:is_empty "")
true
lfe> (string:is_empty "hello")
false
```

Testing if all characters match a predicate:

```lisp
lfe> (lists:all (lambda (c) (and (>= c #\a) (=< c #\z))) "hello")
true
lfe> (lists:all (lambda (c) (and (>= c #\a) (=< c #\z))) "Hello")
false
```

### Formatting Strings

The `format` family of functions provides powerful string interpolation:

```lisp
lfe> (io_lib:format "The answer is ~p" '(42))
"The answer is 42"

lfe> (io_lib:format "~s ~s ~p" '("Hello" "world" 123))
"Hello world 123"

lfe> (io_lib:format "~.2f" '(3.14159))
"3.14"

lfe> (io_lib:format "~10.2.0f" '(3.14159))
"      3.14"
```

Common format specifiers:
- `~p` — pretty-print any term
- `~s` — string
- `~w` — write in Erlang term format
- `~c` — character
- `~f` — float
- `~e` — exponential notation
- `~.Nf` — float with N decimal places
- `~n` — newline

For console output, use `lfe_io:format` or `io:format`:

```lisp
lfe> (lfe_io:format "Hello, ~s!~n" '("World"))
Hello, World!
ok

lfe> (io:format "The answer is ~p~n" '(42))
The answer is 42
ok
```

### Converting Between Types

**Atoms to strings:**

```lisp
lfe> (atom_to_list 'hello)
"hello"
lfe> (atom_to_list 'hello-world)
"hello-world"
```

**Strings to atoms:**

```lisp
lfe> (list_to_atom "hello")
hello
lfe> (list_to_existing_atom "hello")
hello
```

<div class="alert alert-warning">
  <h4 class="alert-heading">
    <i class="fa fa-exclamation-triangle" aria-hidden="true"></i>
    Caution: Atom Creation
  </h4>
  <p class="mb-0">
    Remember that atoms are not garbage collected. Use <code>list_to_atom</code> cautiously with user input, as creating too many atoms can exhaust the atom table and crash the VM. Prefer <code>list_to_existing_atom</code> when you expect the atom to already exist.
  </p>
</div>

**Numbers to strings:**

```lisp
lfe> (integer_to_list 42)
"42"
lfe> (integer_to_list 42 16)
"2A"

lfe> (float_to_list 3.14159)
"3.14159000000000009237e+00"
lfe> (float_to_list 3.14159 '(#(decimals 2)))
"3.14"
```

**Strings to numbers:**

```lisp
lfe> (list_to_integer "42")
42
lfe> (list_to_integer "2A" 16)
42

lfe> (list_to_float "3.14159")
3.14159
```

**Binaries to strings:**

```lisp
lfe> (binary_to_list #"Hello")
"Hello"
lfe> (list_to_binary "Hello")
#"Hello"
```

### Unicode Support

LFE strings can contain Unicode characters, though the integer representation may be surprising:

```lisp
lfe> "Hello, 世界"
"Hello, 世界"

lfe> (length "Hello, 世界")
9

lfe> (lists:nth 8 "Hello, 世界")
19990
```

For proper Unicode string handling, especially with grapheme clusters, use the Unicode-aware functions in the `string` module:

```lisp
lfe> (string:length "Hello, 世界")
9

lfe> (string:slice "Hello, 世界" 7 2)
"世界"
```

The modern `string` module (Erlang 20+) handles Unicode correctly and should be preferred over older list-based string functions when working with international text.

### Common Patterns

**Building strings incrementally:**

```lisp
(defun build-message (name age)
  (++ "Name: " name ", Age: " (integer_to_list age)))
```

**Processing each character:**

```lisp
(defun count-vowels
  (("") 0)
  (((cons c rest))
   (let ((is-vowel (lists:member c "aeiouAEIOU")))
     (if is-vowel
         (+ 1 (count-vowels rest))
         (count-vowels rest)))))
```

**Filtering characters:**

```lisp
(defun remove-spaces
  (("") "")
  (((cons #\space rest)) (remove-spaces rest))
  (((cons c rest)) (cons c (remove-spaces rest))))
```

**Using list comprehensions on strings:**

```lisp
lfe> (lc ((<- c "hello")) (- c 32))
"HELLO"

lfe> (lc ((<- c "hello world") (=/= c #\space)) c)
"helloworld"
```

### Performance Notes

String operations have the same performance characteristics as list operations because they *are* list operations:

- Concatenation with `++` is O(n) in the length of the first string
- Repeated concatenation in a loop can be O(n²)
- For building large strings, accumulate in a list and call `lists:flatten` once
- Consider using binaries for large text or when performance is critical

Example of efficient string building:

```lisp
(defun build-csv-row (items)
  (lists:flatten
    (lists:join "," 
      (lists:map #'format-item/1 items))))
```

### Summary

Strings in LFE are lists of character codes—simple, elegant, and slightly peculiar if you're new to this representation. This design choice means:

- All list operations work on strings
- Strings naturally integrate with pattern matching
- String manipulation is intuitive if you think in terms of list processing
- Performance characteristics match those of lists
- Unicode requires some care but is fully supported

For most string operations in modern code, prefer the `string` module's Unicode-aware functions. For performance-critical text processing with large strings, consider using binaries (covered in a later chapter). But for everyday string manipulation, list-based strings remain a perfectly reasonable and often elegant choice.

Remember: in LFE, as in life, it's not about having a separate type for everything—it's about recognizing that sometimes, a list of integers masquerading as text is exactly what you need.