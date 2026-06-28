# Strings

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

## Creating Strings

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

## String Operations

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

## String Predicates

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

## Formatting Strings

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

## Converting Between Types

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

## Unicode Support

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

## Common Patterns

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

## Performance Notes

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

## Summary

Strings in LFE are lists of character codes—simple, elegant, and slightly peculiar if you're new to this representation. This design choice means:

- All list operations work on strings
- Strings naturally integrate with pattern matching
- String manipulation is intuitive if you think in terms of list processing
- Performance characteristics match those of lists
- Unicode requires some care but is fully supported

For most string operations in modern code, prefer the `string` module's Unicode-aware functions. For performance-critical text processing with large strings, consider using binaries (covered in a later chapter). But for everyday string manipulation, list-based strings remain a perfectly reasonable and often elegant choice.

Remember: in LFE, as in life, it's not about having a separate type for everything—it's about recognizing that sometimes, a list of integers masquerading as text is exactly what you need.
