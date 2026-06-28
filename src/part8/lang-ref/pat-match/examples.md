# Pattern Examples

**List Patterns**:

```lisp
;; Empty list
(case x
  ('() 'empty))

;; Cons pattern
(case x
  ((cons head tail) head))

;; List pattern (fixed length)
(case x
  ((list a b c) (tuple a b c)))

;; List pattern with tail
(case x
  ((list first second . rest)
   (tuple first second rest)))
```

**Tuple Patterns**:

```lisp
;; Fixed structure
(case result
  ((tuple 'ok value) value)
  ((tuple 'error reason) reason))

;; Nested tuples
(case x
  ((tuple 'person name (tuple 'address city state))
   (tuple city state)))

;; Size matching
(case x
  ((tuple _ _) 'pair)
  ((tuple _ _ _) 'triple))
```

**Binary Patterns**:

```lisp
;; Fixed-size header
(case packet
  ((binary (type (size 8))
           (length (size 16))
           (payload (binary)))
   (tuple type length payload)))

;; String prefix
(case bin
  ((binary <<"GET ">> (rest (binary)))
   (handle-get rest))
  ((binary <<"POST ">> (rest (binary)))
   (handle-post rest)))

;; Bit-level matching
(case bits
  ((binary (flag (size 1))
           (value (size 7)))
   (tuple flag value)))
```

**Map Patterns**:

```lisp
;; Match specific keys
(case person
  ((map 'name n 'age a)
   (io:format "~s is ~p years old~n" (list n a))))

;; Match subset of keys
(case config
  ((map 'port p)
   p)
  (_ 8080))  ; Default

;; Nested maps
(case data
  ((map 'user (map 'name n))
   n))
```

**Alias Patterns**:

```lisp
;; Bind to both parts and whole
(case list
  ((= (cons h t) full)
   (tuple h t full)))

;; Multiple aliases
(case x
  ((= (tuple a b) (= t1 (= t2 _)))
   (tuple a b t1 t2)))
```
