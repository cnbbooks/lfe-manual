# clj.lfe - Clojure Compatibility

**Purpose**: Provide Clojure-style macros and functions for LFE, including threading macros.

**Location**: `src/clj.lfe`
**Size**: 842 LOC, 26KB

**Module Classification**: Compatibility layer, Clojure macros

This is the **7th largest module** in the codebase.

## Exported Macros (40+)

**Threading Macros**:

```lisp
(-> x forms...)              ; Thread-first
(->> x forms...)             ; Thread-last
(as-> x name forms...)       ; Thread with explicit name
```

**Examples**:

```lisp
(-> 5
    (+ 3)
    (* 2)
    (- 1))
→ (- (* (+ 5 3) 2) 1) → 15

(->> '(1 2 3)
     (map (lambda (x) (* x 2)))
     (filter (lambda (x) (> x 2))))
→ (2 4 6) filtered to (4 6)

(as-> 10 x
      (+ x 5)
      (* x 2)
      (list x x))
→ (list (* (+ 10 5) 2) (* (+ 10 5) 2)) → (30 30)
```

**Conditional Macros**:

```lisp
(if-let [binding test] then else)
(when-let [binding test] body...)
(if-not test then else)
(when-not test body...)
(cond clauses...)
```

**Looping Macros**:

```lisp
(doseq [var sequence] body...)
(dotimes [var n] body...)
(while test body...)
```

**Destructuring**:

```lisp
(let [[a b & rest] '(1 2 3 4 5)]
  (list a b rest))
→ (1 2 (3 4 5))

(let [{:keys [name age]} #M(name "Alice" age 30)]
  (list name age))
→ ("Alice" 30)
```

**Collection Functions**:

```lisp
(get map key [default])      ; Map lookup
(assoc map k v ...)          ; Add/update keys
(dissoc map k ...)           ; Remove keys
(conj coll item ...)         ; Add to collection
(into to from)               ; Pour from into to
(merge map ...)              ; Merge maps
```

**Sequence Functions**:

```lisp
(comp f g ...)               ; Function composition
(partial f args ...)         ; Partial application
(complement f)               ; Negate predicate
(constantly x)               ; Constant function
```

## Threading Macro Implementation

**From `clj.lfe`**:

```lisp
(defmacro -> (x . forms)
  "Thread-first: insert X as first argument in each form."
  (fletrec ((thread
              ([y ()] y)
              ([y ((cons f args) . rest)]
               (thread (list* f y args) rest))
              ([y (f . rest)] (when (is_atom f))
               (thread (list f y) rest))))
    (thread x forms)))

(defmacro ->> (x . forms)
  "Thread-last: insert X as last argument in each form."
  (fletrec ((thread
              ([y ()] y)
              ([y ((cons f args) . rest)]
               (thread (++ (list f) args (list y)) rest))
              ([y (f . rest)] (when (is_atom f))
               (thread (list f y) rest))))
    (thread x forms)))
```

## Dependencies

**LFE core**: Heavy use of LFE macros and special forms.

## Used By

- User code preferring Clojure style
- Porting Clojure code to LFE

## Special Considerations

**Macro Heavy**: Almost all exports are macros (compile-time transformations).

**Map-Centric**: Many functions designed for LFE maps (Erlang maps).

**Immutability**: Follows Clojure's immutable data philosophy (natural fit for Erlang/LFE).

**Performance**: Threading macros have zero runtime overhead (pure macro expansion).
