# Compound Types

| Type | Syntax | Example | Characteristics |
|------|--------|---------|-----------------|
| **List** | `'(...)` or `(list ...)` | `'(1 2 3)` | Linked list, O(n) access |
| **Improper List** | `(cons a b)` | `(cons 1 2)` → `(1 . 2)` | Cons cell |
| **Tuple** | `#(...)` or `(tuple ...)` | `#(ok 42)` | Fixed array, O(1) access |
| **Map** | `#M(...)` or `(map ...)` | `#M(a 1 b 2)` | Hash map, O(log n) access |
| **Record** | `(make-rec ...)` | See records | Named tuple (macro) |
| **Struct** | `(struct mod ...)` | See structs | Map with **struct** key |

**Lists**:

```lisp
;; Empty list
'()

;; Simple list
'(1 2 3)
(list 1 2 3)

;; Nested lists
'((1 2) (3 4) (5 6))

;; Improper list (dotted pair)
(cons 1 2)           ; → (1 . 2)
(cons 1 (cons 2 3))  ; → (1 2 . 3)

;; List with tail
(list* 1 2 '(3 4))   ; → (1 2 3 4)
```

**Tuples**:

```lisp
;; Literal syntax
#(a b c)

;; Constructor
(tuple 'a 'b 'c)

;; Tagged tuples (common pattern)
#(ok 42)
#(error "not found")

;; Nested
#(person "Alice" #(address "NYC" "NY"))
```

**Maps**:

```lisp
;; Literal syntax
#M(a 1 b 2)

;; Constructor
(map 'a 1 'b 2)

;; Empty map
#M()
(map)

;; Nested maps
#M(user #M(name "Alice" age 30)
   status 'active)

;; Keys can be any term
(map 1 'a 2 'b (tuple 'x 'y) 'c)
```

**Records** (macros generate):

```lisp
;; Define
(defrecord person
  name
  age
  (city "Unknown"))

;; Create
(make-person name "Alice" age 30)
; → #(person "Alice" 30 "Unknown")

;; Access
(person-name p)                    ; Get name
(set-person-age p 31)              ; Set age (returns new record)

;; Pattern match
(let (((match-person name n age a) p))
  (tuple n a))

;; Update
(update-person p age 31 city "NYC")
```

**Structs** (map-based):

```lisp
;; Define
(defmodule person
  (defstruct [name age city]))

;; Create
(person:__struct__ '(name "Alice" age 30))
; → #M(__struct__ person name "Alice" age 30 city undefined)

;; Access (via maps)
(map-get 'name person)
```
