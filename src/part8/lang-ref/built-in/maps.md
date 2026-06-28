# Map Operations

| Function | Syntax | Description |
|----------|--------|-------------|
| `map` | `(map k1 v1 ...)` | Create map |
| `map-size` | `(map-size m)` | Map size |
| `map-get` | `(map-get k m)` or `(maps:get k m)` | Get value |
| `map-set` | `(map-set m k v ...)` | Set keys (returns new map) |
| `map-update` | `(map-update m k v ...)` | Update existing keys |
| `maps:put` | `(maps:put k v m)` | Add/update single key |
| `maps:remove` | `(maps:remove k m)` | Remove key |

**Map Examples**:

```lisp
;; Create
(let ((m (map 'a 1 'b 2)))
  (map-size m))  ; → 2

;; Access
(map-get 'a (map 'a 1 'b 2))      ; → 1
(maps:get 'c (map 'a 1) 'default) ; → default

;; Update
(map-set (map 'a 1) 'b 2 'c 3)    ; → #{a => 1, b => 2, c => 3}

;; Pattern matching
(case m
  ((map 'a v) v)      ; Match if key 'a exists, bind value
  (_ 'not-found'))
```
