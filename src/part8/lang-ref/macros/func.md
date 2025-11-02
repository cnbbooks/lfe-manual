# Function Shortcuts

| Macro | Description | Example |
|-------|-------------|---------|
| `fun` | Lambda shorthand | `#'square/1` → `(function square 1)` |

**Syntax Variations**:

```lisp
;; Function reference
#'func/2              ; Local function
#'module:func/2       ; Remote function

;; In higher-order functions
(lists:map #'square/1 '(1 2 3))
(lists:filter #'even?/1 '(1 2 3 4))
```
