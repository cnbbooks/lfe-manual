# Pattern Types

| Pattern Type | Syntax | Description | Example |
|--------------|--------|-------------|---------|
| **Literal** | `42`, `'atom`, `"string"` | Match exact value | `(case x (42 'found))` |
| **Variable** | `x`, `name` | Bind to any value | `(case x (n n))` |
| **Don't-care** | `_` | Match anything, don't bind | `(case x (_ 'matched))` |
| **Cons** | `(cons h t)` | Match list head/tail | `(case l ((cons h t) h))` |
| **List** | `(list a b c)` | Match list of specific length | `(case x ((list a b) a))` |
| **Tuple** | `(tuple 'ok value)` | Match tuple structure | `(case x ((tuple 'ok v) v))` |
| **Binary** | `(binary (x (size 8)))` | Match binary segments | See below |
| **Map** | `(map 'key value)` | Match map with key | `(case m ((map 'a v) v))` |
| **Alias** | `(= pattern1 pattern2)` | Match both patterns | `(= (cons h t) full-list)` |
