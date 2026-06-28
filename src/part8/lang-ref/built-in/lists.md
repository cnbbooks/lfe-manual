# List Operations

| Function | Signature | Description | Example |
|----------|-----------|-------------|---------|
| `list` | `(list e1 ...)` | Construct list | `(list 1 2 3)` |
| `list*` | `(list* e1 ... tail)` | List with explicit tail | `(list* 1 2 '(3 4))` → `(1 2 3 4)` |
| `append` | `(++ l1 l2)` | Concatenate lists | `(++ '(1 2) '(3 4))` → `(1 2 3 4)` |
| `length` | `(length list)` | List length | `(length '(a b c))` → `3` |
| `reverse` | `(lists:reverse list)` | Reverse list | `(lists:reverse '(1 2 3))` → `(3 2 1)` |
| `member` | `(lists:member elem list)` | Membership test | `(lists:member 2 '(1 2 3))` → `true` |
| `nth` | `(lists:nth n list)` | Get nth element (1-indexed) | `(lists:nth 2 '(a b c))` → `b` |

**Note**: Most list functions are in the `lists` module ([Erlang stdlib](https://www.erlang.org/doc/apps/stdlib/lists.html)).
