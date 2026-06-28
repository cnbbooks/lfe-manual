# Type Predicates

| Predicate | Tests For |
|-----------|-----------|
| `is_atom/1` | Atom |
| `is_binary/1` | Binary |
| `is_bitstring/1` | Bitstring |
| `is_boolean/1` | Boolean (true/false) |
| `is_float/1` | Float |
| `is_function/1` | Function |
| `is_function/2` | Function of specific arity |
| `is_integer/1` | Integer |
| `is_list/1` | List |
| `is_map/1` | Map |
| `is_number/1` | Number (int or float) |
| `is_pid/1` | Process identifier |
| `is_port/1` | Port |
| `is_reference/1` | Reference |
| `is_tuple/1` | Tuple |

**Usage in Guards**:

```lisp
(defun process (x)
  (cond
    ((is_atom x) (process-atom x))
    ((is_list x) (process-list x))
    ((is_tuple x) (process-tuple x))
    ('true 'unknown)))
```
