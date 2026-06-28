# cl.lfe - Common Lisp Compatibility

**Purpose**: Provide Common Lisp-style functions and macros for LFE.

**Location**: `src/cl.lfe`
**Size**: 767 LOC, 21KB

**Module Classification**: Compatibility layer, CL functions

This is the **9th largest module** in the codebase.

## Exported Functions (60+)

**List Functions**:

```lisp
(adjoin item list)           ; Add if not member
(acons key value alist)      ; Add to association list
(pairlis keys values)        ; Create association list
(assoc key alist)            ; Association list lookup
(rassoc value alist)         ; Reverse assoc lookup
(subst new old tree)         ; Substitute in tree
(sublis alist tree)          ; Multiple substitutions
(union list1 list2)          ; Set union
(intersection list1 list2)   ; Set intersection
(set-difference list1 list2) ; Set difference
(member item list)           ; Membership test (returns tail)
(remove item list)           ; Remove all occurrences
(remove-if pred list)        ; Remove matching
(delete item list)           ; Destructive remove
```

**Tree Functions**:

```lisp
(tree-equal tree1 tree2)     ; Deep equality
(subst new old tree)         ; Substitute in tree
(copy-tree tree)             ; Deep copy
```

**Sequence Functions**:

```lisp
(some pred list)             ; Any element satisfies
(every pred list)            ; All elements satisfy
(notany pred list)           ; No element satisfies
(notevery pred list)         ; Not all satisfy
(reduce func list)           ; Fold
(fill list item)             ; Fill with item
(replace list1 list2)        ; Replace elements
```

**Mapping Functions**:

```lisp
(mapcar func list)           ; Map (same as lists:map)
(maplist func list)          ; Map over tails
(mapc func list)             ; Map for side effects
(mapcan func list)           ; Map and concatenate
```

**Property Lists**:

```lisp
(get-properties plist indicators) ; Get first matching property
(getf plist key [default])        ; Get property value
(putf plist key value)            ; Set property value
(remf plist key)                  ; Remove property
```

## Implementation Style

Written in **pure LFE** (not Erlang).

**Example Function** (from `cl.lfe`):

```lisp
(defun adjoin (item list)
  "Add ITEM to LIST if not already member."
  (if (member item list)
      list
      (cons item list)))

(defun pairlis (keys values)
  "Make association list from KEYS and VALUES."
  (zipwith #'tuple/2 keys values))

(defun union (list1 list2)
  "Set union of LIST1 and LIST2."
  (++ list1 (foldl (lambda (elem acc)
                      (if (member elem list1)
                          acc
                          (cons elem acc)))
                    '()
                    list2)))
```

## Dependencies

**Erlang stdlib**:

- `lists`, `ordsets`

**LFE core functions**: Uses LFE's built-in functions heavily.

## Used By

- User code wanting Common Lisp-style functions
- Porting CL code to LFE

## Special Considerations

**Naming**: Function names follow Common Lisp conventions (kebab-case, CL names).

**Semantics**: Attempts to match CL semantics where reasonable, but adapted to Erlang/LFE.

**Performance**: Some functions less efficient than Erlang equivalents (e.g., `member` walks entire list).
