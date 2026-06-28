# Record Macro Generation

**One `define-record` generates 9+ macros**:

```lisp
(define-record person name age city)

;; Generates:
make-person             % Constructor
person?                 % Type predicate
person-name             % Field accessor
person-age              % Field accessor
person-city             % Field accessor
set-person-name         % Field setter
set-person-age          % Field setter
set-person-city         % Field setter
person-name-index       % Field index (for element/2)
```

**Implementation** (src/lfe_macro_record.erl):

Each record definition is stored in environment, and accessor macros are generated as wrappers around tuple operations:

```lisp
;; person-name expands to:
(element 2 person-record)  % name is field 2 (1-indexed after record tag)
```

**Usage**:

```lisp
(let ((p (make-person name "Alice" age 30 city "NYC")))
  (person-name p))  ; → "Alice"
```
