# lfe_macro_record.erl - Record Macro Generator

**Purpose**: Generate accessor macros for record definitions.

**Location**: `src/lfe_macro_record.erl`
**Size**: 122 LOC, 4.5KB

**Module Classification**: Compiler support, code generation

#### Public API

```erlang
define(Name, FieldDefs, Env, Line) ->
    {[{Name, Macros}], Env}
```

Generate record macros. Located at `lfe_macro_record.erl:44-56`.

#### Generated Macros

For a record definition:

```lisp
(define-record person
  name
  age
  (city "Unknown"))  ; With default
```

**9+ Macros Generated**:

1. **`make-person`** - Constructor

   ```lisp
   (make-person name "Alice" age 30)
   → (tuple 'person "Alice" 30 "Unknown")
   ```

2. **`is-person`** - Type test

   ```lisp
   (is-person x)
   → (andalso (is_tuple x) (=:= (tuple-size x) 4) (=:= (element 1 x) 'person))
   ```

3. **`match-person`** - Pattern matcher

   ```lisp
   (match-person name n age a)
   → (= (tuple 'person n a _) x)
   ```

4. **`update-person`** - Updater

   ```lisp
   (update-person p age 31 city "NYC")
   → (setelement 3 (setelement 4 p "NYC") 31)
   ```

5. **`set-person`** - Updater (deprecated alias for `update-person`)

6. **`fields-person`** - Field list

   ```lisp
   (fields-person)
   → (list 'name 'age 'city)
   ```

7. **`size-person`** - Record size

   ```lisp
   (size-person)
   → 4
   ```

8. **`person-field`** (one per field) - Getter

   ```lisp
   (person-name p)    → (element 2 p)
   (person-age p)     → (element 3 p)
   ```

8. **`set-person-field`** (one per field) - Setter

   ```lisp
   (set-person-name p "Bob")
   → (setelement 2 p "Bob")
   ```

#### Tuple Layout

Records compile to tuples with tag as first element:

```lisp
(make-person name "Alice" age 30)
→ #(person "Alice" 30 "Unknown")
  ^^^^^^  ^^^^^^^  ^^   ^^^^^^^^^
  tag     name     age  city
```

**Field Indexing**: 1-based (element 1 is tag, element 2 is first field, etc.)

#### Erlang Compatibility

LFE records are **identical** to Erlang records:

```erlang
% Erlang code can use LFE records:
-include("person.hrl").  % If generated

P = {person, "Alice", 30, "Unknown"},
Name = P#person.name,   % "Alice"
```

#### Dependencies

- `lfe_lib` - Utilities
- `lists` - List operations

#### Used By

- `lfe_macro` - Expands `defrecord` forms

#### Special Considerations

**Macro Count**:

For an N-field record:

- Base macros: 7
- Field accessors: N getters + N setters
- **Total**: 7 + 2N macros

For 10-field record: **27 macros** generated!

**Naming Conflicts**:

If you define:

```lisp
(define-record foo bar)
```

The following names are reserved:

- `make-foo`, `is-foo`, `match-foo`, `update-foo`, `set-foo`, `fields-foo`, `size-foo`
- `foo-bar`, `set-foo-bar`

**Default Values**:

```lisp
(define-record person
  name           ; No default (will be 'undefined' if not specified)
  (age 0)        ; Default: 0
  (city "Stockholm"))  ; Default: "Stockholm"
```

**Performance**: Record access compiles to direct `element/2` calls - very fast.
