# lfe_io_pretty.erl - Pretty Printer

**Purpose**: Write LFE terms in formatted, indented, multi-line layout for readability.

**Location**: `src/lfe_io_pretty.erl`
**Size**: 405 LOC, 15KB

**Module Classification**: I/O support, pretty formatting

#### Public API

```erlang
term(Term) -> [char()]
term(Term, Depth) -> [char()]
term(Term, Depth, Indentation, LineLength) -> [char()]
```

Pretty print term with specified formatting parameters. Located at `lfe_io_pretty.erl:47-49`.

#### Formatting Strategy

**Layout Modes**:

1. **Horizontal** - Single line: `(a b c)`
2. **Vertical** - Multi-line with indentation:

   ```lisp
   (defun foo (x)
     (let ((y (* x 2)))
       (+ y 1)))
   ```

**Line Length Tracking**:

The pretty printer tries to fit output within `LineLength` (default 80) by:

1. Attempting horizontal layout
2. Falling back to vertical if too long
3. Intelligent indentation based on form type

**Special Form Formatting** (`format_form/4` at lines 178-267):

Different forms get custom layouts:

```lisp
; Function definitions - indent body
(defun name (args)
  body)

; Let bindings - align bindings
(let ((x 1)
      (y 2))
  body)

; Conditionals - indent branches
(if test
    then-branch
    else-branch)

; Case - indent clauses
(case expr
  (pattern1 result1)
  (pattern2 result2))
```

#### Indentation Rules

**By Form Type**:

- `defun`, `defmacro` - Indent 2 after function head
- `let`, `let*`, `letrec` - Align binding pairs
- `if`, `cond` - Indent branches
- `lambda` - Indent body
- Default lists - Indent 1 from opening paren

**Column Tracking** (`write_tail/4` at lines 298-356):

Maintains current column position to:

- Decide horizontal vs vertical layout
- Calculate indentation
- Enforce line length limits

#### Dependencies

- `lists`, `io_lib`, `string` (Erlang stdlib)

#### Used By

- `lfe_io` - Via `prettyprint1/*`
- `lfe_shell` - Value display
- `lfe_error` - Error formatting

#### Special Considerations

**Performance**: Slower than compact writing due to layout calculations.

**Configurable**: LineLength and initial indentation customizable.

**Readable Output**: Prioritizes human readability over machine parsing.
