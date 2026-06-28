# Stage 4: Semantic Analysis (lfe_lint.erl)

**Purpose**: Validate code for semantic correctness without executing it.

**Module**: `lfe_lint.erl` (2,532 LOC - **largest module**) - Location: `src/lfe_lint.erl`

**Lint state**:

```erlang
-record(lfe_lint, {
    module = [],              % Module name
    mline = 0,               % Module definition line
    exports = orddict:new(), % Exported function-line
    imports = orddict:new(), % Imported function-{module,func}
    aliases = orddict:new(), % Module-alias
    onload = [],             % Onload function
    funcs = orddict:new(),   % Defined function-line
    types = [],              % Known types
    texps = orddict:new(),   % Exported types
    specs = [],              % Known func specs
    records = orddict:new(), % Record definitions
    struct = undefined,      % Struct definition
    env = [],                % Top-level environment
    func = [],               % Current function
    file = "no file",        % File name
    opts = [],               % Compiler options
    errors = [],             % Errors
    warnings = []            % Warnings
}).
```

**What gets checked** (comprehensive list from src/lfe_lint.erl:200-500):

**1. Module Definition:**

- Bad attributes
- Bad metadata
- Redefining imports/exports
- Multiple module definitions

**2. Functions:**

- Undefined functions (called but not defined)
- Redefining functions
- Redefining core forms/BIFs
- Importing then defining same function (conflict)
- Head arity mismatches (clauses with different arities)
- Bad function definitions

**3. Variables:**

- Unbound symbols (used but not bound)
- Multiple variable definitions in patterns (same var bound twice)
- Unused variables (bound but never used) - warning only

**4. Patterns:**

- Illegal patterns (non-pattern forms in pattern position)
- Bad pattern aliases (`=` operator misuse)
- Invalid record patterns
- Invalid binary patterns

**5. Guards:**

- Illegal guard expressions (only BIFs allowed)
- Bad guard forms
- Non-boolean guard results

**6. Records/Structs:**

- Undefined records/structs (used but not defined)
- Undefined fields (accessing non-existent fields)
- Bad definitions (malformed define-record/define-struct)
- Redefining records/structs

**7. Types/Specs:**

- Undefined types (type references to non-existent types)
- Redefining types
- Bad type definitions (syntax errors)
- Singleton type variables (defined but used only once) - warning only
- Spec arity mismatch (spec doesn't match function arity)

**8. Operators:**

- Illegal operator calls (operators used incorrectly)

**9. Literals:**

- Illegal literal values
- Illegal map keys (non-constant keys)
- Illegal bit segments (invalid bitstring specifications)

**Example error detection**:

```lisp
;; Unbound variable
(defun foo (x)
  (+ x y))  % ERROR: unbound symbol y

;; Undefined function
(defun bar ()
  (baz 42))  % ERROR: undefined function baz/1

;; Arity mismatch
(defun multi
  ([x] x)
  ([x y] (+ x y))
  ([x] (* x 2)))  % ERROR: clause arity mismatch

;; Pattern error
(defun bad ([x x] x))  % ERROR: variable x bound multiple times
```
