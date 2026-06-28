# lfe_lint.erl - Semantic Analyzer

**Purpose**: Validate LFE code for semantic correctness. This is the **largest** module in the codebase.

**Location**: `src/lfe_lint.erl`
**Size**: 2,532 LOC, 94KB

**Module Classification**: Compiler core, validation layer

#### Public API

```erlang
module(Forms) -> {ok, Warnings}
               | {error, Errors, Warnings}
module(Forms, CompilerInfo) -> Result
```

Lint an entire module. Located at `lfe_lint.erl:109-115`.

```erlang
format_error(Error) -> Chars
```

Format lint errors for display. Located at `lfe_lint.erl:128-297`.

#### Lint State

**State Record** (lines 58-79):

```erlang
-record(lfe_lint, {
    module=[],              % Module name
    mline=0,               % Module definition line
    exports=orddict,        % Exported functions
    imports=orddict,        % Imported functions
    aliases=orddict,        % Module aliases
    onload=[],             % On-load function
    funcs=orddict,         % Defined functions
    types=[],              % Type definitions
    texps=orddict,         % Exported types
    specs=[],              % Function specs
    records=orddict,       % Record definitions
    struct=undefined,      % Struct definition
    env=[],                % Current environment
    func=[],               % Current function
    file="no file",        % Source file
    opts=[],               % Compiler options
    errors=[],             % Accumulated errors
    warnings=[]            % Accumulated warnings
}).
```

#### Validation Checks

**Module-Level Checks** (`module_forms/2` at lines 322-401):

1. **Module Definition**:
   - Exactly one `define-module` required
   - Module name must be atom
   - Valid attributes only

2. **Exports**:
   - Exported functions must be defined
   - Duplicate exports detected
   - Export syntax validation

3. **Imports**:
   - Import syntax validation
   - Conflicting imports detected
   - Can't import and define same function

4. **Attributes**:
   - Valid attribute names
   - Attribute value types
   - Duplicates detected

**Function-Level Checks** (`check_function/3` at lines 673-736):

1. **Function Definitions**:
   - Arity consistency across clauses
   - Duplicate definitions
   - Redefining imports
   - Redefining core forms

2. **Variable Bindings**:
   - Unbound variables detected
   - Duplicate bindings in patterns
   - Shadowing warnings

3. **Pattern Matching**:
   - Illegal patterns
   - Improper list patterns
   - Binary segment validation
   - Map key patterns

**Expression Checks** (`check_expr/2` at lines 891-1105):

1. **Special Forms**:
   - Correct special form syntax
   - Argument counts
   - Valid sub-expressions

2. **Function Calls**:
   - Undefined function calls
   - Arity mismatches
   - Core function validation

3. **Guards**:
   - Only guard-safe expressions
   - No function calls in guards (except BIFs)
   - Type test validity

**Type and Spec Checks** (`check_type_def/2` at lines 1406-1593):

1. **Type Definitions**:
   - Valid type syntax
   - Undefined type references
   - Recursive type detection
   - Type variable usage

2. **Function Specs**:
   - Spec matches function arity
   - Type validity in specs
   - Return type specified

**Record and Struct Checks** (`check_record_def/2` at lines 1651-1722):

1. **Record Definitions**:
   - Valid field names
   - Default value types
   - Duplicate field names

2. **Struct Definitions**:
   - One struct per module
   - Valid field list

#### Error Categories

**Errors** (compilation fails):

- Undefined functions
- Arity mismatches
- Invalid patterns
- Unbound variables
- Syntax errors in forms

**Warnings** (compilation succeeds):

- Unused variables
- Shadowed variables
- Deprecated features
- Unused functions (with opt-in flag)

#### Dependencies

**LFE modules**:

- `lfe_env` - Heavy use for tracking bindings
- `lfe_internal` - Form validation
- `lfe_lib` - Utilities
- `lfe_bits` - Binary segment validation

**Erlang stdlib**:

- `lists`, `orddict`, `ordsets`

#### Used By

- `lfe_comp` - Compilation pipeline

#### Key Algorithms

**Environment Tracking**:

The linter maintains an environment (`lfe_env`) to track:

- Variable bindings in scope
- Function definitions
- Macro definitions (though macros are already expanded)
- Record definitions

Example (`check_let/3` at lines 1108-1145):

```erlang
check_let([Bindings|Body], Env, St) ->
    % Check binding expressions
    {Env1, St1} = check_bindings(Bindings, Env, St),
    % Check body with new environment
    {_Env2, St2} = check_body(Body, Env1, St1),
    {Env, St2}.  % Return original env (let is scoped)
```

**Pattern Validation** (`check_pat/2` at lines 1737-1886):

Patterns are checked for:

- Valid constructors (cons, tuple, binary, map, record)
- Literal values
- Variable bindings (no duplicates)
- Proper nesting

**Guard Validation** (`check_guard/2` at lines 1941-2014):

Guards can only contain:

- BIFs from `lfe_internal:is_guard_func/2`
- Comparisons
- Boolean operators
- Type tests
- Arithmetic

**No** user-defined functions allowed in guards.

#### Special Considerations

**Complexity**:

This is the most complex module in LFE:

- 2,532 LOC
- Handles all language constructs
- Deep pattern matching
- Environment threading

**Performance**:

Linting is ~5-10% of compilation time (much less than macro expansion or Erlang compilation).

**Error Messages**:

The `format_error/1` function (lines 128-297) provides user-friendly error messages with context.

**Conservative Validation**:

The linter may produce false positives (e.g., flagging valid dynamic code as errors) but avoids false negatives.
