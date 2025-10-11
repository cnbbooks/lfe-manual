# lfe_codelift.erl - Lambda Lifter

**Purpose**: Transform closures into top-level functions by lifting nested lambdas and converting them to pass captured variables as explicit arguments.

**Location**: `src/lfe_codelift.erl`
**Size**: 785 LOC, 30KB

**Module Classification**: Compiler transformation, closure conversion

#### Public API

```erlang
lift(Forms, State) -> {ok, Forms, State}
                    | {error, Errors, Warnings, State}
```

Lift all lambdas in forms to top-level. Located at `lfe_codelift.erl:82-88`.

#### Lambda Lifting Algorithm

**Process**:

1. **Identify Closures**: Find all `lambda` and `match-lambda` forms
2. **Capture Analysis**: Determine which free variables are captured
3. **Generate Top-Level Function**: Create new function with captured vars as parameters
4. **Transform Call Sites**: Replace lambda with call to lifted function passing captured vars

**Example**:

```lisp
(defun make-adder (x)
  (lambda (y) (+ x y)))

; After lifting:

(defun make-adder (x)
  (lambda (y) (-lfe-make-adder-lambda-1 x y)))

(defun -lfe-make-adder-lambda-1 (x y)
  (+ x y))
```

#### Closure Representation

**Before Lifting**:

```lisp
[lambda [y] [+ x y]]  ; x is free (captured)
```

**After Lifting**:

```lisp
; Lambda becomes call to lifted function
[lambda [y] [-lfe-outer-lambda-1 x y]]

; Lifted function
[-lfe-outer-lambda-1 [x y] [+ x y]]
```

#### Captured Variable Analysis

**Free Variable Detection** (`free_vars/2` at lines 234-389):

```erlang
free_vars([lambda, Args, Body], Env) ->
    % Variables in Args are bound
    BodyFree = free_vars(Body, add_bindings(Args, Env)),
    % Remove Args from free vars
    subtract(BodyFree, Args);

free_vars([let, Bindings, Body], Env) ->
    BindingFree = free_vars_bindings(Bindings, Env),
    BodyFree = free_vars(Body, add_bindings(bound_vars(Bindings), Env)),
    union(BindingFree, BodyFree);

free_vars(Var, Env) when is_atom(Var) ->
    case is_bound(Var, Env) of
        true -> [];
        false -> [Var]
    end.
```

#### Lifted Function Naming

**Naming Convention** (`lifted_name/2` at lines 154-168):

```erlang
-lfe-<OriginalFunc>-lambda-<Counter>
```

**Examples**:

- `make-adder` → `-lfe-make-adder-lambda-1`
- Top-level lambda → `-lfe-top-lambda-1`
- Nested lambda → `-lfe-outer-lambda-2-lambda-1`

**Counter**: Ensures unique names for multiple lambdas in same function.

#### Match-Lambda Handling

**Match-Lambda Lifting** (`lift_match_lambda/3` at lines 456-523):

```lisp
(match-lambda
  [[x] (+ x 1)]
  [[x y] (+ x y)])

; Lifted to:
(lambda Args
  (-lfe-lifted-match-lambda-1 CapturedVars Args))

(defun -lfe-lifted-match-lambda-1 (CapturedVars Args)
  (case Args
    [x] (+ x 1)
    [x y] (+ x y)))
```

#### Recursive Lifting

**Nested Lambdas** are lifted recursively:

```lisp
(defun foo (x)
  (lambda (y)
    (lambda (z)
      (+ x y z))))

; Lifts to:
; foo/1
; -lfe-foo-lambda-1/2   (takes x, y)
; -lfe-foo-lambda-1-lambda-1/3  (takes x, y, z)
```

#### Dependencies

**LFE modules**:

- `lfe_lib` - Utilities
- `lfe_env` - Environment tracking

**Erlang stdlib**:

- `lists`, `ordsets`

#### Used By

- `lfe_codegen` - Before translation to Erlang AST

#### Key Algorithms

**Environment Tracking**:

The lifter maintains:

- Bound variables (function parameters, let-bindings)
- Free variables (captured from outer scope)

**Closure Conversion**:

Standard compiler transformation:

1. Analyze free variables
2. Extend function signature with captured vars
3. Update call sites to pass captured vars

**Optimization: Lift Only When Necessary**:

If a lambda has no free variables, it can be lifted without capturing:

```lisp
(lambda (x) (* x x))
; Lifts to simple top-level function (no captures)
```

#### Special Considerations

**Performance Impact**:

Lambda lifting is necessary because:

- Erlang doesn't have true closures at BEAM level
- All functions must be at module level
- BEAM funs capture by creating new fun objects

**Call Overhead**:

Lifted functions have overhead:

- Extra parameters (captured vars)
- Indirect call (fun object wraps call)

**Generated Code Size**:

Each closure generates a new top-level function, increasing module size.

**Debugging**:

Lifted functions have generated names (`-lfe-...-lambda-N`), making stack traces harder to read.
