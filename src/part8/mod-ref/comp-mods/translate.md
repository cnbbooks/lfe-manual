# lfe_translate.erl - Core Erlang Translator

**Purpose**: Translate LFE forms to Core Erlang intermediate representation. This is the **second-largest** module in the codebase.

**Location**: `src/lfe_translate.erl`
**Size**: 2,182 LOC, 84KB

**Module Classification**: Compiler backend, code generation

#### Public API

```erlang
to_expr(LfeExpr, Line) -> CoreExpr
to_expr(LfeExpr, Line, {Imports, Aliases}) -> CoreExpr
```

Translate LFE expression to Core Erlang. Located at `lfe_translate.erl:82-86`.

```erlang
from_expr(CoreExpr) -> LfeExpr
from_expr(CoreExpr, Options) -> LfeExpr
```

Translate Core Erlang back to LFE. Located at `lfe_translate.erl:96-99`.

**Pattern and Guard Translation**:

```erlang
to_pat(LfePattern, Line) -> CorePattern
to_guard(LfeGuard, Line) -> CoreGuard
```

Located at lines 88-92.

#### Translation Targets

**Core Erlang** is Erlang's intermediate representation:

- Explicit pattern matching
- Simplified control flow
- SSA-like structure
- Direct mapping to BEAM

#### Translation Rules

**Atomic Values** (`to_expr/2` at lines 273-295):

```
LFE                 Core Erlang
---                 -----------
42                  #c_literal{val=42}
3.14                #c_literal{val=3.14}
"hello"             #c_literal{val="hello"}
(quote foo)         #c_literal{val=foo}
```

**Data Constructors** (lines 297-385):

```
[cons H T]          #c_cons{hd=H', tl=T'}
[list A B C]        #c_cons{hd=A', tl=#c_cons{...}}
[tuple A B]         #c_tuple{es=[A', B']}
[binary Seg1 Seg2]  #c_binary{segments=[Seg1', Seg2']}
[map K1 V1 K2 V2]   #c_map{es=[K1=>V1, K2=>V2]}
```

**Functions** (lines 450-582):

```
[lambda [x y] Body]
→ #c_fun{vars=[Vx, Vy], body=Body'}

[match-lambda
  [[x y] Body1]
  [[a b c] Body2]]
→ #c_fun{vars=[V1,V2,...], body=#c_case{...}}
```

**Let Bindings** (lines 630-708):

```
[let [[x Expr1] [y Expr2]] Body]
→ #c_let{vars=[Vx,Vy], arg=#c_values{[Expr1',Expr2']}, body=Body'}
```

**Control Flow** (lines 752-890):

```
[if Test Then Else]
→ #c_case{arg=Test',
         clauses=[#c_clause{pats=[#c_literal{val=true}], body=Then'},
                  #c_clause{pats=[#c_literal{val=false}], body=Else'}]}

[case Expr [Pat1 Body1] [Pat2 Body2]]
→ #c_case{arg=Expr',
         clauses=[#c_clause{pats=[Pat1'], body=Body1'},
                  #c_clause{pats=[Pat2'], body=Body2'}]}

[receive [Pat1 Body1] [Pat2 Body2] [after Timeout AfterBody]]
→ #c_receive{clauses=[...], timeout=Timeout', action=AfterBody'}
```

**Try/Catch** (lines 934-1023):

```
[try Expr
  [case Pat1 Body1]
  [catch [[Class Reason Stacktrace] Handler]]
  [after AfterBody]]
→ #c_try{arg=Expr',
        vars=[V1], body=Body',
        evars=[Eclass,Ereason,Etrace], handler=Handler',
        ...}
```

**Function Calls** (lines 1077-1165):

```
[foo Arg1 Arg2]
→ #c_apply{op=#c_var{name={foo,2}}, args=[Arg1', Arg2']}

[call Mod Fun Arg1]
→ #c_call{module=Mod', name=Fun', args=[Arg1']}

[funcall FunExpr Arg1]
→ #c_apply{op=FunExpr', args=[Arg1']}
```

#### Pattern Translation

**Pattern Contexts** (lines 1267-1453):

Patterns have special translation rules:

- Variables become `#c_var{}`
- Literals must match exactly
- No expressions allowed
- Maps allow both matching and association patterns

```
Pattern                     Core Pattern
-------                     ------------
x                           #c_var{name=X}
42                          #c_literal{val=42}
[= Pat1 Pat2]               #c_alias{var=..., pat=...}
[cons H T]                  #c_cons{hd=H', tl=T'}
[binary [x [size 8]]]       #c_bitstr{val=X, size=8, ...}
```

#### Guard Translation

**Guard Restrictions** (lines 1489-1621):

Guards can only contain:

- BIFs from `lfe_internal:is_guard_func/2`
- Boolean operators: `and`, `or`, `andalso`, `orelse`
- Comparisons: `<`, `>`, `=:=`, `==`, etc.
- Type tests: `is_atom`, `is_list`, etc.

```
Guard                       Core Guard
-----                       ----------
[(> x 10)]                  #c_call{module=erlang, name='>', args=[X,10]}
[(andalso (> x 0) (< x 100))]
                            Conjunction of comparisons
```

#### Import and Alias Resolution

When translating with imports/aliases (`to_expr/3`):

```erlang
Context = {Imports, Aliases}
Imports = [{FuncName, {Module, RemoteName}}]
Aliases = [{Alias, ActualModule}]
```

**Example**:

```lisp
(import (from lists (map list-map)))
(import (rename lists (reverse rev)))
(module-alias (long-module-name short))

(list-map Fn List)  ; Translates to: lists:map(Fn, List)
(rev List)          ; Translates to: lists:reverse(List)
(short:func A)      ; Translates to: long-module-name:func(A)
```

#### Dependencies

**LFE modules**:

- `lfe_internal` (13 calls) - Form validation
- `lfe_lib` (5 calls) - Utilities
- `lfe_bits` - Binary segment translation

**Erlang compiler**:

- `cerl` - Core Erlang construction
- `core_lib` - Core Erlang utilities

#### Used By

- `lfe_codegen` - Code generation pipeline

#### Key Algorithms

**SSA Construction**:

Core Erlang uses Single Static Assignment (SSA) form. The translator generates fresh variables:

```erlang
new_var(St) ->
    N = St#trans.vc,
    {#c_var{name={var,N}}, St#trans{vc=N+1}}.
```

**Pattern Compilation**:

Complex patterns are compiled to nested case expressions:

```lisp
(match-lambda
  [[x x] 'same]         ; Repeated variable
  [[x _] 'different])

→ Compiles to Core with equality check:
  case {Arg1, Arg2} of
    {X, X} -> 'same';
    {X, _} -> 'different'
  end
```

**Binary Optimization** (`lfe_translate.erl:1623-1789`):

Binary construction is optimized:

- Constant-size segments
- UTF-8/UTF-16/UTF-32 encoding
- Bit alignment tracking
- Size expression evaluation

**Let-Optimization** (lines 630-708):

Sequential let bindings are compiled to nested `#c_let` forms for efficiency.

#### Special Considerations

**Core Erlang Validation**:

Generated Core Erlang is validated by the Erlang compiler. If translation produces invalid Core, compilation fails with cryptic errors.

**Optimization Opportunities**:

The translator produces straightforward Core Erlang. The Erlang compiler's optimization passes handle:

- Dead code elimination
- Inlining
- Constant folding

**Performance**:

Translation is ~10% of compilation time. Complex pattern matching and binary construction are the slowest parts.

**Tail Call Optimization**:

The translator marks tail positions. Core Erlang and BEAM handle actual TCO.
