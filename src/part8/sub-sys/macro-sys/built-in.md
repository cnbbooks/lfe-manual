# Built-in Macros Catalog

LFE provides **50+ built-in macros**. Key categories:

**Common Lisp style**:

```lisp
defmodule, defun, defmacro      % Module/function definition
cond                            % Multi-branch conditional
let*, flet*, letrec*           % Binding forms
list*                          % List construction
```

**Data structures**:

```lisp
defrecord                      % Record definition (generates 9+ macros)
defstruct                      % Struct definition (generates functions)
```

**Pattern matching**:

```lisp
match-spec                     % ETS/trace match specification DSL
```

**Comprehensions**:

```lisp
lc                            % List comprehension
bc                            % Binary comprehension
qlc                           % Query list comprehension
```

**Convenience**:

```lisp
c*r macros                    % car, cdr, caar, cadr, cddr, etc.
++, !=, ===, !==             % Operator synonyms
```

**Special**:

```lisp
MODULE                        % Current module name (compile-time constant)
LINE                          % Current line number (compile-time constant)
:module:function             % Call syntax → (call 'module 'function ...)
```
