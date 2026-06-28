# Pattern Macros (syntax-rules)

**From Scheme** (provided by `scm` module).

**Syntax**:

```lisp
(define-syntax name
  (syntax-rules (keywords...)
    (pattern1 template1)
    (pattern2 template2)
    ...))
```

**Example**:

```lisp
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (progn body ...) 'false))))

;; Usage:
(when (> x 10)
  (print x)
  (inc-counter))

;; Expands to:
(if (> x 10)
    (progn (print x) (inc-counter))
    'false)
```

**Ellipsis** (`...`): Matches zero or more repetitions.

**Pattern matching**:

```erlang
mbe_match_pat(Pattern, Args, Keywords) -> true | false
```

From `scm.erl:110-139`:

```lisp
;; Patterns:
'quoted          % Matches exactly
keyword          % Matches keyword (from syntax-rules)
variable         % Matches anything (binds variable)
(p ...)          % Matches list where all elements match p
(p1 p2 ...)      % Matches list structure
```

**Binding extraction**:

```erlang
mbe_get_bindings(Pattern, Args, Keywords) -> Bindings
```

**Template expansion**:

```erlang
mbe_expand_pattern(Template, Bindings, Keywords) -> Expansion
```

**Implementation** (src/scm.erl:242-276):

```erlang
mbe_syntax_rules_proc(Patterns, Keywords, Args, Env, St) ->
    case find_matching_pattern(Patterns, Args, Keywords) of
        {ok, Pattern, Template} ->
            Bindings = mbe_get_bindings(Pattern, Args, Keywords),
            Expansion = mbe_expand_pattern(Template, Bindings, Keywords),
            {ok, Expansion, Env, St};
        no_match ->
            {error, no_matching_pattern}
    end
```
