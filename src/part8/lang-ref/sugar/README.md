# Syntactic Sugar and Shorthands

## Quote Forms

| Form | Equivalent |
|------|------------|
| `'expr` | `(quote expr)` |
| `` `expr`` | `(backquote expr)` |
| `,expr` | `(comma expr)` |
| `,@expr` | `(comma-at expr)` |

## Hash Forms

| Form | Equivalent |
|------|------------|
| `#(...)` | Tuple literal |
| `#B(...)` | Binary literal |
| `#M(...)` | Map literal |
| `#"..."` | Binary string |
| `#'f/2` | `(function f 2)` |
| `#'m:f/2` | `(function m f 2)` |
| `#.expr` | Eval at read time |

## Module Call Syntax

| Form | Equivalent |
|------|------------|
| `(: mod func args)` | `(call 'mod 'func args)` |
| `(mod:func args)` | `(call 'mod 'func args)` |
