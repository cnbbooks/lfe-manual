# Complete Transformation Example

Let's trace a complete example from LFE source through to BEAM bytecode, showing every intermediate representation.

**Source code** (`factorial.lfe`):

```lisp
(defmodule factorial
  (export (fac 1)))

(defun fac (n)
  (fac-helper n 1))

(defun fac-helper
  ([0 acc] acc)
  ([n acc] (when (> n 0))
   (fac-helper (- n 1) (* n acc))))
```

**Stage 1: Tokens** (abbreviated):

```erlang
[{'(', 1}, {symbol, 1, defmodule}, {symbol, 1, factorial},
 {'(', 2}, {symbol, 2, export}, {'(', 2}, {symbol, 2, fac}, {number, 2, 1}, ...
```

**Stage 2: S-expressions**:

```lisp
[defmodule, factorial,
  [export, [fac, 1]]]

[defun, fac, [n],
  ['fac-helper', n, 1]]

[defun, 'fac-helper',
  [[0, acc], acc],
  [[n, acc], [when, ['>', n, 0]],
    ['fac-helper', ['-', n, 1], ['*', n, acc]]]]
```

**Stage 3: After Macro Expansion** (`lfe_macro.erl`):

The macros `defmodule` and `defun` expand to core forms:

```lisp
;; defmodule expands to define-module
['define-module', factorial,
  [],                          % Metadata
  [[export, [fac, 1]]]]       % Attributes

;; defun expands to define-function
['define-function', fac,
  [],                          % Metadata
  [lambda, [n],                % Single-clause function
    ['fac-helper', n, 1]]]

;; defun with multiple clauses expands to match-lambda
['define-function', 'fac-helper',
  [],                          % Metadata
  ['match-lambda',             % Multi-clause function
    [[0, acc], acc],
    [[n, acc], [when, ['>', n, 0]],
      ['fac-helper', ['-', n, 1], ['*', n, acc]]]]]
```

**Stage 4: After Linting** (`lfe_lint.erl`):

Forms are validated (variables bound, exports match definitions, etc.). No transformation, just error checking.

**Stage 5: After Code Generation** (`lfe_codegen.erl`):

Module structure is built, ready for translation:

```erlang
%% Module attributes
{module, factorial}
{exports, [{fac, 1}]}

%% Function definitions (still LFE forms, but organized)
{function, fac, 1,
  [lambda, [n], ['fac-helper', n, 1]]}

{function, 'fac-helper', 2,
  ['match-lambda',
    [[0, acc], acc],
    [[n, acc], [when, ['>', n, 0]],
      ['fac-helper', ['-', n, 1], ['*', n, acc]]]]}
```

**Stage 6: After Translation to Erlang AST** (`lfe_translate.erl`):

This is where LFE becomes Erlang Abstract Format:

```erlang
%% Module attribute
{attribute, 1, module, factorial}

%% Export attribute
{attribute, 2, export, [{fac, 1}]}

%% Function: fac/1
{function, 4, fac, 1,
  [{clause, 4,
    [{var, 4, 'N'}],          % Parameters
    [],                        % Guards
    [{call, 5,                % Body
       {atom, 5, 'fac-helper'},
       [{var, 5, 'N'}, {integer, 5, 1}]}]}]}

%% Function: fac-helper/2
{function, 7, 'fac-helper', 2,
  [%% Clause 1: base case
   {clause, 8,
    [{integer, 8, 0}, {var, 8, 'Acc'}],  % Pattern
    [],                                    % No guard
    [{var, 8, 'Acc'}]},                   % Body

   %% Clause 2: recursive case
   {clause, 9,
    [{var, 9, 'N'}, {var, 9, 'Acc'}],    % Pattern
    [[{op, 9, '>', {var, 9, 'N'}, {integer, 9, 0}}]],  % Guard
    [{call, 10,                           % Body: recursive call
       {atom, 10, 'fac-helper'},
       [{op, 10, '-', {var, 10, 'N'}, {integer, 10, 1}},
        {op, 10, '*', {var, 10, 'N'}, {var, 10, 'Acc'}}]}]}]}
```

**Stage 7: Core Erlang** (intermediate representation before BEAM):

The Erlang compiler (`compile:forms/2`) translates Abstract Format to Core Erlang:

```erlang
module 'factorial' ['fac'/1, 'module_info'/0, 'module_info'/1]
  attributes []

'fac'/1 =
  fun (N) ->
    call 'fac-helper'(N, 1)

'fac-helper'/2 =
  fun (0, Acc) -> Acc
  fun (N, Acc) when call 'erlang':'>'(N, 0) ->
    let <_2> = call 'erlang':'-'(N, 1)
        <_3> = call 'erlang':'*'(N, Acc)
    in call 'fac-helper'(_2, _3)
```

**Stage 8: BEAM Bytecode**:

Core Erlang is compiled to BEAM assembly, then to bytecode. The `.beam` file contains:

- Module name: `factorial`
- Exports: `[{fac,1}]`
- Attributes: `[]`
- Compiled code: native BEAM instructions
- Metadata: line numbers, debug info

**Key observation**: The final BEAM bytecode for this LFE module is **identical** to what would be produced from equivalent Erlang code. This is LFE's zero-overhead interoperability.
