# Erlang Cons Cells

Erlang lists are built as sequences of cons cells, with each cell composed of a value and a pointer to another cons cell or empty list. While structurally similar to Lisp cons cells, Erlang's implementation and usage patterns differ in important ways.

## The Pipe Operator

In Erlang, the cons operator is represented by the pipe symbol (|), which separates the head of a list from its tail. The syntax `[Head | Tail]` constructs or pattern-matches a cons cell.

```erlang
% Building a list with cons
[1 | [2, 3]]      % => [1, 2, 3]
[1 | [2 | [3]]]   % => [1, 2, 3]
[1 | [2 | [3 | []]]]  % => [1, 2, 3]
```

## Pattern Matching and Head/Tail

Every function operating on lists in Erlang is defined in terms of two primitives: head and tail, which return the first element and the rest of the list respectively. Pattern matching with cons cells provides an elegant idiom for recursive list operations, where you can extract the head and tail in function definitions.

```erlang
% Pattern matching to extract head and tail
[Head | Tail] = [1, 2, 3].
% Head => 1
% Tail => [2, 3]
```

This makes recursive list processing natural and efficient:

```erlang
length([]) -> 0;
length([_Head | Tail]) -> 1 + length(Tail).
```

## Proper vs Improper Lists

A proper list in Erlang ends with an empty list as its last cell. When the tail of the last cons cell contains something other than another cons cell or the empty list, you have an improper list:

```erlang
[1 | 2]      % Improper list - tail is not a list
[1 | [2]]    % Proper list
```

While improper lists are valid Erlang terms, most standard list functions expect proper lists.
