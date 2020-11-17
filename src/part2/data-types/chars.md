# Characters

Characters in LFE Are represented internally by integers, however a literal
syntax is offered for convenience:

``` lisp
lfe> #\a
97
lfe> #\A
65
lfe> #\ü
252
lfe> #\Æ
198
```

## Converting

Since a character literal and integer are the same thing as far as LFE is
concerned, there is no such thing as converting between a "char" and "ord" like
there is in some other languages.

However, one can format an integer as a string by telling the class of `format`
functions that the input is "character" type:

``` lisp
lfe> (io_lib:format "~c" `(198))
"Æ"
```

For merely printing to `standard out` instead of returning a value, one may
use:

``` lisp
lfe> (lfe_io:format "~c~n" `(198))
Æ
ok
lfe> (io:format "~c~n" `(198))
Æ
ok
```

## Operators

All operations that are valid for integers are valid for characters.

## Predicates

All predicates that are valid for integers are valid for characters.
