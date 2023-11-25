# Predicates

There are several options for naming boolean-valued functions and variables to indicate they are predicates:

* a trailing `?`
* a trailing `-p`
* a trailing `p`
* a leading `is-`

Modern Lisps tend to prefer `?`, while classic Lisps tend to use `p`. Erlang code tends to use `is_` which translates to `is-` in LFE.  You should use "p" when the rest of the function name is one word and "-p" when it is more than one word.

A rationale for this convention is given in the [CLtL2](https://en.wikipedia.org/wiki/Common_Lisp_the_Language)  chapter on predicates.

Whichever convention your project wishes to use, be consistent and use only that convention in the entire project.

Do not use these boolean indicators in functions that do not return booleans and variables that are not boolean-valued.
