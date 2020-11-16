# Floats

Real numbers in LFE are represented using the stadnard floating point numbers.

```lisp
lfe> 42.42
42.42
lfe> (/ 10 3)
3.3333333333333335
lfe> (math:pow 42 42)
1.5013093754529656e68
```

Note that the ~`1.5e68` above is the floating point equivalent of scientific
notation, namely <code>1.5 x 10<sup>68</sup></code>. LFE follows the 64-bit
standard for float representation given by [IEEE 754-1985](https://en.wikipedia.org/wiki/IEEE_754-1985).

## Converting

An integer may be converted to a float explicitly:

```lisp
lfe> (float 42)
42.0
```

Or, as with integers, to binaries:

``` lisp
lfe> (float_to_binary 42.42)
#"4.24200000000000017053e+01"
lfe> (float_to_binary 42.42 '(#(scientific 10)))
#"4.2420000000e+01"
lfe> (float_to_binary 42.42 '(#(scientific 20)))
#"4.24200000000000017053e+01"
lfe> (float_to_binary 42.42 '(#(decimals 10)))
#"42.4200000000"
lfe> (float_to_binary 42.42 '(#(decimals 10) compact))
#"42.42"
```

Or to lists (LFE and Erlang strings):

``` lisp
lfe> (float_to_list 42.42 '(#(scientific 10)))
"4.2420000000e+01"
lfe> (float_to_list 42.42 '(#(scientific 20)))
"4.24200000000000017053e+01"
lfe> (float_to_list 42.42 '(#(decimals 10)))
"42.4200000000"
lfe> (float_to_list 42.42 '(#(decimals 10) compact))
"42.42"
```

## Formatting

If you need to round floating point numbers to a specific precision, you'll want to use the `format` function from either the `io`, `io_lib`, or `lfe_io` modules. If just want to print a value using Erlang syntax and formatting, the `io` module is what you want. If you prefer LFE syntax and formatting for your output, you'll want to use the `lfe_io` module. If you want to use the data or store it in a variable, you'll need the `io_lib` library.

For default precision:

``` lisp
lfe> (io_lib:format "~f" `(,(math:pow 42 42)))
"150130937545296561929000000000000000000000000000000000000000000000000.000000"
```

Two decimal places:

``` lisp
lfe> (io_lib:format "~.2f" `(,(math:pow 42 42)))
"150130937545296561929000000000000000000000000000000000000000000000000.00"
```

20 decimal places:

``` lisp
lfe> (io_lib:format "~.20f" `(,(math:pow 42 42)))
"150130937545296561929000000000000000000000000000000000000000000000000.00000000000000000000"
```

## Arithmetic Operators & Mathematical Functions

Floats use most of the same operators and functions as integers, so be sure to 
review [these](integers.html#arithmetic-operators) [subsections](integers.html#mathematical-functions) in the ["Integers" section](integers.html).

Others include:

``` lisp
lfe> (abs -42)
42
lfe> (ceil 42.1)
43
lfe> (floor 42.1)
42
lfe> (round 42.4)
42
lfe> (round 42.5)
43
lfe> (min 1 2)
1
lfe> (max 1 2)
2
```

## Predicates

To test if a value is an integer, we will first include some code:

``` lisp
lfe> (include-lib "lfe/include/cl.lfe")
```

That adds Common Lisp inspired functions and macros to our REPL session.

``` lisp
lfe> (floatp 42.42)
true
lfe> (floatp 42)
false
lfe> (floatp "forty-two.forty-two")
false
```

If you prefer the Clojure-style of predicates:

``` lisp
lfe> (include-lib "lfe/include/clj.lfe")
lfe> (float? 42.42)
true
lfe> (float? "forty-two.forty-two")
false
```

Of course there is always the Erlang predicate, usable without having to do any includes:

``` lisp
lfe> (is_float 42.42)
true
lfe> (is_float "forty-two.forty-two")
false
```
