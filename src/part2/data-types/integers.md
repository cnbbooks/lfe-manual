# Integers

Integers in LFE may be either positive or negative and are by default base 10. Like Erlang, LFE does not have a maximum size of integer (in contrast to languages like C). This is accomplished via automatic conversion to larger (or smaller) internal representations (including the use of bignums).

``` lisp
lfe> 42
42
lfe> -42
-42
lfe> 1764
1764
lfe> 150130937545296561928688012959677941476701514734130607701636390322176
150130937545296561928688012959677941476701514734130607701636390322176
```

## Bases

Several bases are supported via special syntax:

``` lisp
lfe> #b101010 ; binary
42
lfe> #o52 ; octal
42
lfe> #d42 ; decimal (explicit base 10)
42)
lfe> #x2a ; hexadecimal
42
```

Generic bases are supported, too:

``` lisp
lfe> #36r16
42
```

The number after the hash `#` is the base and may be any positive integer from 2 through 36. The number after the radix `r` is the actual value and must only bve comprised of integers allowed for the given base.

Converting between bases in LFE is most easily done via the `integer_to_list` Erlang function. For example:

``` lisp
lfe> (integer_to_list 1000 2)
"1111101000"
lfe> (integer_to_list 1000 8)
"1750"
lfe> (integer_to_list 1000 10)
"1000"
lfe> (integer_to_list 1000 16)
"3E8"
lfe> (integer_to_list 1000 36)
"RS"
```

If, for whatever reason, you want your base 10 integrer as a list (Erlang/LFE string), you can do that with this:

``` lisp
lfe> (integer_to_list 1000)
"1000"
```

Conversion to the binary type is also supported:

``` lisp
lfe> (integer_to_binary 1000)
#"1000"
lfe> (integer_to_binary 1000 2)
#"1111101000"
lfe> (integer_to_binary 1000 8)
#"1750"
```

The results above show LFE's literal representations of binary data; this will be covered in the chapter on "Bytes and Binaries".

## Arithmetic Operators

Integers may be operated upon with the following:

``` lisp
lfe> (+ 1)
1
lfe> (+ 1 2)
3
lfe> (+ 1 2 3)
6
lfe> (- 1 2 3)
-4
lfe> (* 1 2 3)
6
lfe> (/ 1 2 3)
0.16666666666666666
```

Note that the division operator returns a float; floats will be covered in the next section.

Integer division is supported with a 2-arity function:

``` lisp
lfe> (div 1 2)
0
lfe> (div 10 2)
5
```

LFE also supports the remainder operation:

``` lisp
lfe> (rem 10 3)
1
```

## Mathematical Functions

The auto-loaded `erlang` module has several mathematical functions and is
accessible in LFE without having to type the `erlang:` module prefix in the
function calls. These include the following:

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

Additional maths functions are provided via the `math` module. Since this module
is not auto-loaded, in order to auto-complete it in the REPL you will need to
load it:

``` lisp
lfe> (code:ensure_loaded 'math)
#(module math)
```

Now you can hit `<TAB>` after typing the following:

``` lisp
lfe> (math:
```

Which gives:

```
acos/1         acosh/1        asin/1         asinh/1        atan/1
atan2/2        atanh/1        ceil/1         cos/1          cosh/1
erf/1          erfc/1         exp/1          floor/1        fmod/2
log/1          log10/1        log2/1         module_info/0  module_info/1
pi/0           pow/2          sin/1          sinh/1         sqrt/1
tan/1          tanh/1
```

``` lisp
lfe> (round (math:pow 42 42))
150130937545296561928688012959677941476701514734130607701636390322176
```

The documentation for these functions is limited ([available here](http://erlang.org/doc/man/math.html)) due in part to the fact that these are C library wrappers. Those that aren't documented should be self-explanatory for anyone who has used simular mathematical functions in other programming language libraries.

## Predicates

To test if a value is an integer, we will first include some code:

``` lisp
lfe> (include-lib "lfe/include/cl.lfe")
```

That adds Common Lisp inspired functions and macros to our REPL session.

``` lisp
lfe> (integerp 42)
true
lfe> (integerp 42.24)
false
lfe> (integerp "forty-two")
false
```

If you prefer the Clojure-style of predicates:

``` lisp
lfe> (include-lib "lfe/include/clj.lfe")
lfe> (integer? 42)
true
lfe> (integer? "forty-two")
false
```

Of course there is always the Erlang predicate, usable without having to do any includes:

``` lisp
(is_integer 42)
true
(is_integer "forty-two")
false
```
