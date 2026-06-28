# Atoms

The cloest analog in LFE to what most Lisp dialects call symbols is the Erlang
atom. Just as with Lisps do with symbols, LFE uses atoms for its variable and
function names. Atoms are literals and constants, which means that their value
is the same as their name and once created, cannot be changed.

Some basic examples of atoms:

``` lisp
lfe> 'zark
zark
lfe> 'zarking-amazing
zarking-amazing
```

Slightly less straight-forward examples which start with non-alphanumeric
characters:

``` lisp
lfe> ':answer
:answer
lfe> '42answer
42answer
lfe> '42.0e42answer
42.0e42answer
lfe> '42°C
42°C
```

Standard LFE atom names may be comprised of all the latin-1 character set except
the following:

* control character
* whitespace
* the various brackets
* double quotes
* semicolon

Of these, only `|`, `\`, `'`, `,`, and `#` may not be the first character of the
symbol's name (but they *are* allowed as subsequent letters).

Non-standard atom names may be created using atom quotes:

``` lisp
lfe> '|symbol name with spaces|
|symbol name with spaces|
lfe> '|'with staring quote!|
|'with staring quote!|
lfe> '|| ; <-- empty atoms are supported too
||
lfe> '|really weird atom: '#[]{}()<>";\||
|really weird atom: '#[]{}()<>";\||
```

In this case the name can contain any character of in the range from 0 to 255,
and even no character at all.

In the case of atoms, it is important to understand a little something about
their internals. In particular,
Erlang and LFE atoms are global for each instance of a running Erlang virtual
machine. Atoms are maintained by the VM in a table and are not garbage
collected. By default, the Erlang atom table allows for a maximum of 1,048,576
entries.

<div class="alert alert-danger">
  <h4 class="alert-heading">
    <i class="fa fa-minus-circle" aria-hidden="true"></i>
    Danger!
  </h4>
  <p class="mb-0">
     Uncontrolled automatic creation of atoms can crash the VM!
  </p>
   <p class="mb-0">
    See the "Caveats" section below for more details.
  </p>
</div>

## As Symbols

The following code shows LFE's use of atoms in variable names. First, let's use
a function for a slighly different purpose than designed: calling
`list_to_existing_atom` on a string will only return a result if an atom
of the same name already exists in the atom table. Otherwise, it will return
an error:

``` lisp
lfe> (list_to_existing_atom "zaphod")
** exception error: bad argument
  in (erlang : list_to_existing_atom "zaphod")
```

This confirms that there is no `zaphod` atom in the atom table.

Now let's create a variable, assigning a value to it, and then use our indirect
means of checkihg the atom table:

``` lisp
lfe> (set zaphod "frood")
"frood"
lfe> (list_to_existing_atom "zaphod")
zaphod
```

And here's an example showing LFE's use of atoms in function names using the
same approach as above:

``` lisp
lfe> (list_to_existing_atom "beez")
** exception error: bad argument
  in (erlang : list_to_existing_atom "beez")

lfe> (defun beez (x) x)
beez
lfe> (list_to_existing_atom "beez")
beez
```

## Converting

Atoms may be converted to strings and bitstrings, and vice versa.

``` lisp
(atom_to_binary 'trisha)
#"trisha"
(atom_to_binary 'mcmillan 'latin1)
#"mcmillan"
lfe> (atom_to_list 'trillian)
"trillian"
```

Note that the first one above is only available in Erlang 23.0 and above.

Some more examples for encoding:

``` lisp
lfe> (atom_to_binary '42°C 'latin1)
#B(52 50 176 67)
lfe> (atom_to_binary '42°C 'utf8)
#"42°C"
```

Functions that convert atoms only if they already exist in the atom table:

``` lisp
lfe> (binary_to_existing_atom #"trisha")
trisha
lfe> (binary_to_existing_atom #"trisha" 'latin1)
trisha
lfe> (list_to_existing_atom "trisha")
trisha
```

## Operators

The only operators you may use on atoms are the comparison operators, e.g.:

``` lisp
lfe> (> 'a 'b)
false
lfe> (< 'a 'b)
true
lfe> (=:= 'a 'a)
true
```

## Predicates

To test if a value is an atom, we will first include some code:

``` lisp
lfe> (include-lib "lfe/include/cl.lfe")
```

That adds Common Lisp inspired functions and macros to our REPL session.

``` lisp
lfe> (atomp 'arthur)
true
lfe> (atomp 42)
false
lfe> (atomp "forty-two.forty-two")
false
```

If the atom in question has been used in a function name definition:

If you prefer the Clojure-style of predicates:

``` lisp
lfe> (include-lib "lfe/include/clj.lfe")
lfe> (atom? 'dent)
true
lfe> (atom? "Ford")
false
```

Of course there is always the Erlang predicate, usable without having to do any includes:

``` lisp
lfe> (is_atom 'arthur)
true
lfe> (is_atom "forty-two.forty-two")
false
```

## Caveats

As mentioned above ([and documented](http://erlang.org/doc/efficiency_guide/commoncaveats.html)),
one needs to take care when creating atoms. By default, the maximum number of
atoms that the Erlang VM will allow is 1,048,576; any more than that, and the
VM will crash.

The first rule of thumb is not to
write any code that generates large numbers of atoms. More explicitly useful,
there are some handy functions for keeping track of the atom table, should you
have the need.

``` lisp
lfe> (erlang:memory 'atom_used)
244562
lfe> (erlang:system_info 'atom_count)
9570
lfe> (erlang:system_info 'atom_limit)
1048576
```

Note that support for easily extracting the current atom data
from `system_info` -- as demonstrated by the last two function calls above --
were added in Erlang 20; should you be running an older
version of Erlang, you will need to parse the `(system_info 'info)`
bitstring.

The default atom table size may be overridden during startup by passing a value
with the `+t` options:

``` shell
$ lfe +t 200000001
```

``` lisp
lfe> (erlang:system_info 'atom_limit)
200000001
```
