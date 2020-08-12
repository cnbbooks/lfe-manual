# Getting User Input

How do we get user input in LFE? Like this!

```lisp
lfe> (io:fread "Guess number: " "~d")
```

This will print the prompt `Guess number:` and then await your input and the press of the `<ENTER>` key. The input you provide needs to match the format type given in the second argument. In this case, the `~d` tells us that this needs to be a decimal (base 10) integer.

```lisp
fe> (io:fread "Guess number: " "~d")
```

```lisp
Guess number: 42
#(ok "*")
```

If we try typing something that is not a base 10 integer, we get an error:

```lisp
lfe> (io:fread "Guess number: " "~d")
```

```lisp
Guess number: forty-two
#(error #(fread integer))
```

With correct usage, how do we capture the value in a variable? The standard way to do this in LFE is destructuring via pattern matching. The following snippet extracts the value and then prints the extracted value in the REPL:

```lisp
lfe> (let ((`#(ok (,value)) (io:fread "Guess number: " "~d")))
lfe>   (io:format "Got: ~p~n" `(,value)))
```
```text
Guess number: 42
Got: 42
ok
```

We'll talk a lot more about pattern matching in the future, as well as the meaning of the backtick and commas. For now,let's keep pottering in the REPL with these explorations, and make a function for this:

```lisp
lfe> (defun guess ()
lfe>   (let ((`#(ok (,value)) (io:fread "Guess number: " "~d")))
lfe>     (io:format "You guessed: ~p~n" `(,value))))
```

And call it:

```lisp
lfe> (guess)
```

```text
Guess number: 42
You guessed: 42
ok
```
