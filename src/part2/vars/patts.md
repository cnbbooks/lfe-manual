# Pattern-matching Preview

Pattern matching is deeply integral to Erlang, and thus LFE. Great swaths of the language touch upon it, so therefore it is a farily large subject to cover in LFE. We will be revisiting pattern matching throughout the rest of the book, but for now we will provide a brief preview in the context of variables and binding values to them.

In the previous section, we saw in-REPL binding like this:

``` lisp
lfe> (set answer 42)
42
```

But what if it wasn't a simple integer we wanted to assign? What if we wanted to bind something in a data structure, like the tuple `#(answer 42)`? This is one of the ways in which pattern matching is used in Erlang:

``` lisp
lfe> (set (tuple 'answer answer) `#(answer 42))
#(answer 42)
lfe> answer
42
```

If we want to capture the key name, too?

``` lisp
lfe> (set (tuple key answer) `#(answer 42))
#(answer 42)
lfe> answer
42
lfe> key
answer
```

And if we don't care about the key at all?

``` lisp
lfe> (set (tuple _ answer) `#(answer 42))
#(answer 42)
lfe> answer
42
```

We can do the same thing in many other LFE forms, but here's a quick example of variable assignment with pattern matching in a `let` form, again borrowing from the previous section:

``` lisp
(defun display-answer ()
  (let (((tuple _ answer) `#(answer 42)))
    (io:format "The answer is ~p~n" `(,answer))))
```

Pattern matching can be used in may places in LFE, but things really start getting interesting when you define function heads to extract specific values! We will cover examples of that later in the book, in addition to many others.
