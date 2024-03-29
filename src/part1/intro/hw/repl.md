# From the REPL

As previously demonstrated, it is possible to start up the LFE 'read-eval-print loop' (REPL) using `rebar3`:

```bash
$ rebar3 lfe repl
```

Once you are at the LFE prompt, you may write a simple LFE "program" like the following:

```lisp
lfe> (io:format "~p~n" (list "Hello, World!"))
```

Or, for the terminally lazy:

```lisp
lfe> (io:format "~p~n" '("Hello, World!"))
```

While technically a program, it is not a very interesting one: we didn't create a function of our own, nor did we run it from outside the LFE interactive programming environment.

Let's address one of those points right now. Try this:

```lisp
lfe> (defun hello-world ()
lfe>   (io:format "~p~n" '("Hello, World!")))
```

This is a simple function definition in LFE.

We can run it by calling it:

```lisp
lfe> (hello-world)
;; "Hello, World!"
;; ok
```

When we execute our `hello-world` function, it prints our message to `standard-output` and then lets us know everything really quite fine with a friendly `ok`.

<div class="alert alert-info">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    Note
  </h4>
  <p class="mb-0">
    LFE displays `ok` as output for functions that do not return a value.
  </p>
</div>

Now let's address the other point: running a Hello-World programming from outside LFE.

Hit `<CTRL-G><Q>` to exit the REPL and get back to your terminal.
