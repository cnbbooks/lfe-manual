# REPL Me Up!

Make sure you've `cd`ed into your new LFE project directory, and then do this:

```bash
$ rebar3 lfe repl
```

On windows first enter Erlang's repl then  run `lfe_shell:start().`
```
D:\Lfe\my-test-lib
λ rebar3 lfe repl
===> Verifying dependencies...
===> Compiling my-test-lib
Eshell V10.3  (abort with ^G)
1> lfe_shell:start().
```

This should give you something that looks like the following:

```
Erlang/OTP 23 [erts-11.0.2] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [hipe] [dtrace]

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/rvirding/lfe
   \     l    |_/    |
    \   r     /      |   LFE v1.3-dev (abort with ^G)
     `-E___.-'

lfe>
```

Try setting a variable:

```lisp
> (set my-list (lists:seq 1 6))
(1 2 3 4 5 6)
```

Here are some operations using more functions from the built-in Erlang `lists` module:

```lisp
> (* 2 (lists:sum my-list))
42
> (* 2 (lists:foldl (lambda (n acc) (+ n acc)) 0 my-list))
42
```

Let's turn that into a function:

```lisp
> (defun my-sum (start stop)
    (let ((my-list (lists:seq start stop)))
      (* 2 (lists:foldl
             (lambda (n acc)
               (+ n acc))
             0 my-list))))
my-sum
```

And try it out!

```lisp
> (my-sum 1 6)
42
```

Enough with the fancy REPL-play ... What about some *real* code? What does a **project**
look like?
