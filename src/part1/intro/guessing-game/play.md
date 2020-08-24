# Playing the Game

If you are still in the REPL, quit out of it so that `rebar3` can rebuild our changed module. Then start it up again:

```bash
$ rebar3 lfe repl
```

Once at the LFE propmpt, start up the application:

```lisp
lfe> (application:ensure_all_started 'guessing-game)
```

With the application and all of its dependencies started, we're ready to start the game and play it through:

```lisp
lfe> (guessing-game:start-game)
;; ok
;; Guess the number I have chosen, between 1 and 10.
```

```lisp
lfe> (guessing-game:guess 10)
;; ok
;; Your guess is too high.
```

```lisp
lfe> (guessing-game:guess 1)
;; ok
;; Your guess is too low.
```

```lisp
lfe> (guessing-game:guess 5)
;; ok
;; Your guess is too low.
```

```lisp
lfe> (guessing-game:guess 7)
;; ok
;; Your guess is too low.
```
```lisp
lfe> (guessing-game:guess 8)
;; ok
;; Well-guessed!!
;; Game over
```

Now that our code is complete and the game is working, let's review the module.
