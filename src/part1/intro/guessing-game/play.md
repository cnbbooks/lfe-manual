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

<p style="font-size: 16pt;">
<img class="liffy-bot-mascot" src="../../../images/LiffyBot-5-x500-bold-color.png"/>Success! You've just done something pretty amazing, if still mysterious: you've not only created your first <strong>OTP application</strong> running a <strong>generic server</strong>, you've successully run it through to completion! Until we can dive into all the details of what you've just seen, let's take a quick moment to review what you've done and indicate which parts of this book will provide the remaining missing pieces.
</p>
