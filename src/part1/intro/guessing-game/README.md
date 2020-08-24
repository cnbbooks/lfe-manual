# Walk-through: An LFE Guessing Game

Now that you've seen some LFE in action, let's do something completely insane: write a whole game before we even know the language!

We will follow the same patterns established in the Hello-World examples, so if you are still in one of the Hello-World projects, change directory and then create a new LFE project:

```bash
$ cd ../
$ rebar3 new lfe-app guessing-game
$ cd ./guessing-game
```

We will create this game by exploring functions in the REPL and then saving the results in a file. Open up your generated project in your favourite code-editing application, and then open up a terminal from your new project directory, and start the REPL:

```bash
$ rebar3 lfe repl
```
