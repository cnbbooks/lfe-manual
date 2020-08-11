# From the Command Line

From your system shell prompt, run the following to create a new project that will let us run a Hello-World program from the command line:

```shell
rebar3 new lfe-main hello-world
cd ./hello-world
```

Once in the project directory, you can actually just do this:

```shell
rebar3 lfe run
```

You will see code getting downloaded and compiled, and then your script will run, generating the following output:

```text
Running script '/usr/local/bin/rebar3' with args [] ...
'hello-world
```

When you created a new LFE project of type 'main', a Hello-World function was automatically generated for you, one that's even simpler than what we created in the previous section:

```lisp
(defun my-fun ()
  'hello-world)
```

The other code that was created when we executed `rebar3 new lfe-main hello-world` was a script meant to be used by LFE with LFE acting as a shell interpreter:

```lisp
#!/usr/bin/env lfescript

(defun main (args)
  (let ((script-name (escript:script_name)))
    (io:format "Running script '~s' with args ~p ...~n" `(,script-name ,args))
    (io:format "~p~n" `(,(hello-world:my-fun)))))
```

You may be wondering about the `args` argument to the `main` function, and the fact that the printed output for the `args` when we ran this was `[]`. Let's try something:

```shell
rebar3 lfe run -- Fenchurch 42
```

```text
Running script '/usr/local/bin/rebar3' with args [<<"Fenchurch">>,<<"42">>] ...
'hello-world'
```

We have to provide the two dashes to let `rebar3` know that we're done with it, that the follow argsuments are not for it, but rather for the program we want it to start for us. So it passes everything after the `--` to our script.

As for the code itself, it's tiny. But there is a lot going on just with these two files. HAve no fear, though: the remainder of this book will explore all of that and more. For now, know that the main function in the executable is calling the `hello-world` module's `my-fun` function, which takes no arguments. To put another way, what we really have here is a tiny, trivial library project with the addition of a script that calls a function from that library.

For know just know that an executable file which starts with `#!/usr/bin/env lfescript` and contains a `main` function accepting one argument is an LFE script capable of being executed from the command line -- a we have shown!
