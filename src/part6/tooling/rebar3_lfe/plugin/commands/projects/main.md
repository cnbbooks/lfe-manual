# Creating `main` Scripts

LFE supports the creation of scripts, and these can be integrated with
libraries, allowing for the best of both worlds: a simple collection of
useful functions (library) and a means of running them from the command
line.

Create a library that also has a script with a `main` function:

```shell
rebar3 new lfe-main special-proj
```

Which generates:

```text
===> Writing special-proj/README.md
===> Writing special-proj/LICENSE
===> Writing special-proj/rebar.config
===> Writing special-proj/.gitignore
===> Writing special-proj/src/special-proj.lfe
===> Writing special-proj/scripts/main.lfe
===> Writing special-proj/src/special-proj.app.src
```

The generated project's `main` script + function may then be run with:

```shell
cd mymain
rebar3 lfe run -- 42
```

Which will produce the following output:

```text
Running script '/usr/local/bin/rebar3' with args [<<"42">>] ...
'hello-worl
```
