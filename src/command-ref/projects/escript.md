# Creating escripts

The LFE rebar3 plugin also supports generating escript-based projects
*in LFE*. This is similar in nature to the `main`-based project, but is more
standard in the BEAM family of languages.

To create an escript-based project:

```shell
rebar3 new lfe-escript myapp
```

```text
===> Writing myapp/README.md
===> Writing myapp/LICENSE
===> Writing myapp/rebar.config
===> Writing myapp/.gitignore
===> Writing myapp/src/myapp.lfe
===> Writing myapp/src/myapp.app.src
```

Compile the LFE and then bundle all the code up by "escriptizing" it:

```shell
cd myapp
rebar3 lfe compile
rebar3 escriptize
```

Run the newly-created escript:

```shell
rebar3 lfe run-escript 1 2 5 no '3!'
```

Which will display the following:

```lisp
Got args: ("1" "2" "5" "no" "3!")
Answer: 42
```
