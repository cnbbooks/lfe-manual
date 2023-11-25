# Creating OTP Applications

In the wider Erlang community, it is very common to see applications that
run one or more `gen_server`s (or other server behaviours) managed by a
supervision tree (with the appropriate restart strategy defined). The LFE
rebar3 plugin provides the ability to generate OTP `gen_server` applications
with the server managed in a supervision tree.

To create an LFE/OTP application:

```shell
rebar3 new lfe-app otp-lfe
```

```text
===> Writing otp-lfe/README.md
===> Writing otp-lfe/LICENSE
===> Writing otp-lfe/rebar.config
===> Writing otp-lfe/.gitignore
===> Writing otp-lfe/src/otp-lfe.lfe
===> Writing otp-lfe/src/otp-lfe-app.lfe
===> Writing otp-lfe/src/otp-lfe-sup.lfe
===> Writing otp-lfe/src/otp-lfe.app.src
```

We can use the plugin's REPL command to demonstrate usage.

Start the REPL:

```shell
cd otp-lfe
rebar3 lfe repl
```

Start the app:

```lisp
lfe> (application:ensure_all_started 'otp-lfe)
#(ok (otp-lfe))
```

Make sure the supervisor is running:

```lisp
lfe> (erlang:whereis 'otp-lfe-sup)
#Pid<0.205.0>
```

Make an API call to the `gen_server`:

```lisp
lfe> (otp-lfe:echo "testing the supervised gen_server ...")
"testing the supervised gen_server ..."
```
