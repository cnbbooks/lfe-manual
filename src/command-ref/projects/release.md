# Creating OTP Releases

If you're going to be running an LFE application in production, you will very
likely want to do so using the "release" functionality provided by OTP.

Create a release-based project with:

```shell
rebar3 new lfe-release prod-lfe
```

```text
===> Writing prod-lfe/README.md
===> Writing prod-lfe/LICENSE
===> Writing prod-lfe/rebar.config
===> Writing prod-lfe/.gitignore
===> Writing prod-lfe/apps/prod-lfe/src/prod-lfe.lfe
===> Writing prod-lfe/apps/prod-lfe/src/prod-lfe-app.lfe
===> Writing prod-lfe/apps/prod-lfe/src/prod-lfe-sup.lfe
===> Writing prod-lfe/apps/prod-lfe/src/prod-lfe.app.src
===> Writing prod-lfe/config/sys.config
===> Writing prod-lfe/config/vm.args
```

Change directoy into your new app: 

```shell
cd prod-lfe
```

Build the release:

```shell
rebar3 release
```

Start up the application:

```shell
rebar3 lfe run-release start
```

Check the status of the application:

```shell
rebar3 lfe run-release ping
```

```text
pong
```

<div class="alert alert-warning">
  <h4 class="alert-heading">
    <i class="fa fa-exclamation-triangle" aria-hidden="true"></i>
    Known Issue!
  </h4>
  <p class="mb-0">
    If your <code>ping</code> doesn't get a <code>pong</code> after starting the
    release, this is a known issue that is being investigated in the following ticket:
  </p>
</div>

[https://github.com/lfe-rebar3/rebar3_lfe/issues/33](https://github.com/lfe-rebar3/rebar3_lfe/issues/33)

<div class="alert alert-info">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    Workaround
  </h4>
  <p class="mb-0">
    The current workaround for a relese that doesn't start is simply to run the following again:
  </p>
</div>


```shell
rebar3 release
rebar3 lfe run-release start
rebar3 lfe run-release ping
```

In addition to using the LFE rebar3 commands to start the application,
you can start up a release console and then switch to the LFE REPL.

Start the console:

```shell
./_build/default/rel/prod-lfe/bin/prod-lfe console
```

```erlang
Eshell V11.0  (abort with ^G)
(prod-app@spacemac)1> lfe_shell:start().
```

```lisp
(prod-app@spacemac)lfe> (erlang:whereis 'prod-lfe-sup)
#Pid<0.351.0>
(prod-app@spacemac)lfe> (prod-lfe:echo "testing from production!")
"testing from production!"
```
