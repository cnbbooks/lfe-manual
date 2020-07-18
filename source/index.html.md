---
title: rebar3_lfe Documentation

language_tabs: # must be one of https://git.io/vQNgJ
  - shell

toc_footers:
  - <a href='https://github.com/lfe-rebar3/rebar3_lfe'>rebar3_lfe on Github</a>
  - <a href='https://github.com/slatedocs/slate'>Documentation Powered by Slate</a>

search: true

code_clipboard: true
---

# `rebar3_lfe` v0.3.0-dev

This is the documentation for `rebar3_lfe` v0.3.0-dev, released 2020-xx-xx.

Previos versions of the documentation:

* [v0.2.0](/v0.2.0)
* [v0.1.0](/v0.1.0)

# Introduction

The `rebar3_lfe` project is a rebar3 plugin for the [LFE language][lfe-wiki].
It provides many of the conveniences one has come to expext of a programming
language's build tool:

* Project Creation
* A REPL
* Compilation
* Maintenane Tasks (e.g., file cleanup)
* Metadata

# Background

This plugin originally started life as a shell script (`lfetool` -- there's
even a T-shirt for it!), then it toyed with integrating with `rebar` (the
original). Around that time, though, `rebar3` was under initial development,
and LFE took a chance on it as an early adopter. This lead to a whole series of
LFE plugins, but after a few years momentum was lost. 

Those early `rebar3` efforts have been combined into a single plugin in this
project, with many updates and using all the latest approaches developed in
`rebar3`'s now mature ecosystem.

# Features

* Create new LFE projects:
  * `rebar3 new lfe-lib`
  * `rebar3 new lfe-main`
  * `rebar3 new lfe-escript`
  * `rebar3 new lfe-app`
  * `rebar3 new lfe-release`
* Start up an LFE REPL:
  * `rebar3 lfe repl`
* Compile LFE source code:
  * `rebar3 lfe compile`
* Run an LFE project's `main/1` function as an lfescript (run `rebar3 new lfe-main` to see an example):
  * `rebar3 lfe run`
  * `rebar3 lfe run -- 1 2 5`
  * `rebar3 lfe run -main some/path/main.lfe`
* Escriptize an LFE escript project:
  * `rebar3 ecsriptize`
* Run an escriptized LFE escript project:
  * `rebar3 lfe run-ecsript`
* Generate an LFE/OTP release
  * `rebar3 release`
* Run an LFE/OTP release project's release script (`COMMAND` can be `start` , `stop` , `status`, `ping`, etc.):
  * `rebar3 lfe run-release COMMAND`
* Cleanup
  * `rebar3 lfe clean`
  * `rebar3 lfe clean-build`
  * `rebar3 lfe clean-cache`
  * `rebar3 lfe clean-all`
* Metadata
  * `rebar3 lfe versions`

[More coming soon!](https://github.com/lfe-rebar3/rebar3_lfe/issues?q=is%3Aissue+is%3Aopen+label%3Afeature)

# Setup

After installing `rebar3`, the only thing you need to do in order to take full
advantage of the LFE rebar3 plugin is add it to the plugins in your global
`rebar.config` file.

> To use the latest stable release, update your `rebar.config` to:

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar2_lfe.git", {tag, "0.2.0"}}}
]}.
```

> If you want to use the current development branch (unstable):

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar2_lfe.git", {tag, "0.3.0-dev"}}}
]}.
```

# Command Reference

## Compiling

The single most imporant convenience provided by the LFE rebar3 plugin is
arguably the compiler. This allows any LFE project to be downloaded,
compile, and used by any BEAM language that is also using rebar3 to manage
its dependencies, etc.

> To compile an LFE project:

```shell
rebar3 lfe compile
```

If you are publishing your LFE code, or using it in another project, you'll
want to update your `rebar.config` file so that it is compile when a user
(or script) executes the regular `rebar3 compile` command.

> To ensure your LFE code will compile in other projects, add the following to your project's `rebar.config`:

```erlang
{provider_hooks, [
  {pre, [{compile, {lfe, compile}}]}
  ]}.
```

## Running the REPL

An LFE project and all of its dependencies may be interacted with via a REPL
that is started with the LFE rebar3 plguin, as rebar3 sets all of the correct
library locations for use by shells and REPLs.

> To start up a REPL:

```shell
rebar3 lfe repl
```

## Creating Libraries

The `rebar3_lfe` plugin is capable of creating several common project layouts.
This and following sections provide details on those that are currently
supported.

Library projects are those with no running applications or scripts; they simply
provide some core bit of functionality intended for use by applications,
scripts, or other libraries.

> To create a library project with the default name:

```shell
rebar3 new lfe-lib
```

> This will generate the following output:

```text
===> Writing my-lfe-lib/README.md
===> Writing my-lfe-lib/LICENSE
===> Writing my-lfe-lib/rebar.config
===> Writing my-lfe-lib/.gitignore
===> Writing my-lfe-lib/src/my-lfe-lib.lfe
===> Writing my-lfe-lib/src/my-lfe-lib.app.src
```

> You can also explicitely name your project:

```shell
rebar3 new lfe-lib forty-two
```

> Which will produce the following:

```text
===> Writing forty-two/README.md
===> Writing forty-two/LICENSE
===> Writing forty-two/rebar.config
===> Writing forty-two/.gitignore
===> Writing forty-two/src/forty-two.lfe
===> Writing forty-two/src/forty-two.app.src
```

As mentioned abouve, the REPL offers a nice way to quickly interact your new
project.

> Start the REPL:

```shell
cd forty-two
rebar3 lfe repl
```

> Call the generated/sample LFE function:

```lisp
lfe> (mything:my-fun)
hello-world
```

## Creating `main` Scripts

LFE supports the creation of scripts, and these can be integrated with
libraries, allowing for the best of both worlds: a simple collection of
useful functions (library) and a means of running them from the command
line.

> Create a library that also has a script with a `main` function:

```shell
$ rebar3 new lfe-main special-proj
```

> Which generates:

```text
===> Writing special-proj/README.md
===> Writing special-proj/LICENSE
===> Writing special-proj/rebar.config
===> Writing special-proj/.gitignore
===> Writing special-proj/src/special-proj.lfe
===> Writing special-proj/scripts/main.lfe
===> Writing special-proj/src/special-proj.app.src
```

> The generated project's `main` script + function may then be run with:

```shell
cd mymain
rebar3 lfe run -- 42
```

> Which will produce the following output:

```text
Running script '/usr/local/bin/rebar3' with args [<<"42">>] ...
'hello-worl
```

## Creating escripts

The LFE rebar3 plugin also supports generating escript-based projects
*in LFE*. This is similar in nature to the `main`-based project, but is more
standard in the BEAM family of languages.

> To create an escript-based project:

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

> Compile the LFE and then bundle all the code up by "escriptizing" it:

```shell
cd myapp
rebar3 lfe compile
rebar3 escriptize
```

> Run the newly-created escript:

```shell
rebar3 lfe run-escript 1 2 5 no '3!'
```

> Which will display the following:

```lisp
Got args: ("1" "2" "5" "no" "3!")
Answer: 42
```

## Creating OTP Applications

In the wider Erlang community, it is very common to see applications that
run one or more `gen_server`s (or other server behaviours) managed by a
supervision tree (with the appropriate restart strategy defined). The LFE
rebar3 plugin provides the ability to generate OTP `gen_server` applications
with the server managed in a supervision tree.

> To create an LFE/OTP application:

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

> Start the REPL:

```shell
cd otp-lfe
rebar3 lfe repl
```

> Start the app, make sure the supervisor is running, and make an API call to the `gen_server`:

```lisp
lfe> (application:ensure_all_started 'myapp)
#(ok (myapp))
lfe> (erlang:whereis 'myapp-sup)
#Pid<0.205.0>
lfe> (myapp:echo "testing the supervised gen_server ...")
"testing the supervised gen_server ..."
```

### Creating OTP Releases

If you're going to be running an LFE application in production, you will very
likely want to do so using the "release" functionality provided by OTP.

> Create a release-based project with:

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

> Start up the application:

```shell
rebar3 lfe run-release start
```

> Check the status of the application:

```shell
rebar3 lfe run-release ping
```

```text
pong
```

If your `ping` doesn't get a `pong` after starting the release, this is a known
issue that is being investigated in the following ticket:

* [https://github.com/lfe-rebar3/rebar3_lfe/issues/33](https://github.com/lfe-rebar3/rebar3_lfe/issues/33)

> The current workaround for a relese that doesn't start is simply to run the following again:

```shell
rebar3 release
rebar3 lfe run-release start
rebar3 lfe run-release ping
```

In addition to using the LFE rebar3 commands to start the application,
you can start up a release console and then switch to the LFE REPL.

> Start the console:

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

## Cleanup Commands

There are a handful of tasks the author of the plugin as found useful when
debugging LFE applications, rebar3 plugins, etc. These include various
"cleanup" tasks that are not currently supported by `rebar3` (or whose support
is problematic).

> Remove the apps' `ebin/*` files:

```shell
rebar3 lfe clean
```

> Remove the `_build` directory:

```shell
rebar3 lfe clean-build
```

> Remove the the cached directories for the app and the rebar3_lfe plugin, both global and local:

```shell
rebar3 lfe clean-cache
```

> Perform all clean tasks as well as remove the files `erl_crash.dump`, `rebar3.crashdump`, and `rebar.lock`:

```shell
rebar3 lfe clean-all
```

## Versions

It can be very useful to obtain all the important version info for a project:
your project, LFE, the LFE rebar3 plugin, and the Erlang release
installed/active on your machine. When submiting a bug or asking for help on
the LFE Slack, you might very well be asked for this information.

> Get versions:

```shell
rebar3 lfe versions
```

```lisp
(#(apps
   (#(prod-lfe git)))
 #(languages
   (#(lfe "1.3-dev")
    #(erlang "23")
    #(emulator "11.0")
    #(driver_version "3.3")))
 #(tooling
   (#(rebar "3.10.0")
    #(rebar3_lfe "0.2.0"))))
```

<!-- Named page links below: /-->

[lfe-wiki]: https://en.wikipedia.org/wiki/LFE_(programming_language)
[run-release-issue]: https://github.com/lfe-rebar3/rebar3_lfe/issues/33
