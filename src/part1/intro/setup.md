# Development Setup

## `rebar3` Configuration
Having followed the notes and linked instructions in the [Prerequisites](prereq.html) section, you are ready to add global support for the LFE `rebar3` plugin.

First, unless you have configured other `rebar3` plugins on your system, you will need to create the configuration directory and the configuration file:

```bash
$ mkdir ~/.config/rebar3
$ touch ~/.config/rebar3/rebar.config
```

Next, open up that file in your favourite editor, and give it these contents:

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar3_lfe.git", {branch, "master"}}}
]}.
```

If you already have a `rebar.config` file with a plugins entry, then simply add a comma after the last plugin listed and paste the `{rebar3_lfe, {...}}` line from above (with no trailing comma!).

<div class="alert alert-info">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    For Windows users
  </h4>
  <p class="mb-0">Some notes on compatibility:</p>
  <p class="mb-0">
    While LFE, Erlang, and <code>rebar3</code> work on *NIX, BSD, and Windows systems, much of the development the community does occurs predominently on the first two and sometimes Windows support is spotty and less smooth than on the more used platforms (this is more true for <code>rebar3</code> and LFE, and _most_ true for LFE).

In particular, starting a REPL in Windows can take a little more effort (an extra step or two) than it does on, for example, Linux and Mac OS X machines.
  </p>
</div>

## A Quick Test with the REPL

With the LFE `rebar3` plugin successfully configured, you should be able to start up the LFE REPL anywhere on your system with the following:

```shell
$ rebar3 lfe repl
```
```text
Erlang/OTP 23 [erts-11.0] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:1] [hipe]

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

Exit out of the REPL for now by typing `<CTRL><G>` and  thn `<Q>`.

<div class="alert alert-warning">
  <h4 class="alert-heading">
    <i class="fa fa-exclamation-triangle" aria-hidden="true"></i>
    For Windows users
  </h4>
  <p class="mb-0">
    On Windows, this currently puts you into the Erlang shell, not the LFE REPL. To continue to the LFE REPL, you will need to enter <code>lfe_shell:server().</code> and then press <code>&lt;ENTER&gt;</code>.
  </p>
</div>
