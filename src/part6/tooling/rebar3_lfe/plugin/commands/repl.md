# Running the REPL

An LFE project and all of its dependencies may be interacted with via a REPL
that is started with the LFE rebar3 plguin, as rebar3 sets all of the correct
library locations for use by shells and REPLs.

To start up a REPL:

```shell
rebar3 lfe repl
```

At which point you will be greeted with the familiar:

``` text
Erlang/OTP 25 [erts-13.2.2.2] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit]

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/lfe/lfe
   \     l    |_/    |
    \   r     /      |   LFE v2.1.2 (abort with ^C; enter JCL with ^G)
     `-E___.-'

lfe>
```

<div class="alert alert-warning">
  <h4 class="alert-heading">
    <i class="fa fa-exclamation-triangle" aria-hidden="true"></i>
    Known Issue: Erlang 26+!
  </h4>
  <p class="mb-0">
    Erlang 26.0 completely refactored its support for shells. While a fix was released for LFE-proper's REPL, that same fix does not work for the rebar3 LFE REPL. To follow the progress, you can subscribe to this ticket: <a href="https://github.com/lfe/rebar3/issues/79">https://github.com/lfe/rebar3/issues/79</a>.
  </p>
</div>

The LFE banner is highly configurable in in rebar3_lfe and accepts the following configuration in your project's `rebar.config` file:

``` erlang
{lfe, [
   {repl, [
     {nobanner, false},
     {version, "9999.2"},
     {quit_message, "You can never LEEEEEAVE!"},
     {banner_template, "Weeee!~nLFE v~s ~s~n"}
   ]}
]}.
```

A project configuration with those values would have a banner like the following:

``` text
Erlang/OTP 25 [erts-13.2.2.2] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit]

Weeee!
LFE v9999.2 You can never LEEEEEAVE!
lfe>
```

To disable the banner, use a configuration like this:

``` text
{lfe, [
   {repl, [
     {nobanner, true}
   ]}
]}.
```

Which would give:

``` text
Erlang/OTP 25 [erts-13.2.2.2] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit]

lfe>
```
