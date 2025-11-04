# Versions

It can be very useful to obtain all the important version info for a project:
your project, LFE, the LFE rebar3 plugin, and the Erlang release
installed/active on your machine. When submiting a bug or asking for help on
the LFE Slack, you might very well be asked for this information.

Get a project's versions:

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
