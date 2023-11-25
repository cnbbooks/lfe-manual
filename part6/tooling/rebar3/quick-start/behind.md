# Behind the Scenes

<img src="https://raw.githubusercontent.com/lfe/docs/master/docs/current/images/doubles.jpg"
     style="float: right; padding-left: 1em;">

As we warned in the beginning, there's a lot going on behind the scenes: in `rebar3` as well as LFE and Erlang (we haven't even _touched_ OTP here ...!). This guide just gives you a quick taste of the LFE flavour :-) Before parting ways, though, there are some more bits we should share.

Some of those things are hinted at when just checking the current versions you are
running using the LFE plugin's `versions` command (from a terminal where you have
`cd`ed into your project directory):

```shell
rebar3 lfe versions
```

```lisp
(#(apps (#(my-test-lib git)))
 #(languages
   (#(lfe "1.3-dev")
    #(erlang "23")
    #(emulator "11.0.2")
    #(driver_version "3.3")))
 #(tooling (#(rebar "3.10.0") #(rebar3_lfe "0.2.0"))))
```

To give a sense of what you'll encounter in the future: very often Erlang,
LFE, and other BEAM language apps include more than just themselves when they
are shipped. For instance, if you're in the REPL and you type `(regs)` you will
see a list of applications that have been registered by name, currently running
in support of the REPL. Usually, each app will have its own version. There is an
[LFE blog series](http://blog.lfe.io/tutorials/2015/05/23/1720-new-series-lfe-otp-tutorials/)
on such things, showing you how to create and mess around with different types
of LFE apps.

The LFE rebar3 plugin will also help you create OTP apps in LFE and perform
other key tasks you may wish to integrate into your development workflow.
You can learn more about those in the plugin's
[Command Reference](https://lfe-rebar3.github.io/)

## Next Stop

Where to go from here ...
