# run-release

To run an LFE project that was generated with `rebar3 new lfe-release`
and has been compiled with `rebar3 release`:

```shell
rebar3 lfe run-release COMMAND
```

Where `COMMAND` is one of the non-interactive commands supported by the
release script:

* `start`
* `stop`
* `restart`
* `reboot`
* `pid`
* `ping`

Others non-interactive commands not listed may also work, but they have not
been tested.
