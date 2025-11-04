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

<div class="alert alert-warning">
  <h4 class="alert-heading">
    <i class="fa fa-exclamation-triangle" aria-hidden="true"></i>
    Known Issue!
  </h4>
  <p class="mb-0">
    If your release doesn't start
    (e.g., running <code>rebar3 lfe run-release ping</code> doesn't return
    <code>pong</code>),
    this is a known issue that is being investigated in the following ticket:
  </p>
</div>

[https://github.com/lfe-rebar3/rebar3_lfe/issues/33](https://github.com/lfe-rebar3/rebar3_lfe/issues/33)

<div class="alert alert-info">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    Workaround
  </h4>
  <p class="mb-0">
    The current workaround for a relese that doesn't start is simply to re-run
    <code>rebar3 release</code> and <code>rebar3 lfe run-release start</code>.
  </p>
</div>
