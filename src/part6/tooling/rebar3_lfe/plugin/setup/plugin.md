# Using the Plugin

After installing `rebar3`, the only thing you need to do in order to take full
advantage of the LFE rebar3 plugin is add it to the plugins in your global
`rebar.config` file.

<div class="alert alert-success">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    Stable
  </h4>
  <p class="mb-0">
    To use the latest stable release, update your `rebar.config` to:
  </p>
</div>

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar3_lfe.git", {tag, "0.2.0"}}}
]}.
```

<div class="alert alert-warning">
  <h4 class="alert-heading">
    <i class="fa fa-exclamation-triangle" aria-hidden="true"></i>
Unstable
  </h4>
  <p class="mb-0">
    If you want to use the current development branch (unstable):
  </p>
</div>

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar3_lfe.git", {branch, "release/0.3.x"}}}
]}.
```
