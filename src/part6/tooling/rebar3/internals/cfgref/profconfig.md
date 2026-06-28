# Profile Configuration

## profiles

**Purpose**: Environment-specific configuration

**Type**: `[{ProfileName, Config}]`

**Built-in Profiles**: `default`, `test`, `prod`

**Example**:

```erlang
{profiles, [
    {test, [
        {deps, [meck, proper]},
        {erl_opts, [debug_info, nowarn_export_all]}
    ]},

    {prod, [
        {erl_opts, [no_debug_info, inline_list_funcs]},
        {relx, [{dev_mode, false}, {include_erts, true}]}
    ]},

    {custom, [
        {deps, [recon]},
        {erl_opts, [{d, 'CUSTOM_BUILD'}]}
    ]}
]}.
```

**Usage**:

```bash
rebar3 as test compile
rebar3 as prod release
rebar3 as test,custom ct
```

**Merging**: Profile config merges with default, later profiles override earlier
