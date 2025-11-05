# State Management

Rebar3 uses two primary data structures to manage build state: `rebar_state:t()` holds global build configuration and state, while `rebar_app_info:t()` holds per-application information. Both are immutable records passed through the entire compilation chain.
