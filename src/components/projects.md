# Projects

Use the established best practices for Erlang project creation, adapted for LFE. These have been used when defining the LFE new project templates in the [rebar3 plugin](https://lfe.io/reference/lfe-rebar3/current/). That is probably the best way to get consistent results when creating the most common types of LFE projects (e.g., `main`-scripts, escripts, libraries, OTP applications, and OTP releases).

While Erlang and LFE do not support packages, it is possible to use the `rebar3` LFE plugin to simulate packages, complete with project directory structures that consolidate specific functionality in collections of sub-directories. These will be detected at compile-time when you use `rebar3 lfe compile` and from these, proper Erlang-compatible modules will be created (with dotted names preserving the hierarchy).

For instance, a game project hierarchy might look like the following:

```shell
 ├── LICENSE
 ├── README.md
 ├── rebar.config
 ├── src
 │  ├── title.app.src
 │  └── title
 │     ├── config.lfe
 │     ├── db.lfe
 │     ├── graphics
 │     │  ├── mesh.lfe
 │     │  ├── obj.lfe
 │     │  └── gl.lfe
 │     └── logging.lfe
 ├── test
 ...
```

Which represents a more abstract hierarchy like this:

```lisp
title
  title.graphics
    title.graphics.mesh
    title.graphics.obj
    title.graphics.gl
  title.config
  title.logging
  title.db
```

Will actually have `.beam` files created in the `ebin` directory with the following names:

```shell
title.app
title.config.beam
title.db.beam
title.graphics.mesh.beam
title.graphics.obj.beam
title.graphics.gl.beam
title.logging.beam
```
