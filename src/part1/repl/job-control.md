# Job Control

When in the LFE REPL, a special mode is accessible upon typing `<CTRL-G>`:

```
lfe>
User switch command
 -->
```

At the JCL `-->` prompt, you may get help text by typing `?`:

```text
 --> ?
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q                 - quit erlang
  ? | h             - this message
 -->
```

## Running Multiple Shells

[ also: custom prompts ]
