# Starting LFE

## The `lfe` executable

While this book focuses upon the use of `rebar3` and its LFE plugin -- due entirely to the amount of time it saves through various features it supports -- LFE may be used quite easily without it.

To use LFE and its REPL without `rebar3`, you'll need to clone the repo, e.g.:

```bash
cd ~/lab
git clone https://github.com/lfe/lfe.git
cd lfe
```

Since you have read the earlier section on dependencies, you already have Erlang, `make`, and your system build tools installed. As such, all you have to do is run the following to build LFE:

```bash
make
```

This will generate an executable in `./bin` and you can start the LFE REPL by calling it:

```bash
./bin/lfe
```

```text
Erlang/OTP 28 [erts-16.0] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit] [dtrace]

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/lfe/lfe
   \     l    |_/    |
    \   r     /      |   LFE v2.2.0 (abort with ^G)
     `-E___.-'

lfe>
```

If you opt to install LFE system-wide with `make install`, then you can start the REPL from anywhere by simply executing `lfe`.

## Via `rebar3 lfe repl`

As demonstrated earlier on several occasions, you can start the LFE REPL with the `rebar3` LFE plugin (and this is what we'll do in the rest of this manual):

```bash
rebar3 lfe repl
```

Since you have updated your global rebar3 settings (in the "Prerequisites" section, after following the instructions on the `rebar3` site), you may also start the LFE REPL from anywhere on your machine using the `rebar3` command.
