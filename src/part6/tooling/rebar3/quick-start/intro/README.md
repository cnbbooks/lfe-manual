# Introduction

This is a version of the LFE quick start that has been re-imagined with
the LFE rebar3 plugin in mind.

We will cover the following:

* How to get started quickly with LFE using just `rebar3` and your local
  installation of Erlang
* Creating a new LFE project
* Looking at LFE code in the REPL and in modules
* Provide a little insight on how this works
* Leave you with resources for jumping into LFE in more detail

## About `rebar3`

Rebar3 is an Erlang tool that makes it easy to create, develop, and release Erlang libraries, applications, and systems in a repeatable manner.

Rebar3 will:
- respect and enforce standard Erlang/OTP conventions for project
  structure so they are easily reusable by the community;
- manage source dependencies and Erlang [packages](https://hex.pm)
  while ensuring repeatable builds;
- handle build artefacts, paths, and libraries such that standard
  development tools can be used without a headache;
- adapt to projects of all sizes on almost any platform;
- treat [documentation](https://www.rebar3.org/docs/) as a feature,
  and errors or lack of documentation as a bug.

Rebar3 is also a self-contained Erlang script. It is easy to distribute or
embed directly in a project. Tasks or behaviours can be modified or expanded
with a [plugin system](https://www.rebar3.org/docs/configuration/plugins/#recommended-plugins)
[flexible enough](https://www.rebar3.org/docs/configuration/plugins) that even other languages
on the Erlang VM will use it as a build tool.

The [rebar3 site](https://www.rebar3.org/docs/getting-started#installing-binary) provides some nice instructions on installing `rebar3`.
