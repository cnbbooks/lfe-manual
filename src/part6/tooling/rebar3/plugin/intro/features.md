# Features

* Create new LFE projects:
  * `rebar3 new lfe-lib`
  * `rebar3 new lfe-main`
  * `rebar3 new lfe-escript`
  * `rebar3 new lfe-app`
  * `rebar3 new lfe-release`
* Start up an LFE REPL:
  * `rebar3 lfe repl`
* Compile LFE source code:
  * `rebar3 lfe compile`
* Run eunit tests
  * `rebar3 eunit`
* Run an LFE project's `main/1` function as an lfescript (run `rebar3 new lfe-main` to see an example):
  * `rebar3 lfe run`
  * `rebar3 lfe run -- 1 2 5`
  * `rebar3 lfe run -main some/path/main.lfe`
* Escriptize an LFE escript project:
  * `rebar3 ecsriptize`
* Run an escriptized LFE escript project:
  * `rebar3 lfe run-ecsript`
* Generate an LFE/OTP release
  * `rebar3 release`
* Run an LFE/OTP release project's release script (`COMMAND` can be `start` , `stop` , `status`, `ping`, etc.):
  * `rebar3 lfe run-release COMMAND`
* Cleanup
  * `rebar3 lfe clean`
  * `rebar3 lfe clean-build`
  * `rebar3 lfe clean-cache`
  * `rebar3 lfe clean-all`
* Metadata
  * `rebar3 lfe versions`

[More coming soon!](https://github.com/lfe-rebar3/rebar3_lfe/issues?q=is%3Aissue+is%3Aopen+label%3Afeature)
