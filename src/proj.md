# Creating a New Project

<img src="https://raw.githubusercontent.com/lfe/docs/master/docs/current/images/barf.jpg"
     style="float: right; padding-left: 1em;">A project? Already?! It sounds
daunting, but it's easier than you might think. Open up a terminal window
and do this in a directory of your choosing:

```bash
rebar3 new lfe-lib my-test-lib
```

It might take a minute or two to finish; here's what's happening:

* `rebar3` downloads the LFE plugin
* Finds its dependencies, downloads those too
* Compiles all of them (plugins and dependencies)
* `rebar3` then executes the `new` command, searches for (and
  finds!) the lfe-lib template
* Creates the project files for the given template

As that last step executes, you will see the following output:

```text
===> Writing my-test-lib/README.md
===> Writing my-test-lib/LICENSE
===> Writing my-test-lib/rebar.config
===> Writing my-test-lib/.gitignore
===> Writing my-test-lib/src/my-test-lib.lfe
===> Writing my-test-lib/src/my-test-lib.app.src
```

It's as simple as that! Your new project is ready to go :-)


## Next Stop

You can taste it, can't you? That *LFE flavour* coming your way? Yup, you're
right.  You're going to be looking at LFE code next ...
